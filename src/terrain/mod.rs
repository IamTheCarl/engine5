use bevy::{
    asset::{AssetPath, LoadState},
    pbr::{StandardMaterialFlags, StandardMaterialUniform},
    prelude::*,
    reflect::TypeUuid,
    render::{
        mesh::{Indices, MeshVertexAttribute, VertexAttributeValues},
        render_asset::RenderAssets,
        render_resource::{
            AsBindGroup, AsBindGroupShaderType, Extent3d, PrimitiveTopology, ShaderDefVal,
            ShaderRef, TextureDimension, TextureFormat, VertexFormat,
        },
        texture::Volume,
    },
};
use std::{borrow::Cow, collections::HashMap, num::NonZeroU16, ops::Range, str::FromStr};
use thiserror::Error;

pub mod terrain_file;
pub use terrain_file::TerrainFile;

pub mod terrain_generation;
pub use terrain_generation::*;

pub mod terrain_space;
pub use terrain_space::{
    LoadAllTerrain, LoadTerrain, LoadsTerrain, TerrainSpace, TerrainSpaceBundle,
};

type BlockID = NonZeroU16;
pub type GlobalBlockCoordinate = IVec3;
pub type LocalBlockCoordinate = IVec3; // TODO consider making this an unsigned type.

#[derive(Clone, Copy, Debug)]
pub enum BlockDirection {
    Up,
    Down,
    North,
    South,
    East,
    West,
}

impl BlockDirection {
    pub const ALL: [BlockDirection; 6] = [
        BlockDirection::Up,
        BlockDirection::Down,
        BlockDirection::North,
        BlockDirection::South,
        BlockDirection::East,
        BlockDirection::West,
    ];

    pub fn inverse(&self) -> Self {
        match self {
            Self::Up => Self::Down,
            Self::Down => Self::Up,
            Self::North => Self::South,
            Self::South => Self::North,
            Self::East => Self::West,
            Self::West => Self::East,
        }
    }
}

impl From<BlockDirection> for Vec3 {
    fn from(direction: BlockDirection) -> Self {
        match direction {
            BlockDirection::Up => Vec3::Y,
            BlockDirection::Down => Vec3::NEG_Y,
            BlockDirection::North => Vec3::Z,
            BlockDirection::South => Vec3::NEG_Z,
            BlockDirection::East => Vec3::X,
            BlockDirection::West => Vec3::NEG_X,
        }
    }
}

impl From<BlockDirection> for IVec3 {
    fn from(direction: BlockDirection) -> Self {
        match direction {
            BlockDirection::Up => IVec3::Y,
            BlockDirection::Down => IVec3::NEG_Y,
            BlockDirection::North => IVec3::Z,
            BlockDirection::South => IVec3::NEG_Z,
            BlockDirection::East => IVec3::X,
            BlockDirection::West => IVec3::NEG_X,
        }
    }
}

// Indexes used for shaders.
impl From<BlockDirection> for u8 {
    fn from(direction: BlockDirection) -> Self {
        match direction {
            BlockDirection::Up => 0,
            BlockDirection::Down => 1,
            BlockDirection::North => 2,
            BlockDirection::South => 3,
            BlockDirection::East => 4,
            BlockDirection::West => 5,
        }
    }
}

struct BlockFaces {
    top: u16,
    bottom: u16,
    north: u16,
    south: u16,
    east: u16,
    west: u16,
}

impl BlockFaces {
    fn get_face_texture_index(&self, direction: BlockDirection) -> u16 {
        match direction {
            BlockDirection::Up => self.top,
            BlockDirection::Down => self.bottom,
            BlockDirection::North => self.north,
            BlockDirection::South => self.south,
            BlockDirection::East => self.east,
            BlockDirection::West => self.west,
        }
    }
}

struct BlockNeighborSet {
    up: Option<Block>,
    down: Option<Block>,
    north: Option<Block>,
    south: Option<Block>,
    east: Option<Block>,
    west: Option<Block>,
}

impl BlockNeighborSet {
    fn get(&self, direction: BlockDirection) -> Option<Block> {
        match direction {
            BlockDirection::Up => self.up,
            BlockDirection::Down => self.down,
            BlockDirection::North => self.north,
            BlockDirection::South => self.south,
            BlockDirection::East => self.east,
            BlockDirection::West => self.west,
        }
    }
}

pub struct BlockData {
    _name: String,
    id: BlockID,
    faces: BlockFaces,
}

impl BlockData {
    pub fn spawn(&self) -> Block {
        Block { id: self.id }
    }
}

/// A type used to identify blocks in a save file independent way.
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct BlockTag<'a> {
    namespace: Cow<'a, str>,
    block_name: Cow<'a, str>,
}

#[derive(Error, Debug)]
pub enum BlockTagError<'a> {
    #[error("Failed to parse block tag: {0}")]
    ParseError(#[from] BlockTagParseError),

    #[error("A block with this tag already exists in the library.")]
    AlreadyExists,

    #[error("A block with the tag `{0}` could not be found.")]
    NotFound(BlockTag<'a>),
}

#[derive(Error, Debug, PartialEq, Eq)]
pub enum BlockTagParseError {
    #[error("Could not find separating colon.")]
    NoColon,

    #[error("Could not parse block namespace from block tag.")]
    MissingNamespace,

    #[error("Could not parse block name from block tag.")]
    MissingBlockName,

    #[error("Found too many components in block name.")]
    TooManyComponents,
}

impl<'a> BlockTag<'a> {
    pub fn to_static(&self) -> BlockTag<'static> {
        BlockTag {
            namespace: Cow::Owned(self.namespace.clone().into_owned()),
            block_name: Cow::Owned(self.block_name.clone().into_owned()),
        }
    }

    fn get_parts(s: &str) -> Result<(&str, &str), BlockTagParseError> {
        if s.contains(':') {
            let mut iter = s.split(':');
            let namespace = iter
                .next()
                .filter(|s| !s.is_empty())
                .map_or(Err(BlockTagParseError::MissingNamespace), Ok)?;

            let block_name = iter
                .next()
                .filter(|s| !s.is_empty())
                .map_or(Err(BlockTagParseError::MissingBlockName), Ok)?;

            if iter.next().is_some() {
                Err(BlockTagParseError::TooManyComponents)
            } else {
                Ok((namespace, block_name))
            }
        } else {
            Err(BlockTagParseError::NoColon)
        }
    }
}

impl<'a> std::fmt::Display for BlockTag<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.namespace, self.block_name)
    }
}

impl<'a> FromStr for BlockTag<'a> {
    type Err = BlockTagParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (namespace, block_name) = Self::get_parts(s)?;

        Ok(Self {
            namespace: Cow::Owned(namespace.to_string()),
            block_name: Cow::Owned(block_name.to_string()),
        })
    }
}

impl<'a> TryFrom<&'a str> for BlockTag<'a> {
    type Error = BlockTagParseError;

    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        let (namespace, block_name) = Self::get_parts(s)?;

        Ok(Self {
            namespace: Cow::Borrowed(namespace),
            block_name: Cow::Borrowed(block_name),
        })
    }
}

#[test]
fn block_tag() {
    let tag = BlockTag::from_str("game:block").unwrap();
    assert_eq!(tag.namespace.as_ref(), "game");
    assert_eq!(tag.block_name.as_ref(), "block");

    assert_eq!(BlockTag::from_str(""), Err(BlockTagParseError::NoColon));
    assert_eq!(BlockTag::from_str("game"), Err(BlockTagParseError::NoColon));

    assert_eq!(
        BlockTag::from_str(":"),
        Err(BlockTagParseError::MissingNamespace)
    );
    assert_eq!(
        BlockTag::from_str(":block"),
        Err(BlockTagParseError::MissingNamespace)
    );
    assert_eq!(
        BlockTag::from_str("game:"),
        Err(BlockTagParseError::MissingBlockName)
    );

    assert_eq!(
        BlockTag::from_str("game:block:"),
        Err(BlockTagParseError::TooManyComponents)
    );
    assert_eq!(
        BlockTag::from_str("game:block:entity"),
        Err(BlockTagParseError::TooManyComponents)
    );
}

#[derive(Resource)]
pub struct BlockRegistry {
    block_data: Vec<BlockData>,
    block_tags: HashMap<BlockTag<'static>, BlockID>,
}

impl BlockRegistry {
    pub fn load(terrain_texture: &TerrainTextureManager) -> Result<Self, BlockTagError<'static>> {
        let mut registry = Self {
            block_data: Vec::new(),
            block_tags: HashMap::new(),
        };

        // TODO load this data from a file.

        let stone_index = terrain_texture.get_image_index("stone");
        registry.add_block(
            "core:stone".try_into()?,
            "Stone",
            BlockFaces {
                top: stone_index,
                bottom: stone_index,
                north: stone_index,
                south: stone_index,
                east: stone_index,
                west: stone_index,
            },
        )?;

        let dirt_index = terrain_texture.get_image_index("dirt");
        registry.add_block(
            "core:dirt".try_into()?,
            "Dirt",
            BlockFaces {
                top: dirt_index,
                bottom: dirt_index,
                north: dirt_index,
                south: dirt_index,
                east: dirt_index,
                west: dirt_index,
            },
        )?;

        let default_index = terrain_texture.get_image_index("default");
        registry.add_block(
            "core:default".try_into()?,
            "Default",
            BlockFaces {
                top: default_index,
                bottom: default_index,
                north: default_index,
                south: default_index,
                east: default_index,
                west: default_index,
            },
        )?;

        Ok(registry)
    }

    fn add_block(
        &mut self,
        tag: BlockTag<'static>,
        name: impl Into<String>,
        faces: BlockFaces,
    ) -> Result<(), BlockTagError<'static>> {
        // We need to report an error if an insertion isn't done, so this will stay an error if an insertion doesn't happen.
        let mut result = Err(BlockTagError::AlreadyExists);
        self.block_tags.entry(tag).or_insert_with(|| {
            result = Ok(()); // We inserted! That means we can return Ok.

            let _name = name.into();

            let id = self.block_data.len() as u16;
            let id = BlockID::new(id + 1).expect("Block ID miscalculated.");
            self.block_data.push(BlockData { _name, id, faces });
            id
        });

        result
    }

    pub fn get_by_tag(&self, tag: &BlockTag) -> Result<&BlockData, BlockTagError> {
        let id = self
            .block_tags
            .get(tag)
            .ok_or_else(|| BlockTagError::NotFound(tag.to_static()))?;
        self.block_data
            .get(id.get() as usize - 1)
            .ok_or_else(|| BlockTagError::NotFound(tag.to_static()))
    }

    pub fn get(&self, block: &Block) -> Option<&BlockData> {
        self.block_data.get(block.id.get() as usize - 1)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Block {
    id: BlockID,
}

impl Block {
    fn insert_face(
        direction: BlockDirection,
        offset: UVec3,
        texture_index: u16,
        vertex_buffer: &mut Vec<u32>,
        indices: &mut Vec<u32>,
    ) {
        let source_vertices = &[
            // Top
            [1, 1, 0],
            [0, 1, 0],
            [0, 1, 1],
            [1, 1, 1],
            // Bottom
            [1, 0, 1],
            [0, 0, 1],
            [0, 0, 0],
            [1, 0, 0],
            // East
            [1, 0, 0],
            [1, 1, 0],
            [1, 1, 1],
            [1, 0, 1],
            // West
            [0, 0, 1],
            [0, 1, 1],
            [0, 1, 0],
            [0, 0, 0],
            // North
            [0, 0, 1],
            [1, 0, 1],
            [1, 1, 1],
            [0, 1, 1],
            // South
            [0, 1, 0],
            [1, 1, 0],
            [1, 0, 0],
            [0, 0, 0],
        ];

        let source_uv = [
            // Top
            [0, 0],
            [1, 0],
            [1, 1],
            [0, 1],
            // Bottom
            [0, 0],
            [1, 0],
            [1, 1],
            [0, 1],
            // East
            [0, 0],
            [1, 0],
            [1, 1],
            [0, 1],
            // West
            [0, 0],
            [1, 0],
            [1, 1],
            [0, 1],
            // North
            [0, 0],
            [1, 0],
            [1, 1],
            [0, 1],
            // South
            [0, 0],
            [1, 0],
            [1, 1],
            [0, 1],
        ];

        // if matches!(direction, BlockDirection::Up) {
        let range = match direction {
            BlockDirection::Up => 0..=3,
            BlockDirection::Down => 4..=7,
            BlockDirection::East => 8..=11,
            BlockDirection::West => 12..=15,
            BlockDirection::North => 16..=19,
            BlockDirection::South => 20..=23,
        };

        let starting_index = vertex_buffer.len() as u32;

        let vertex_iter = source_vertices[range.clone()]
            .iter()
            .map(|vec| UVec3::from(*vec) + offset)
            .zip(source_uv[range].iter().map(|uv| UVec2::from(*uv)))
            .map(|(position, uv)| -> u32 {
                TerrainVertex {
                    position,
                    direction,
                    uv,
                    w: texture_index,
                }
                .into()
            });

        vertex_buffer.extend(vertex_iter);

        indices.extend_from_slice(&[
            starting_index,
            starting_index + 1,
            starting_index + 2,
            starting_index + 2,
            starting_index + 3,
            starting_index,
        ]);
    }

    fn add_to_mesh(
        &self,
        block_registry: &BlockRegistry,
        neighbors: BlockNeighborSet,
        offset: UVec3,
        vertex_buffer: &mut Vec<u32>,
        indices: &mut Vec<u32>,
    ) {
        let block_data = block_registry
            .get(self)
            .expect("Did not find self in registry.");

        for direction in BlockDirection::ALL {
            let neighbor = neighbors.get(direction);

            // We have to draw this face.
            if neighbor.is_none() {
                let texture_index = block_data.faces.get_face_texture_index(direction);
                Self::insert_face(direction, offset, texture_index, vertex_buffer, indices);
            }
        }
    }
}

#[derive(Debug)]
struct TerrainVertex {
    position: UVec3,
    direction: BlockDirection,
    uv: UVec2,
    w: u16,
}

#[inline]
fn insert_vertex_bit(word: &mut u32, value: impl Into<u32>, width: usize, offset: usize) {
    let value = value.into();

    *word |= (value << offset) & ((!0u32 >> (32 - width)) << offset);
}

impl From<TerrainVertex> for u32 {
    fn from(vertex: TerrainVertex) -> Self {
        let mut value = 0u32;

        insert_vertex_bit(&mut value, vertex.position.x, 5, 0);
        insert_vertex_bit(&mut value, vertex.position.y, 5, 5);
        insert_vertex_bit(&mut value, vertex.position.z, 5, 10);

        insert_vertex_bit(&mut value, u8::from(vertex.direction), 3, 15);

        insert_vertex_bit(&mut value, vertex.uv.x, 1, 18);
        insert_vertex_bit(&mut value, vertex.uv.y, 1, 19);
        insert_vertex_bit(&mut value, vertex.w, 9, 20);

        value
    }
}

#[test]
fn terrain_vertex_encode() {
    let value: u32 = TerrainVertex {
        position: UVec3::new(0xFF, 0xEE, 0xDD),
        direction: BlockDirection::North,
        uv: UVec2::new(0, 1),
        w: 348,
    }
    .into();

    // This is equivalent to the decode function used in the shader side of this rendering system.
    fn extract_unsigned(word: u32, width: u32, offset: u32) -> u32 {
        (word >> offset) & (!0u32 >> (32u32 - width))
    }

    // fn extract_signed(word: u32, width: u32, offset: u32) -> i32 {
    //     let unsigned = (word >> offset) & (!0u32 >> (32u32 - width));
    //     let mask = 1u32 << (width - 1u32); // mask can be pre-computed if b is fixed
    //     let signed = (unsigned ^ mask).wrapping_sub(mask);

    //     println!(
    //         "{:08x} {:08x} {:08x} {:08x}",
    //         mask,
    //         (unsigned ^ mask),
    //         unsigned,
    //         signed
    //     );

    //     signed as i32
    // }

    // println!("{:08x}", value);
    assert_eq!(extract_unsigned(value, 5, 0), 0x1F);
    assert_eq!(extract_unsigned(value, 5, 5), 0x0E);
    assert_eq!(extract_unsigned(value, 5, 10), 0x1D);

    assert_eq!(
        extract_unsigned(value, 3, 15) as u8,
        u8::from(BlockDirection::North)
    );

    assert_eq!(extract_unsigned(value, 1, 18), 0);
    assert_eq!(extract_unsigned(value, 1, 19), 1);
    assert_eq!(extract_unsigned(value, 9, 20), 348);
}

#[derive(Component, Clone)]
pub struct Chunk {
    // Blocks are organized as x, z, y.
    blocks: [[[Option<Block>; Self::CHUNK_DIAMETER as usize]; Self::CHUNK_DIAMETER as usize];
        Self::CHUNK_DIAMETER as usize],
}

impl Chunk {
    pub const CHUNK_BIT_SHIFT: u32 = 4;
    pub const CHUNK_DIAMETER: i32 = 1 << Self::CHUNK_BIT_SHIFT as i32;

    pub fn new(block: Option<Block>) -> Self {
        Self {
            blocks: [[[block; Self::CHUNK_DIAMETER as usize]; Self::CHUNK_DIAMETER as usize];
                Self::CHUNK_DIAMETER as usize],
        }
    }

    fn get_neighbor(
        &self,
        position: LocalBlockCoordinate,
        direction: BlockDirection,
    ) -> Option<Block> {
        let offset: LocalBlockCoordinate = direction.into();
        let new_position = position + offset;

        self.get_block_local(new_position)
    }

    pub fn get_block_local(&self, position: LocalBlockCoordinate) -> Option<Block> {
        let x: usize = position.x.try_into().ok()?;
        let y: usize = position.y.try_into().ok()?;
        let z: usize = position.z.try_into().ok()?;

        self.blocks.get(x)?.get(z)?.get(y).and_then(|block| *block)
    }

    pub fn get_block_local_mut(
        &mut self,
        position: LocalBlockCoordinate,
    ) -> Option<&mut Option<Block>> {
        let x: usize = position.x.try_into().ok()?;
        let y: usize = position.y.try_into().ok()?;
        let z: usize = position.z.try_into().ok()?;

        self.blocks.get_mut(x)?.get_mut(z)?.get_mut(y)
    }

    pub fn iter(&self) -> impl Iterator<Item = (LocalBlockCoordinate, Option<Block>)> + '_ {
        self.blocks.iter().enumerate().flat_map(|(x, z_slices)| {
            z_slices.iter().enumerate().flat_map(move |(z, y_slices)| {
                y_slices.iter().enumerate().map(move |(y, block)| {
                    (
                        LocalBlockCoordinate::new(x as i32, y as i32, z as i32),
                        *block,
                    )
                })
            })
        })
    }

    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (LocalBlockCoordinate, &mut Option<Block>)> + '_ {
        self.blocks
            .iter_mut()
            .enumerate()
            .flat_map(|(x, z_slices)| {
                z_slices
                    .iter_mut()
                    .enumerate()
                    .flat_map(move |(z, y_slices)| {
                        y_slices.iter_mut().enumerate().map(move |(y, block)| {
                            (
                                LocalBlockCoordinate::new(x as i32, y as i32, z as i32),
                                block,
                            )
                        })
                    })
            })
    }

    pub fn iter_range_mut(
        &mut self,
        point_a: IVec3,
        point_b: IVec3,
    ) -> impl Iterator<Item = (LocalBlockCoordinate, &mut Option<Block>)> + '_ {
        fn to_valid_range(range: Range<i32>) -> Range<usize> {
            range.start as usize..range.end as usize
        }

        let lower_bound = point_a.min(point_b).max(IVec3::splat(0));
        let upper_bound = point_a.max(point_b).min(IVec3::splat(Self::CHUNK_DIAMETER));

        self.blocks
            .get_mut(to_valid_range(lower_bound.x..upper_bound.x))
            .expect("Range validation failed")
            .iter_mut()
            .enumerate()
            .flat_map(move |(x, z_slices)| {
                z_slices
                    .get_mut(to_valid_range(lower_bound.z..upper_bound.z))
                    .expect("Range validation failed")
                    .iter_mut()
                    .enumerate()
                    .flat_map(move |(z, y_slices)| {
                        y_slices
                            .get_mut(to_valid_range(lower_bound.y..upper_bound.y))
                            .expect("Range validation failed")
                            .iter_mut()
                            .enumerate()
                            .map(move |(y, block)| {
                                (
                                    LocalBlockCoordinate::new(x as i32, y as i32, z as i32)
                                        + lower_bound,
                                    block,
                                )
                            })
                    })
            })
    }

    pub fn iter_columns(
        &self,
    ) -> impl Iterator<Item = (IVec2, &[Option<Block>; Self::CHUNK_DIAMETER as usize])> {
        self.blocks.iter().enumerate().flat_map(|(x, z_slices)| {
            z_slices
                .iter()
                .enumerate()
                .map(move |(z, column)| (IVec2::new(x as i32, z as i32), column))
        })
    }

    pub fn iter_columns_mut(
        &mut self,
    ) -> impl Iterator<Item = (IVec2, &mut [Option<Block>; Self::CHUNK_DIAMETER as usize])> {
        self.blocks
            .iter_mut()
            .enumerate()
            .flat_map(|(x, z_slices)| {
                z_slices
                    .iter_mut()
                    .enumerate()
                    .map(move |(z, column)| (IVec2::new(x as i32, z as i32), column))
            })
    }

    pub fn build_mesh(&self, block_registry: &BlockRegistry) -> Mesh {
        let mut vertex_buffer = Vec::new();
        let mut indices = Vec::new();

        for (position, block) in self.iter() {
            if let Some(block) = block {
                let neighbors = BlockNeighborSet {
                    up: self.get_neighbor(position, BlockDirection::Up),
                    down: self.get_neighbor(position, BlockDirection::Down),
                    north: self.get_neighbor(position, BlockDirection::North),
                    south: self.get_neighbor(position, BlockDirection::South),
                    east: self.get_neighbor(position, BlockDirection::East),
                    west: self.get_neighbor(position, BlockDirection::West),
                };

                block.add_to_mesh(
                    block_registry,
                    neighbors,
                    position.as_uvec3(),
                    &mut vertex_buffer,
                    &mut indices,
                );
            }
        }

        let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);
        mesh.insert_attribute(
            MeshVertexAttribute::new("vertex", 0, VertexFormat::Uint32),
            VertexAttributeValues::Uint32(vertex_buffer),
        );
        mesh.set_indices(Some(Indices::U32(indices)));

        mesh
    }
}

#[derive(AsBindGroup, Reflect, FromReflect, Debug, Clone, TypeUuid)]
#[uuid = "ab106dd3-3971-4655-a535-b3b47738c649"]
#[uniform(0, StandardMaterialUniform)]
#[reflect(Default, Debug)]
struct TerrainMaterial {
    pub base_color: Color,

    #[texture(1, dimension = "2d_array")]
    #[sampler(2)]
    pub base_color_texture: Option<Handle<Image>>,

    pub emissive: Color,

    #[texture(3, dimension = "2d_array")]
    #[sampler(4)]
    pub emissive_texture: Option<Handle<Image>>,
    pub perceptual_roughness: f32,

    pub metallic: f32,

    #[texture(5, dimension = "2d_array")]
    #[sampler(6)]
    pub metallic_roughness_texture: Option<Handle<Image>>,
    #[doc(alias = "specular_intensity")]
    pub reflectance: f32,

    #[texture(9, dimension = "2d_array")]
    #[sampler(10)]
    pub normal_map_texture: Option<Handle<Image>>,

    pub flip_normal_map_y: bool,

    #[texture(7, dimension = "2d_array")]
    #[sampler(8)]
    pub occlusion_texture: Option<Handle<Image>>,
    pub double_sided: bool,
    // #[reflect(ignore)]
    // pub cull_mode: Option<Face>,
    pub unlit: bool,
    pub alpha_mode: AlphaMode,
    pub depth_bias: f32,
}

impl TerrainMaterial {
    pub fn new(terrain_texture: &TerrainTextureManager) -> Self {
        Self {
            base_color_texture: Some(terrain_texture.color_image_handle.clone()),
            emissive_texture: Some(terrain_texture.emissive_image_handle.clone()),
            metallic_roughness_texture: Some(terrain_texture.metallic_image_handle.clone()),
            occlusion_texture: Some(terrain_texture.occlusion_image_handle.clone()),
            normal_map_texture: Some(terrain_texture.normal_image_handle.clone()),
            ..Default::default()
        }
    }
}

impl Default for TerrainMaterial {
    fn default() -> Self {
        let dimension = Extent3d {
            width: 64,
            height: 128,
            depth_or_array_layers: 1,
        };

        let format = TextureFormat::Rgba8UnormSrgb;
        let data_length = dimension.volume() * 4;

        // We can't use the default because it's not an array. It'll cause a validation failure.
        let mut fallback_image = Image::new(
            dimension,
            TextureDimension::D2,
            vec![0u8; data_length],
            format,
        );
        fallback_image.reinterpret_stacked_2d_as_array(2);

        TerrainMaterial {
            base_color: Color::rgb(1.0, 1.0, 1.0),
            base_color_texture: None,
            emissive: Color::BLACK,
            emissive_texture: None,
            // This is the minimum the roughness is clamped to in shader code
            // See <https://google.github.io/filament/Filament.html#materialsystem/parameterization/>
            // It's the minimum floating point value that won't be rounded down to 0 in the
            // calculations used. Although technically for 32-bit floats, 0.045 could be
            // used.
            perceptual_roughness: 0.089,
            // Few materials are purely dielectric or metallic
            // This is just a default for mostly-dielectric
            metallic: 0.01,
            metallic_roughness_texture: None,
            // Minimum real-world reflectance is 2%, most materials between 2-5%
            // Expressed in a linear scale and equivalent to 4% reflectance see
            // <https://google.github.io/filament/Material%20Properties.pdf>
            reflectance: 0.5,
            occlusion_texture: None,
            normal_map_texture: None,
            flip_normal_map_y: false,
            double_sided: false,
            // cull_mode: Some(Face::Back),
            unlit: false,
            alpha_mode: AlphaMode::Opaque,
            depth_bias: 0.0,
        }
    }
}

impl AsBindGroupShaderType<StandardMaterialUniform> for TerrainMaterial {
    fn as_bind_group_shader_type(&self, images: &RenderAssets<Image>) -> StandardMaterialUniform {
        let mut flags = StandardMaterialFlags::NONE;
        if self.base_color_texture.is_some() {
            flags |= StandardMaterialFlags::BASE_COLOR_TEXTURE;
        }
        if self.emissive_texture.is_some() {
            flags |= StandardMaterialFlags::EMISSIVE_TEXTURE;
        }
        if self.metallic_roughness_texture.is_some() {
            flags |= StandardMaterialFlags::METALLIC_ROUGHNESS_TEXTURE;
        }
        if self.occlusion_texture.is_some() {
            flags |= StandardMaterialFlags::OCCLUSION_TEXTURE;
        }
        if self.double_sided {
            flags |= StandardMaterialFlags::DOUBLE_SIDED;
        }
        if self.unlit {
            flags |= StandardMaterialFlags::UNLIT;
        }
        let has_normal_map = self.normal_map_texture.is_some();
        if has_normal_map {
            if let Some(texture) = images.get(self.normal_map_texture.as_ref().unwrap()) {
                match texture.texture_format {
                    // All 2-component unorm formats
                    TextureFormat::Rg8Unorm
                    | TextureFormat::Rg16Unorm
                    | TextureFormat::Bc5RgUnorm
                    | TextureFormat::EacRg11Unorm => {
                        flags |= StandardMaterialFlags::TWO_COMPONENT_NORMAL_MAP;
                    }
                    _ => {}
                }
            }
            if self.flip_normal_map_y {
                flags |= StandardMaterialFlags::FLIP_NORMAL_MAP_Y;
            }
        }
        // NOTE: 0.5 is from the glTF default - do we want this?
        let mut alpha_cutoff = 0.5;
        match self.alpha_mode {
            AlphaMode::Opaque => flags |= StandardMaterialFlags::ALPHA_MODE_OPAQUE,
            AlphaMode::Mask(c) => {
                alpha_cutoff = c;
                flags |= StandardMaterialFlags::ALPHA_MODE_MASK;
            }
            AlphaMode::Blend => flags |= StandardMaterialFlags::ALPHA_MODE_BLEND,
            AlphaMode::Premultiplied => flags |= StandardMaterialFlags::ALPHA_MODE_PREMULTIPLIED,
            AlphaMode::Add => flags |= StandardMaterialFlags::ALPHA_MODE_ADD,
            AlphaMode::Multiply => flags |= StandardMaterialFlags::ALPHA_MODE_MULTIPLY,
        };

        StandardMaterialUniform {
            base_color: self.base_color.as_linear_rgba_f32().into(),
            emissive: self.emissive.into(),
            roughness: self.perceptual_roughness,
            metallic: self.metallic,
            reflectance: self.reflectance,
            flags: flags.bits(),
            alpha_cutoff,
        }
    }
}

impl Material for TerrainMaterial {
    fn vertex_shader() -> ShaderRef {
        "shaders/terrain.wgsl".into()
    }

    fn fragment_shader() -> ShaderRef {
        // "shaders/terrain.wgsl".into()
        StandardMaterial::fragment_shader()
    }

    fn specialize(
        _pipeline: &bevy::pbr::MaterialPipeline<Self>,
        descriptor: &mut bevy::render::render_resource::RenderPipelineDescriptor,
        _layout: &bevy::render::mesh::MeshVertexBufferLayout,
        _key: bevy::pbr::MaterialPipelineKey<Self>,
    ) -> Result<(), bevy::render::render_resource::SpecializedMeshPipelineError> {
        let defines = [
            ShaderDefVal::Bool(String::from("VERTEX_UVS"), true),
            ShaderDefVal::Bool(String::from("STANDARDMATERIAL_NORMAL_MAP"), true),
            ShaderDefVal::Bool(String::from("VERTEX_TANGENTS"), true),
            ShaderDefVal::Bool(String::from("ARRAY_TEXTURES"), true),
        ];

        descriptor.vertex.shader_defs.extend(defines.clone());
        let fragment = descriptor
            .fragment
            .as_mut()
            .expect("Fragment shader unavailable.");
        fragment.shader_defs.extend(defines);

        Ok(())
    }
}

enum ImageSource {
    Handle(Handle<Image>),
    Blank([u8; 4]),
    Loaded,
}

struct LoadingSet {
    color: ImageSource,
    emissive: ImageSource,
    metallic: ImageSource,
    occlusion: ImageSource,
    normal: ImageSource,
}

enum TerrainLoadingState {
    Loading {
        image_handles: Vec<LoadingSet>,
        size: Extent3d,
        color_image_data: Vec<u8>,
        emissive_image_data: Vec<u8>,
        metallic_image_data: Vec<u8>,
        occlusion_image_data: Vec<u8>,
        normal_image_data: Vec<u8>,
    },
    Loaded,
}

#[derive(Resource)]
pub struct TerrainTextureManager {
    loading_state: TerrainLoadingState,
    color_image_handle: Handle<Image>,
    emissive_image_handle: Handle<Image>,
    metallic_image_handle: Handle<Image>,
    occlusion_image_handle: Handle<Image>,
    normal_image_handle: Handle<Image>,
    image_paths: HashMap<String, usize>,
}

impl TerrainTextureManager {
    pub fn new(
        asset_server: &Res<AssetServer>,
        size: Extent3d,
        image_resources: &mut ResMut<Assets<Image>>,
        images: impl Iterator<
            Item = (
                impl Into<String>,
                Option<AssetPath<'static>>,
                Option<AssetPath<'static>>,
            ),
        >,
    ) -> Self {
        // format.pixel_size() // TODO validate pixel format of images.
        let mut image_handles = Vec::new();
        let mut image_paths = HashMap::new();

        assert_eq!(
            size.depth_or_array_layers, 1,
            "Only load 1 dimensional images."
        );

        fn extract_path(
            asset_server: &Res<AssetServer>,
            default: [u8; 4],
            path: Option<AssetPath>,
        ) -> ImageSource {
            path.map_or(ImageSource::Blank(default), |path| {
                ImageSource::Handle(asset_server.load(path))
            })
        }

        for (name, color_image_path, normal_image_path) in images {
            let name = name.into();

            let color = extract_path(asset_server, [0, 0, 0, 0], color_image_path);
            let normal = extract_path(asset_server, [0, 128, 0, 255], normal_image_path);

            image_paths.insert(name, image_handles.len());
            image_handles.push(LoadingSet {
                color,
                emissive: ImageSource::Blank([0, 0, 0, 255]),
                metallic: ImageSource::Blank([255, 255, 255, 255]),
                occlusion: ImageSource::Blank([255, 255, 255, 255]),
                normal,
            });
        }

        if image_handles.is_empty() {
            // FIXME should switch to an error state.
            panic!("No terrain images were provided.");
        }

        let dimension = Extent3d {
            width: 64,
            height: 128,
            depth_or_array_layers: 1,
        };

        let format = TextureFormat::Rgba8UnormSrgb;
        let data_length = dimension.volume() * 4;

        let mut placeholder_image = Image::new(
            dimension,
            TextureDimension::D2,
            vec![0u8; data_length],
            format,
        );
        placeholder_image.reinterpret_stacked_2d_as_array(2);

        Self {
            loading_state: TerrainLoadingState::Loading {
                image_handles,
                size,
                color_image_data: Vec::new(),
                emissive_image_data: Vec::new(),
                metallic_image_data: Vec::new(),
                occlusion_image_data: Vec::new(),
                normal_image_data: Vec::new(),
            },
            color_image_handle: image_resources.add(placeholder_image.clone()),
            emissive_image_handle: image_resources.add(placeholder_image.clone()),
            metallic_image_handle: image_resources.add(placeholder_image.clone()),
            occlusion_image_handle: image_resources.add(placeholder_image.clone()),
            normal_image_handle: image_resources.add(placeholder_image),
            image_paths,
        }
    }

    pub fn get_image_index(&self, name: &str) -> u16 {
        if let Some(index) = self.image_paths.get(name).map(|index| *index as u16) {
            index
        } else {
            log::warn!("Request for terrain texture `{}` could not be satisfied. It has been replaced with a default.", name);
            0
        }
    }
}

/// A new type so I can store a handle to a resource.
#[derive(Resource)]
pub struct TerrainMaterialHandle(Handle<TerrainMaterial>);

fn terrain_texture_loading(
    asset_server: Res<AssetServer>,
    mut texture: ResMut<TerrainTextureManager>,
    mut images: ResMut<Assets<Image>>,
    terrain_material: ResMut<TerrainMaterialHandle>,
    mut terrain_material_assets: ResMut<Assets<TerrainMaterial>>,
) {
    fn process_image(
        image_handle_container: &mut ImageSource,
        final_image_data: &mut Vec<u8>,
        size: &Extent3d,
        asset_server: &Res<AssetServer>,
        images: &mut ResMut<Assets<Image>>,
    ) -> bool {
        match image_handle_container {
            ImageSource::Handle(image_handle) => {
                let load_state = asset_server.get_load_state(image_handle.clone());

                match load_state {
                    LoadState::Loaded => {
                        let image = images
                            .remove(image_handle.clone())
                            .expect("Image wasn't actually loaded."); // FIXME we need an error state in this game.
                        *image_handle_container = ImageSource::Loaded; // Mark that it's been transferred over.

                        // We need all textures to be the same size. Make sure to enforce that.
                        if image.texture_descriptor.size != *size {
                            // FIXME this should switch us to an error state.
                            // TODO list the culprit.
                            panic!("All textures must be of the same size and format.");
                        }

                        final_image_data.extend(image.data);

                        true
                    }
                    LoadState::Failed => {
                        // FIXME this should switch us to an error state.
                        // TODO list the culprit.
                        panic!("Failed to load a terrain texture.");
                    }
                    _ => false,
                }
            }
            ImageSource::Blank(fill) => {
                let fill = *fill;
                *image_handle_container = ImageSource::Loaded; // Mark that it's been transferred over.

                final_image_data.extend(
                    std::iter::repeat(fill)
                        .zip(0..size.volume())
                        .flat_map(|(byte, _index)| byte),
                );

                true
            }
            ImageSource::Loaded => true,
        }
    }

    fn finalize_image(
        true_size: Extent3d,
        num_layers: u32,
        images: &mut ResMut<Assets<Image>>,
        final_image_handle: &Handle<Image>,
        final_image_data: Vec<u8>,
        terrain_material_image_handle: &mut Option<Handle<Image>>,
    ) {
        let mut pre_size = true_size; // Copies.
        pre_size.height *= num_layers;

        // FIXME switch to an error state.
        let image = images
            .get_mut(final_image_handle)
            .expect("Texture image was not initially created.");

        *image = Image::new(
            pre_size,
            TextureDimension::D2,
            final_image_data,
            TextureFormat::Rgba8UnormSrgb,
        );
        image.reinterpret_stacked_2d_as_array(num_layers);

        *terrain_material_image_handle = Some(final_image_handle.clone());
    }

    if let TerrainLoadingState::Loading {
        size,
        image_handles,
        color_image_data,
        emissive_image_data,
        metallic_image_data,
        occlusion_image_data,
        normal_image_data,
    } = &mut texture.as_mut().loading_state
    {
        // Check that all the images we depend on are loaded, otherwise, bail out.
        let mut ready = true;

        for image_set in image_handles.iter_mut() {
            ready &= process_image(
                &mut image_set.color,
                color_image_data,
                size,
                &asset_server,
                &mut images,
            );
            ready &= process_image(
                &mut image_set.emissive,
                emissive_image_data,
                size,
                &asset_server,
                &mut images,
            );
            ready &= process_image(
                &mut image_set.metallic,
                metallic_image_data,
                size,
                &asset_server,
                &mut images,
            );
            ready &= process_image(
                &mut image_set.occlusion,
                occlusion_image_data,
                size,
                &asset_server,
                &mut images,
            );
            ready &= process_image(
                &mut image_set.normal,
                normal_image_data,
                size,
                &asset_server,
                &mut images,
            );

            // Bail out early so we can guarantee the loading order.
            if !ready {
                break;
            }
        }

        if ready {
            let mut swap_state = TerrainLoadingState::Loaded;
            std::mem::swap(&mut swap_state, &mut texture.loading_state);

            if let TerrainLoadingState::Loading {
                image_handles,
                size,
                color_image_data: final_color_image_data,
                emissive_image_data,
                metallic_image_data,
                occlusion_image_data,
                normal_image_data: final_normal_image_data,
            } = swap_state
            {
                let num_layers = image_handles.len() as u32;

                let terrain_material = terrain_material_assets
                    .get_mut(&terrain_material.0)
                    .expect("Terrain material is not available.");

                finalize_image(
                    size,
                    num_layers,
                    &mut images,
                    &texture.color_image_handle,
                    final_color_image_data,
                    &mut terrain_material.base_color_texture,
                );

                finalize_image(
                    size,
                    num_layers,
                    &mut images,
                    &texture.emissive_image_handle,
                    emissive_image_data,
                    &mut terrain_material.emissive_texture,
                );

                finalize_image(
                    size,
                    num_layers,
                    &mut images,
                    &texture.metallic_image_handle,
                    metallic_image_data,
                    &mut terrain_material.metallic_roughness_texture,
                );

                finalize_image(
                    size,
                    num_layers,
                    &mut images,
                    &texture.occlusion_image_handle,
                    occlusion_image_data,
                    &mut terrain_material.occlusion_texture,
                );

                finalize_image(
                    size,
                    num_layers,
                    &mut images,
                    &texture.normal_image_handle,
                    final_normal_image_data,
                    &mut terrain_material.normal_map_texture,
                );

                log::info!("Terrain data loaded and ready.");
            } else {
                panic!("Texture was not loading."); // FIXME This should change to an error state.
            }
        }
    }
}

pub type ChunkIndex = IVec3;

#[derive(Component)]
pub struct ChunkPosition {
    pub index: ChunkIndex,
}

impl ChunkPosition {
    pub fn as_transform(&self) -> Transform {
        Transform::from_translation(
            self.index.as_vec3() * Vec3::splat(Chunk::CHUNK_DIAMETER as f32),
        )
    }

    pub fn as_block_coordinate(&self) -> GlobalBlockCoordinate {
        self.index * IVec3::splat(Chunk::CHUNK_DIAMETER)
    }
}

// It's everything a chunk needs, except the chunk.
#[derive(Bundle)]
struct PreChunkBundle {
    chunk_position: ChunkPosition,
    global_transform: GlobalTransform,
    visibility: Visibility,
    computed_visibility: ComputedVisibility,
}

impl Default for PreChunkBundle {
    fn default() -> Self {
        Self {
            chunk_position: ChunkPosition { index: IVec3::ZERO },
            global_transform: Default::default(),
            visibility: Default::default(),
            computed_visibility: Default::default(),
        }
    }
}

fn terrain_setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut image_resources: ResMut<Assets<Image>>,
    mut terrain_material_assets: ResMut<Assets<TerrainMaterial>>,
) {
    // For terrain rendering.
    let terrain_texture = TerrainTextureManager::new(
        &asset_server,
        Extent3d {
            // TODO make this user configurable.
            width: 512,
            height: 512,
            depth_or_array_layers: 1,
        },
        &mut image_resources,
        vec![
            (
                "default",
                Some("terrain/default-color.png".into()),
                Some("terrain/default-normal.png".into()),
            ),
            (
                "dirt",
                Some("terrain/dirt-color.png".into()),
                Some("terrain/dirt-normal.png".into()),
            ),
            (
                "grass_top",
                Some("terrain/grass_top-color.png".into()),
                Some("terrain/grass_top-normal.png".into()),
            ),
            (
                "grass_side",
                Some("terrain/grass_side-color.png".into()),
                Some("terrain/grass_side-normal.png".into()),
            ),
            (
                "sand",
                Some("terrain/sand-color.png".into()),
                Some("terrain/sand-normal.png".into()),
            ),
            (
                "stone",
                Some("terrain/stone-color.png".into()),
                Some("terrain/stone-normal.png".into()),
            ),
        ]
        .drain(..),
    );

    let block_registry = BlockRegistry::load(&terrain_texture).unwrap();
    // let stone_tag = BlockTag::try_from("core:stone").unwrap();
    // let stone_data = block_registry.get_by_tag(&stone_tag).unwrap();
    // let stone_block = stone_data.spawn();
    // let dirt_tag = BlockTag::try_from("core:dirt").unwrap();
    // let dirt_data = block_registry.get_by_tag(&dirt_tag).unwrap();
    // let dirt_block = dirt_data.spawn();
    // let default_tag = BlockTag::try_from("core:default").unwrap();
    // let default_data = block_registry.get_by_tag(&default_tag).unwrap();
    // let default_block = default_data.spawn();

    // let mut hump_chunk = Chunk::new(Some(default_block));
    // for (position, block) in hump_chunk.iter_mut() {
    //     let position = position.as_vec3();

    //     let height = ((position.x / 16.0) * std::f64::consts::PI as f32).sin()
    //         * ((position.z / 16.0) * std::f64::consts::PI as f32).sin()
    //         * 16.0;
    //     if position.y > height {
    //         *block = None;
    //     }
    //     //  else if (position.x % 2f32) == 0f32 {
    //     //     chunk
    //     //         .set_block_local(
    //     //             BlockLocalCoordinate::new(x as i8, y as i8, z as i8),
    //     //             Some(dirt_block),
    //     //         )
    //     //         .ok();
    //     // }
    // }

    // let box_chunk = Chunk::new(Some(default_block));

    let mut terrain_material = TerrainMaterial::new(&terrain_texture);
    terrain_material.reflectance = 0.0;

    let terrain_material_handle = terrain_material_assets.add(terrain_material);
    commands.insert_resource(block_registry);
    commands.insert_resource(TerrainMaterialHandle(terrain_material_handle));
    commands.insert_resource(terrain_texture);

    // commands.spawn((
    //     hump_chunk.clone(),
    //     Position {
    //         translation: Vec3::new(0.0, 0.0, 0.0),
    //         rotation: 0.0,
    //     },
    //     SpatialHashOffset {
    //         translation: Vec3::new(8.0, 0.0, 8.0),
    //     },
    // ));
    // commands.spawn((
    //     hump_chunk.clone(),
    //     Position {
    //         translation: Vec3::new(0.0, 0.0, 16.0),
    //         rotation: 0.0,
    //     },
    //     SpatialHashOffset {
    //         translation: Vec3::new(8.0, 0.0, 8.0),
    //     },
    // ));

    // commands.spawn((
    //     box_chunk.clone(),
    //     Position {
    //         translation: Vec3::new(-16.0, 0.0, 0.0),
    //         rotation: 0.0,
    //     },
    //     SpatialHashOffset {
    //         translation: Vec3::new(8.0, 0.0, 8.0),
    //     },
    // ));
    // commands.spawn((
    //     box_chunk,
    //     Position {
    //         translation: Vec3::new(-16.0, 0.0, 16.0),
    //         rotation: 0.0,
    //     },
    //     SpatialHashOffset {
    //         translation: Vec3::new(8.0, 0.0, 8.0),
    //     },
    // ));

    // commands.spawn((
    //     hump_chunk.clone(),
    //     Position {
    //         translation: Vec3::new(16.0, 0.0, 0.0),
    //         rotation: std::f64::consts::FRAC_PI_4 as f32,
    //     },
    //     SpatialHashOffset {
    //         translation: Vec3::new(8.0, 0.0, 8.0),
    //     },
    // ));
    // commands.spawn((
    //     hump_chunk.clone(),
    //     Position {
    //         translation: Vec3::new(16.0, 0.0, 0.0)
    //             + Quat::from_rotation_y(std::f64::consts::FRAC_PI_4 as f32)
    //                 * Vec3::new(16.0, 0.0, 0.0),
    //         rotation: std::f64::consts::FRAC_PI_4 as f32,
    //     },
    //     SpatialHashOffset {
    //         translation: Vec3::new(8.0, 0.0, 8.0),
    //     },
    // ));

    // commands.spawn((
    //     hump_chunk,
    //     Position {
    //         translation: Vec3::new(-32.0, 0.0, 0.0),
    //         rotation: std::f64::consts::FRAC_PI_4 as f32,
    //     },
    //     SpatialHashOffset {
    //         translation: Vec3::new(8.0, 0.0, 8.0),
    //     },
    //     Velocity {
    //         translation: Vec3::ZERO,
    //         rotational: 0.5,
    //     },
    // ));
}

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct UpdateMesh;

fn generate_chunk_mesh(
    mut commands: Commands,
    terrain_material: Res<TerrainMaterialHandle>,
    block_registry: Res<BlockRegistry>,
    chunks: Query<(Entity, &Chunk, With<UpdateMesh>)>,
    mut meshes: ResMut<Assets<Mesh>>,
) {
    for (entity, chunk, _with_update_mesh) in chunks.iter() {
        let chunk_mesh = chunk.build_mesh(&block_registry);

        commands
            .entity(entity)
            .insert((meshes.add(chunk_mesh), terrain_material.0.clone()))
            .remove::<UpdateMesh>();
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct TerrainPlugin;

impl Plugin for TerrainPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugin(MaterialPlugin::<TerrainMaterial>::default());

        app.configure_set(TerrainPlugin);
        app.add_startup_system(terrain_setup.in_set(TerrainPlugin));

        app.add_system(generate_chunk_mesh);
        app.add_system(terrain_texture_loading);

        terrain_file::register_terrain_files(app);
        terrain_generation::register_terrain_generators(app);
        terrain_space::register_terrain_space(app);
    }
}
