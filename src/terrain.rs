use bevy::{
    asset::{AssetPath, LoadState},
    prelude::*,
    reflect::TypeUuid,
    render::{
        mesh::{Indices, MeshVertexAttribute, VertexAttributeValues},
        render_resource::{AsBindGroup, Extent3d, PrimitiveTopology, ShaderRef, VertexFormat},
        texture::Volume,
    },
};
use nalgebra::{Vector2, Vector3};
use std::{borrow::Cow, collections::HashMap, num::NonZeroU16, str::FromStr};
use thiserror::Error;
use wgpu::{TextureDimension, TextureFormat};

type BlockID = NonZeroU16;
pub type BlockCoordinate = nalgebra::Vector3<i64>;
pub type BlockLocalCoordinate = nalgebra::Vector3<i8>;

#[derive(Error, Debug)]
pub enum BlockIndexError {
    #[error("Indexed block out of chunk range.")]
    OutOfRange,
}

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

impl From<BlockDirection> for BlockCoordinate {
    fn from(direction: BlockDirection) -> Self {
        match direction {
            BlockDirection::Up => Self::y(),
            BlockDirection::Down => Self::zeros() - Self::y(),
            BlockDirection::North => Self::z(),
            BlockDirection::South => Self::zeros() - Self::z(),
            BlockDirection::East => Self::x(),
            BlockDirection::West => Self::zeros() - Self::x(),
        }
    }
}

impl From<BlockDirection> for BlockLocalCoordinate {
    fn from(direction: BlockDirection) -> Self {
        match direction {
            BlockDirection::Up => Self::y(),
            BlockDirection::Down => Self::zeros() - Self::y(),
            BlockDirection::North => Self::z(),
            BlockDirection::South => Self::zeros() - Self::z(),
            BlockDirection::East => Self::x(),
            BlockDirection::West => Self::zeros() - Self::x(),
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
    name: String,
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

        let stone_index = terrain_texture.get_image_index("terrain/stone-color.png");
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

        let dirt_index = terrain_texture.get_image_index("terrain/dirt-color.png");
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

            let name = name.into();

            let id = self.block_data.len() as u16;
            let id = BlockID::new(id + 1).expect("Block ID miscalculated.");
            self.block_data.push(BlockData { name, id, faces });
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
        offset: Vector3<u8>,
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

        let source_normals = &[
            // Top
            [0, 1, 0],
            [0, 1, 0],
            [0, 1, 0],
            [0, 1, 0],
            // Bottom
            [0, -1, 0],
            [0, -1, 0],
            [0, -1, 0],
            [0, -1, 0],
            // East
            [1, 0, 0],
            [1, 0, 0],
            [1, 0, 0],
            [1, 0, 0],
            // West
            [-1, 0, 0],
            [-1, 0, 0],
            [-1, 0, 0],
            [-1, 0, 0],
            // North
            [0, 0, 1],
            [0, 0, 1],
            [0, 0, 1],
            [0, 0, 1],
            // South
            [0, 0, -1],
            [0, 0, -1],
            [0, 0, -1],
            [0, 0, -1],
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
            .map(|vec| Vector3::from(*vec) + offset)
            .zip(
                source_normals[range.clone()]
                    .iter()
                    .map(|normal| Vector3::from(*normal)),
            )
            .zip(source_uv[range].iter().map(|uv| Vector2::from(*uv)))
            .map(|((position, normal), uv)| -> u32 {
                TerrainVertex {
                    position,
                    normal,
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
        offset: Vector3<u8>,
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
    position: Vector3<u8>,
    normal: Vector3<i8>,
    uv: Vector2<u8>,
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

        insert_vertex_bit(&mut value, vertex.normal.x as u8, 2, 15);
        insert_vertex_bit(&mut value, vertex.normal.y as u8, 2, 17);
        insert_vertex_bit(&mut value, vertex.normal.z as u8, 2, 19);

        insert_vertex_bit(&mut value, vertex.uv.x, 1, 21);
        insert_vertex_bit(&mut value, vertex.uv.y, 1, 22);
        insert_vertex_bit(&mut value, vertex.w, 9, 23);
        // 32

        value
    }
}

#[test]
fn terrain_vertex_encode() {
    let value: u32 = TerrainVertex {
        position: nalgebra::Vector3::new(0xFF, 0xEE, 0xDD),
        normal: nalgebra::Vector3::new(0, 1, -1),
        uv: nalgebra::Vector2::new(0, 1),
        w: 348,
    }
    .into();

    // This is equivalent to the decode function used in the shader side of this rendering system.
    fn extract_unsigned(word: u32, width: u32, offset: u32) -> u32 {
        (word >> offset) & (!0u32 >> (32u32 - width))
    }

    fn extract_signed(word: u32, width: u32, offset: u32) -> i32 {
        let unsigned = (word >> offset) & (!0u32 >> (32u32 - width));
        let mask = 1u32 << (width - 1u32); // mask can be pre-computed if b is fixed
        let signed = (unsigned ^ mask).wrapping_sub(mask);

        println!(
            "{:08x} {:08x} {:08x} {:08x}",
            mask,
            (unsigned ^ mask),
            unsigned,
            signed
        );

        signed as i32
    }

    // println!("{:08x}", value);
    assert_eq!(extract_unsigned(value, 5, 0), 0x1F);
    assert_eq!(extract_unsigned(value, 5, 5), 0x0E);
    assert_eq!(extract_unsigned(value, 5, 10), 0x1D);

    assert_eq!(extract_signed(value, 2, 15), 0);
    assert_eq!(extract_signed(value, 2, 17), 1);
    assert_eq!(extract_signed(value, 2, 19), -1);

    assert_eq!(extract_unsigned(value, 1, 21), 0);
    assert_eq!(extract_unsigned(value, 1, 22), 1);
    assert_eq!(extract_unsigned(value, 9, 23), 348);
}

#[derive(Component)]
pub struct Chunk {
    blocks: [[[Option<Block>; Self::CHUNK_DIAMETER]; Self::CHUNK_DIAMETER]; Self::CHUNK_DIAMETER],
}

impl Chunk {
    pub const CHUNK_DIAMETER: usize = 16;

    pub fn new(block: Option<Block>) -> Self {
        Self {
            blocks: [[[block; Self::CHUNK_DIAMETER]; Self::CHUNK_DIAMETER]; Self::CHUNK_DIAMETER],
        }
    }

    fn get_neighbor(
        &self,
        position: BlockLocalCoordinate,
        direction: BlockDirection,
    ) -> Option<Block> {
        let offset: BlockLocalCoordinate = direction.into();
        let new_position = position + offset;

        self.get_block_local(new_position)
    }

    pub fn get_block_local(&self, position: BlockLocalCoordinate) -> Option<Block> {
        let x: usize = position.x.try_into().ok()?;
        let y: usize = position.y.try_into().ok()?;
        let z: usize = position.z.try_into().ok()?;

        self.blocks.get(z)?.get(y)?.get(x).and_then(|block| *block)
    }

    pub fn set_block_local(
        &mut self,
        position: BlockLocalCoordinate,
        block: Option<Block>,
    ) -> Result<(), BlockIndexError> {
        let x: usize = position
            .x
            .try_into()
            .map_err(|_error| BlockIndexError::OutOfRange)?;
        let y: usize = position
            .y
            .try_into()
            .map_err(|_error| BlockIndexError::OutOfRange)?;
        let z: usize = position
            .z
            .try_into()
            .map_err(|_error| BlockIndexError::OutOfRange)?;

        *self
            .blocks
            .get_mut(z)
            .ok_or(BlockIndexError::OutOfRange)?
            .get_mut(y)
            .ok_or(BlockIndexError::OutOfRange)?
            .get_mut(x)
            .ok_or(BlockIndexError::OutOfRange)? = block;

        Ok(())
    }

    pub fn build_mesh(&self, block_registry: &BlockRegistry) -> Mesh {
        let mut vertex_buffer = Vec::new();
        let mut indices = Vec::new();

        for (z_offset, z) in self.blocks.iter().enumerate() {
            for (y_offset, y) in z.iter().enumerate() {
                for (x_offset, x) in y.iter().enumerate() {
                    if let Some(x) = x {
                        let position = BlockLocalCoordinate::new(
                            x_offset as i8,
                            y_offset as i8,
                            z_offset as i8,
                        );

                        let neighbors = BlockNeighborSet {
                            up: self.get_neighbor(position, BlockDirection::Up),
                            down: self.get_neighbor(position, BlockDirection::Down),
                            north: self.get_neighbor(position, BlockDirection::North),
                            south: self.get_neighbor(position, BlockDirection::South),
                            east: self.get_neighbor(position, BlockDirection::East),
                            west: self.get_neighbor(position, BlockDirection::West),
                        };

                        x.add_to_mesh(
                            block_registry,
                            neighbors,
                            Vector3::new(x_offset as u8, y_offset as u8, z_offset as u8),
                            &mut vertex_buffer,
                            &mut indices,
                        );
                    }
                }
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

#[derive(Resource, AsBindGroup, TypeUuid, Debug, Clone)]
#[uuid = "ab106dd3-3971-4655-a535-b3b47738c649"]
pub struct TerrainMaterial {
    #[texture(0, dimension = "2d_array")]
    #[sampler(1)]
    color_texture: Handle<Image>,

    #[texture(2, dimension = "2d_array")]
    #[sampler(3)]
    normal_texture: Handle<Image>,
}

impl TerrainMaterial {
    pub fn new(terrain_texture: &TerrainTextureManager) -> Self {
        Self {
            color_texture: terrain_texture.color_image_handle.clone(),
            normal_texture: terrain_texture.normal_image_handle.clone(),
        }
    }
}

impl Material for TerrainMaterial {
    fn vertex_shader() -> ShaderRef {
        "shaders/terrain.wgsl".into()
    }

    fn fragment_shader() -> ShaderRef {
        "shaders/terrain.wgsl".into()
    }
}

struct LoadingSet {
    color: Option<Handle<Image>>,
    normal: Option<Handle<Image>>,
}

enum TerrainLoadingState {
    Loading {
        image_handles: Vec<LoadingSet>,
        size: Option<Extent3d>, // Will be set by the first image loaded.
        final_color_image_data: Vec<u8>,
        final_normal_image_data: Vec<u8>,
    },
    Loaded,
}

#[derive(Resource)]
pub struct TerrainTextureManager {
    loading_state: TerrainLoadingState,
    color_image_handle: Handle<Image>,
    normal_image_handle: Handle<Image>,
    image_paths: HashMap<String, usize>,
}

impl TerrainTextureManager {
    pub fn new(
        asset_server: &Res<AssetServer>,
        image_resources: &mut ResMut<Assets<Image>>,
        images: impl Iterator<Item = (impl Into<String>, AssetPath<'static>, AssetPath<'static>)>,
    ) -> Self {
        let mut image_handles = Vec::new();
        let mut image_paths = HashMap::new();

        for (name, color_image_path, normal_image_path) in images {
            let name = name.into();

            let color_image_handle: Handle<Image> = asset_server.load(color_image_path.clone());
            let normal_image_handle: Handle<Image> = asset_server.load(normal_image_path.clone());

            image_paths.insert(name, image_handles.len());
            image_handles.push(LoadingSet {
                color: Some(color_image_handle),
                normal: Some(normal_image_handle),
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

        let mut color_image = Image::new(
            dimension,
            TextureDimension::D2,
            vec![0u8; data_length],
            format,
        );
        color_image.reinterpret_stacked_2d_as_array(2);
        let normal_image = color_image.clone();

        Self {
            loading_state: TerrainLoadingState::Loading {
                image_handles,
                size: None,
                final_color_image_data: Vec::new(),
                final_normal_image_data: Vec::new(),
            },
            color_image_handle: image_resources.add(color_image),
            normal_image_handle: image_resources.add(normal_image),
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

pub fn terrain_texture_loading(
    asset_server: Res<AssetServer>,
    mut texture: ResMut<TerrainTextureManager>,
    mut images: ResMut<Assets<Image>>,
    terrain_material: ResMut<TerrainMaterialHandle>,
    mut terrain_material_assets: ResMut<Assets<TerrainMaterial>>,
) {
    fn process_image(
        image_handle_container: &mut Option<Handle<Image>>,
        final_image_data: &mut Vec<u8>,
        size: &mut Option<Extent3d>,
        asset_server: &Res<AssetServer>,
        images: &mut ResMut<Assets<Image>>,
    ) -> bool {
        if let Some(image_handle) = image_handle_container {
            let load_state = asset_server.get_load_state(image_handle.clone());

            match load_state {
                LoadState::Loaded => {
                    let image = images
                        .remove(image_handle.clone())
                        .expect("Image wasn't actually loaded."); // FIXME we need an error state in this game.
                    *image_handle_container = None; // Mark that it's been transferred over.

                    // We need all textures to be the same size. Make sure to enforce that.
                    if let Some(size) = size {
                        if image.texture_descriptor.size != *size {
                            // FIXME this should switch us to an error state.
                            // TODO list the culprit.
                            panic!("All textures must be of the same size and format.");
                        }
                    } else {
                        *size = Some(image.texture_descriptor.size);
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
        } else {
            true
        }
    }

    fn finalize_image(
        true_size: Extent3d,
        num_layers: u32,
        images: &mut ResMut<Assets<Image>>,
        final_image_handle: &Handle<Image>,
        final_image_data: Vec<u8>,
        terrain_material_image_handle: &mut Handle<Image>,
    ) {
        // FIXME this should switch to an error state.
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

        *terrain_material_image_handle = final_image_handle.clone();
    }

    if let TerrainLoadingState::Loading {
        size,
        image_handles,
        final_color_image_data,
        final_normal_image_data,
    } = &mut texture.as_mut().loading_state
    {
        // Check that all the images we depend on are loaded, otherwise, bail out.
        let mut ready = true;

        for image_set in image_handles.iter_mut() {
            ready &= process_image(
                &mut image_set.color,
                final_color_image_data,
                size,
                &asset_server,
                &mut images,
            );
            ready &= process_image(
                &mut image_set.normal,
                final_normal_image_data,
                size,
                &asset_server,
                &mut images,
            );
        }

        if ready {
            let mut swap_state = TerrainLoadingState::Loaded;
            std::mem::swap(&mut swap_state, &mut texture.loading_state);

            if let TerrainLoadingState::Loading {
                image_handles,
                size,
                final_color_image_data,
                final_normal_image_data,
            } = swap_state
            {
                let num_layers = image_handles.len() as u32;

                let terrain_material = terrain_material_assets
                    .get_mut(&terrain_material.0)
                    .expect("Terrain material is not available.");

                let size = size.expect("Texture size was never provided.");

                finalize_image(
                    size,
                    num_layers,
                    &mut images,
                    &texture.color_image_handle,
                    final_color_image_data,
                    &mut terrain_material.color_texture,
                );

                finalize_image(
                    size,
                    num_layers,
                    &mut images,
                    &texture.normal_image_handle,
                    final_normal_image_data,
                    &mut terrain_material.normal_texture,
                );

                log::info!("Terrain data loaded and ready.");
            } else {
                panic!("Texture was not loading."); // FIXME This should change to an error state.
            }
        }
    }
}

pub fn terrain_setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut image_resources: ResMut<Assets<Image>>,
    mut terrain_material_assets: ResMut<Assets<TerrainMaterial>>,
) {
    // For terrain rendering.
    let terrain_texture = TerrainTextureManager::new(
        &asset_server,
        &mut image_resources,
        vec![
            (
                "default",
                "terrain/default-color.png".into(),
                "terrain/default-normal.png".into(),
            ),
            (
                "dirt",
                "terrain/dirt-color.png".into(),
                "terrain/dirt-normal.png".into(),
            ),
            (
                "grass_top",
                "terrain/grass_top-color.png".into(),
                "terrain/grass_top-normal.png".into(),
            ),
            (
                "grass_side",
                "terrain/grass_side-color.png".into(),
                "terrain/grass_side-normal.png".into(),
            ),
            (
                "sand",
                "terrain/sand-color.png".into(),
                "terrain/sand-normal.png".into(),
            ),
            (
                "stone",
                "terrain/stone-color.png".into(),
                "terrain/stone-normal.png".into(),
            ),
        ]
        .drain(..),
    );

    let block_registry = BlockRegistry::load(&terrain_texture).unwrap();
    let stone_tag = BlockTag::try_from("core:stone").unwrap();
    let stone_data = block_registry.get_by_tag(&stone_tag).unwrap();
    let stone_block = stone_data.spawn();
    let dirt_tag = BlockTag::try_from("core:dirt").unwrap();
    let dirt_data = block_registry.get_by_tag(&dirt_tag).unwrap();
    let dirt_block = dirt_data.spawn();

    let mut chunk = Chunk::new(Some(stone_block));
    for x in 0..Chunk::CHUNK_DIAMETER {
        for y in 0..Chunk::CHUNK_DIAMETER {
            for z in 0..Chunk::CHUNK_DIAMETER {
                let x = x as f32;
                let y = y as f32;
                let z = z as f32;

                let height = ((x / 16.0) * std::f64::consts::PI as f32).sin()
                    * ((z / 16.0) * std::f64::consts::PI as f32).sin()
                    * 16.0;
                if y > height {
                    chunk
                        .set_block_local(BlockLocalCoordinate::new(x as i8, y as i8, z as i8), None)
                        .ok();
                } else if (x % 2f32) == 0f32 {
                    chunk
                        .set_block_local(
                            BlockLocalCoordinate::new(x as i8, y as i8, z as i8),
                            Some(dirt_block),
                        )
                        .ok();
                }
            }
        }
    }

    let terrain_material_handle =
        terrain_material_assets.add(TerrainMaterial::new(&terrain_texture));
    commands.insert_resource(block_registry);
    commands.insert_resource(TerrainMaterialHandle(terrain_material_handle));
    commands.insert_resource(terrain_texture);

    commands.spawn(chunk);
}

pub fn generate_chunk_mesh(
    mut commands: Commands,
    terrain_material: Res<TerrainMaterialHandle>,
    block_registry: Res<BlockRegistry>,
    chunks: Query<(Entity, &Chunk, Without<Handle<TerrainMaterial>>)>,
    mut meshes: ResMut<Assets<Mesh>>,
) {
    // TODO can meshes be generated in parallel? Is there even a perk to doing that?
    for (entity, chunk, _without_material_handle) in chunks.iter() {
        let chunk_mesh = chunk.build_mesh(&block_registry);

        commands.entity(entity).insert(MaterialMeshBundle {
            mesh: meshes.add(chunk_mesh),
            material: terrain_material.0.clone(),
            transform: Transform::from_xyz(0.0, 0.0, 0.0), // TODO let the chunk set that information itself.
            ..Default::default()
        });
    }
}
