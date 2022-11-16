use bevy::{
    prelude::*,
    render::{mesh::Indices, render_resource::PrimitiveTopology},
};
use std::{borrow::Cow, collections::HashMap, num::NonZeroU16, str::FromStr};
use thiserror::Error;

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
    top: Handle<Image>,
    bottom: Handle<Image>,
    north: Handle<Image>,
    south: Handle<Image>,
    east: Handle<Image>,
    west: Handle<Image>,
}

impl BlockFaces {
    fn get_face_texture(&self, direction: BlockDirection) -> &Handle<Image> {
        match direction {
            BlockDirection::Up => &self.top,
            BlockDirection::Down => &self.bottom,
            BlockDirection::North => &self.north,
            BlockDirection::South => &self.south,
            BlockDirection::East => &self.east,
            BlockDirection::West => &self.west,
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

impl<'a> BlockNeighborSet {
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
    faces: Option<BlockFaces>,
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

pub struct BlockRegistry {
    block_data: Vec<BlockData>,
    block_tags: HashMap<BlockTag<'static>, BlockID>,
}

impl BlockRegistry {
    pub fn load() -> Result<Self, BlockTagError<'static>> {
        // Self {
        //     block_data: vec![BlockData {
        //         name: String::from("Stone"),
        //         faces: None,
        //     }],
        //     block_tags: HashMap::from([("core:stone".parse().unwrap(), 0)]),
        // }

        let mut registry = Self {
            block_data: Vec::new(),
            block_tags: HashMap::new(),
        };

        registry.add_block("core:stone".try_into()?, "Stone", None)?;

        Ok(registry)
    }

    fn add_block(
        &mut self,
        tag: BlockTag<'static>,
        name: impl Into<String>,
        faces: Option<BlockFaces>,
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
        self.block_data.get(block.id.get() as usize)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Block {
    id: BlockID,
}

impl Block {
    fn insert_face(
        direction: BlockDirection,
        offset: &[f32],
        vertex_buffer: &mut Vec<[f32; 3]>,
        normals: &mut Vec<[f32; 3]>,
        uvs: &mut Vec<[f32; 2]>,
        indices: &mut Vec<u32>,
    ) {
        let source_vertices = &[
            // Top
            [1.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
            [0.0, 1.0, 1.0],
            [1.0, 1.0, 1.0],
            // Bottom
            [1.0, 0.0, 1.0],
            [0.0, 0.0, 1.0],
            [0.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            // East
            [1.0, 0.0, 0.0],
            [1.0, 1.0, 0.0],
            [1.0, 1.0, 1.0],
            [1.0, 0.0, 1.0],
            // West
            [0.0, 0.0, 1.0],
            [0.0, 1.0, 1.0],
            [0.0, 1.0, 0.0],
            [0.0, 0.0, 0.0],
            // North
            [0.0, 0.0, 1.0],
            [1.0, 0.0, 1.0],
            [1.0, 1.0, 1.0],
            [0.0, 1.0, 1.0],
            // South
            [0.0, 1.0, 0.0],
            [1.0, 1.0, 0.0],
            [1.0, 0.0, 0.0],
            [0.0, 0.0, 0.0],
        ];

        let source_normals = &[
            // Top
            [0.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
            [0.0, 1.0, 0.0],
            // Bottom
            [0.0, -1.0, 0.0],
            [0.0, -1.0, 0.0],
            [0.0, -1.0, 0.0],
            [0.0, -1.0, 0.0],
            // East
            [1.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            [1.0, 0.0, 0.0],
            // West
            [-1.0, 0.0, 0.0],
            [-1.0, 0.0, 0.0],
            [-1.0, 0.0, 0.0],
            [-1.0, 0.0, 0.0],
            // North
            [0.0, 0.0, 1.0],
            [0.0, 0.0, 1.0],
            [0.0, 0.0, 1.0],
            [0.0, 0.0, 1.0],
            // South
            [0.0, 0.0, -1.0],
            [0.0, 0.0, -1.0],
            [0.0, 0.0, -1.0],
            [0.0, 0.0, -1.0],
        ];

        let source_uv = [
            // Top
            [0.0, 0.0],
            [1.0, 0.0],
            [1.0, 1.0],
            [0.0, 1.0],
            // Bottom
            [0.0, 0.0],
            [1.0, 0.0],
            [1.0, 1.0],
            [0.0, 1.0],
            // East
            [0.0, 0.0],
            [1.0, 0.0],
            [1.0, 1.0],
            [0.0, 1.0],
            // West
            [0.0, 0.0],
            [1.0, 0.0],
            [1.0, 1.0],
            [0.0, 1.0],
            // North
            [0.0, 0.0],
            [1.0, 0.0],
            [1.0, 1.0],
            [0.0, 1.0],
            // South
            [0.0, 0.0],
            [1.0, 0.0],
            [1.0, 1.0],
            [0.0, 1.0],
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
            .map(|vec| [vec[0] + offset[0], vec[1] + offset[1], vec[2] + offset[2]]);
        vertex_buffer.extend(vertex_iter);

        let normal_iter = source_normals[range.clone()].iter();
        normals.extend(normal_iter);

        let uv_iter = source_uv[range].iter();
        uvs.extend(uv_iter);

        indices.extend_from_slice(&[
            starting_index,
            starting_index + 1,
            starting_index + 2,
            starting_index + 2,
            starting_index + 3,
            starting_index,
        ]);
        // }
    }

    fn add_to_mesh(
        &self,
        neighbors: BlockNeighborSet,
        offset: &[f32],
        vertex_buffer: &mut Vec<[f32; 3]>,
        normals: &mut Vec<[f32; 3]>,
        uvs: &mut Vec<[f32; 2]>,
        indices: &mut Vec<u32>,
    ) {
        for direction in BlockDirection::ALL {
            let neighbor = neighbors.get(direction);

            // We have to draw this face.
            if neighbor.is_none() {
                Self::insert_face(direction, offset, vertex_buffer, normals, uvs, indices);
            }
        }
    }
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

    pub fn build_mesh(&self) -> Mesh {
        let mut vertex_buffer = Vec::new();
        let mut normals = Vec::new();
        let mut uvs = Vec::new();
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
                            neighbors,
                            &[x_offset as f32, y_offset as f32, z_offset as f32],
                            &mut vertex_buffer,
                            &mut normals,
                            &mut uvs,
                            &mut indices,
                        );
                    }
                }
            }
        }

        let mut mesh = Mesh::new(PrimitiveTopology::TriangleList);

        mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, vertex_buffer);
        mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
        // mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
        mesh.set_indices(Some(Indices::U32(indices)));

        mesh
    }
}
