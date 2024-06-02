use anyhow::{anyhow, bail, Context, Result};
use bevy::{
    prelude::*,
    render::{
        mesh::{Indices, MeshVertexAttribute, VertexAttributeValues},
        render_asset::RenderAssetUsages,
        render_resource::{PrimitiveTopology, VertexFormat},
    },
};
use serde::{de::Visitor, ser::SerializeSeq, Deserialize, Serialize};
use std::{
    borrow::Cow, collections::HashMap, mem::size_of, num::NonZeroU16, ops::Range, str::FromStr,
};

mod meshing;

pub mod storage;
pub use storage::TerrainStorage;

pub mod terrain_space;
pub use terrain_space::{
    LoadAllTerrain, LoadTerrain, LoadsTerrain, TerrainSpace, TerrainSpaceBundle,
};

use self::{meshing::UpdateMesh, terrain_space::ChunkModificationRequestList};

#[derive(Resource)]
pub struct TerrainTime {
    time: usize,
}

fn terrain_time_tick(mut terrain_time: ResMut<TerrainTime>) {
    terrain_time.time += 1;
}

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

impl<'a> BlockTag<'a> {
    pub fn to_static(&self) -> BlockTag<'static> {
        BlockTag {
            namespace: Cow::Owned(self.namespace.clone().into_owned()),
            block_name: Cow::Owned(self.block_name.clone().into_owned()),
        }
    }

    fn get_parts(s: &str) -> Result<(&str, &str)> {
        if s.contains(':') {
            let mut iter = s.split(':');
            let namespace = iter
                .next()
                .filter(|s| !s.is_empty())
                .context("Could not parse block namespace from block tag.")?;

            let block_name = iter
                .next()
                .filter(|s| !s.is_empty())
                .context("Could not parse block name from block tag.")?;

            if iter.next().is_some() {
                bail!("Found too many components in block name.")
            } else {
                Ok((namespace, block_name))
            }
        } else {
            bail!("Could not find separating colon.")
        }
    }
}

impl<'a> std::fmt::Display for BlockTag<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.namespace, self.block_name)
    }
}

impl<'a> FromStr for BlockTag<'a> {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (namespace, block_name) = Self::get_parts(s)?;

        Ok(Self {
            namespace: Cow::Owned(namespace.to_string()),
            block_name: Cow::Owned(block_name.to_string()),
        })
    }
}

impl<'a> TryFrom<&'a str> for BlockTag<'a> {
    type Error = anyhow::Error;

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

    assert!(BlockTag::from_str("").is_err());
    assert!(BlockTag::from_str("game").is_err());

    assert!(BlockTag::from_str(":").is_err());
    assert!(BlockTag::from_str(":block").is_err());
    assert!(BlockTag::from_str("game:").is_err());

    assert!(BlockTag::from_str("game:block:").is_err());
    assert!(BlockTag::from_str("game:block:entity").is_err());
}

#[derive(Resource)]
pub struct BlockRegistry {
    block_data: Vec<BlockData>,
    block_tags: HashMap<BlockTag<'static>, BlockID>,
}

impl BlockRegistry {
    pub fn load() -> Result<Self> {
        let mut registry = Self {
            block_data: Vec::new(),
            block_tags: HashMap::new(),
        };

        // TODO load this data from a file.

        let stone_index = 0;
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

        let dirt_index = 1;
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

        let default_index = 2;
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
    ) -> Result<()> {
        // We need to report an error if an insertion isn't done, so this will stay an error if an insertion doesn't happen.
        let mut block_found = false;
        self.block_tags.entry(tag).or_insert_with(|| {
            block_found = true; // We inserted! That means we can return Ok.

            let _name = name.into();

            let id = self.block_data.len() as u16;
            let id = BlockID::new(id + 1).expect("Block ID miscalculated.");
            self.block_data.push(BlockData { _name, id, faces });
            id
        });

        if block_found {
            Ok(())
        } else {
            Err(anyhow!(
                "A block with this tag already exists in the library."
            ))
        }
    }

    pub fn get_by_tag(&self, tag: &BlockTag) -> Result<&BlockData> {
        let id = self
            .block_tags
            .get(tag)
            .with_context(|| format!("A block with the tag `{}` could not be found.", tag))?;
        self.block_data
            .get(id.get() as usize - 1)
            .with_context(|| format!("A block with the tag `{}` could not be found.", tag))
    }

    pub fn get(&self, block: &Block) -> Option<&BlockData> {
        self.block_data.get(block.id.get() as usize - 1)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Block {
    id: BlockID,
}

#[test]
fn test_size_in_memory() {
    use std::mem::size_of;

    // Strictly verify the size of these things.
    // The size in memory can be very costly if unexpected things happen.

    assert_eq!(size_of::<Block>(), 2);
    assert_eq!(size_of::<Option<Block>>(), 2);

    let chunk_diameter = Chunk::CHUNK_DIAMETER as usize;
    assert_eq!(
        size_of::<Chunk>(),
        2 * chunk_diameter * chunk_diameter * chunk_diameter
    );
}

#[derive(Component, Clone, PartialEq, Eq)]
pub struct Chunk {
    // Blocks are organized as x, z, y.
    blocks: [[[Option<Block>; Self::CHUNK_DIAMETER as usize]; Self::CHUNK_DIAMETER as usize];
        Self::CHUNK_DIAMETER as usize],
}

impl Chunk {
    pub const CHUNK_BIT_SHIFT: u32 = 4;
    pub const CHUNK_DIAMETER: i32 = 1 << Self::CHUNK_BIT_SHIFT as i32;
    pub const NUM_BLOCKS: usize = Self::CHUNK_DIAMETER as usize
        * Self::CHUNK_DIAMETER as usize
        * Self::CHUNK_DIAMETER as usize;

    pub const fn new(block: Option<Block>) -> Self {
        Self {
            blocks: [[[block; Self::CHUNK_DIAMETER as usize]; Self::CHUNK_DIAMETER as usize];
                Self::CHUNK_DIAMETER as usize],
        }
    }

    pub fn deserialize(buffer: &[u8]) -> Self {
        let mut blocks = [[[None; Self::CHUNK_DIAMETER as usize]; Self::CHUNK_DIAMETER as usize];
            Self::CHUNK_DIAMETER as usize];

        let block_iter = blocks
            .iter_mut()
            .flat_map(|array| array.iter_mut())
            .flat_map(|array| array.iter_mut());

        for (block_memory, block_bytes_reference) in
            block_iter.zip(buffer.chunks(size_of::<BlockID>()))
        {
            let mut block_bytes = [0; size_of::<BlockID>()];
            block_bytes.copy_from_slice(block_bytes_reference);
            let block_id = u16::from_le_bytes(block_bytes);

            *block_memory = BlockID::new(block_id).map(|id| Block { id });
        }

        Self { blocks }
    }

    pub fn serialize(&self, buffer: &mut Vec<u8>) {
        for (_position, block) in self.iter() {
            let block_id = match block {
                Some(block_id) => block_id.id.get(),
                None => 0,
            };
            let block_id = block_id.to_le_bytes();

            buffer.extend(block_id.iter());
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
}

impl Serialize for Chunk {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut blocks = serializer.serialize_seq(Some(Self::NUM_BLOCKS))?;

        for (_position, block) in self.iter() {
            let block_id = block.map(|block| block.id.get()).unwrap_or(0);
            blocks.serialize_element(&block_id)?;
        }

        blocks.end()
    }
}

impl<'de> Deserialize<'de> for Chunk {
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct ChunkVisitor;

        impl<'de> Visitor<'de> for ChunkVisitor {
            type Value = Chunk;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("an array of 4096 U16 integers")
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let mut chunk = Chunk {
                    blocks: [[[None; Chunk::CHUNK_DIAMETER as usize];
                        Chunk::CHUNK_DIAMETER as usize];
                        Chunk::CHUNK_DIAMETER as usize],
                };

                for (_position, block) in chunk.iter_mut() {
                    let block_id = seq.next_element::<u16>()?.unwrap_or(0);
                    *block = NonZeroU16::new(block_id).map(|id| Block { id });
                }

                Ok(chunk)
            }
        }

        deserializer.deserialize_seq(ChunkVisitor)
    }
}

#[test]
fn test_serialized_size() {
    // Strictly verify the size of these things once serialized. We can waste a lot of
    // bandwidth and hard drive space if we get careless with these.

    let block = Block {
        id: NonZeroU16::new(1).unwrap(),
    };

    let mut storage = Vec::new();
    ciborium::into_writer(&block, &mut storage).unwrap();
    assert_eq!(storage.len(), 5);

    storage.clear();
    let blocks = [block; 25];
    ciborium::into_writer(&blocks, &mut storage).unwrap();
    assert_eq!(storage.len(), 127);

    // You can conclude from this that storing blocks with cbor is very inefficient.

    // But we make a custom serializer for Chunks that makes it much more efficient.
    let chunk = Chunk::new(Some(block));
    ciborium::into_writer(&chunk, &mut storage).unwrap();
    assert_eq!(storage.len(), 4226);
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

    pub fn to_database_key(&self) -> [u8; 12] {
        let x = self.index.x.to_be_bytes();
        let y = self.index.y.to_be_bytes();
        let z = self.index.z.to_be_bytes();

        [
            x[0], x[1], x[2], x[3], y[0], y[1], y[2], y[3], z[0], z[1], z[2], z[3],
        ]
    }

    pub fn from_database_key(key: [u8; 12]) -> Self {
        let x = [key[0], key[1], key[2], key[3]];
        let y = [key[4], key[5], key[6], key[7]];
        let z = [key[8], key[9], key[10], key[11]];

        let x = i32::from_be_bytes(x);
        let y = i32::from_be_bytes(y);
        let z = i32::from_be_bytes(z);

        Self {
            index: IVec3::new(x, y, z),
        }
    }
}

// It's everything a chunk needs, except the chunk.
#[derive(Bundle)]
struct PreChunkBundle {
    chunk_position: ChunkPosition,
    global_transform: GlobalTransform,
    visibility: Visibility,
    inherited_visibility: InheritedVisibility,
    view_visibility: ViewVisibility,
    modification_request_list: ChunkModificationRequestList,
    update_mesh: UpdateMesh,
}

impl Default for PreChunkBundle {
    fn default() -> Self {
        Self {
            chunk_position: ChunkPosition { index: IVec3::ZERO },
            global_transform: Default::default(),
            visibility: Default::default(),
            inherited_visibility: Default::default(),
            view_visibility: Default::default(),
            modification_request_list: ChunkModificationRequestList::default(),
            update_mesh: UpdateMesh,
        }
    }
}

fn terrain_setup(mut commands: Commands) -> anyhow::Result<()> {
    let block_registry = BlockRegistry::load().unwrap();
    commands.insert_resource(block_registry);

    Ok(())
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct TerrainPlugin;

impl Plugin for TerrainPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Startup,
            (
                terrain_setup
                    .pipe(crate::error_handler)
                    .in_set(TerrainPlugin),
                apply_deferred.after(terrain_setup),
            ),
        );

        app.configure_sets(Update, TerrainPlugin);
        app.add_systems(FixedUpdate, terrain_time_tick);

        meshing::register(app);
        storage::register_terrain_files(app);
        terrain_space::register_terrain_space(app);
    }
}
