use std::{
    collections::HashMap,
    num::{NonZeroU16, NonZeroU64},
};

use bevy::prelude::*;
use serde::{Deserialize, Serialize};

use super::storage::PersistantId;

type VoxelID = NonZeroU16;
pub type VoxelCoordinate = IVec3;
type ChunkIndex = IVec3;
type LocalVoxelCoordinate = UVec3;

const CHUNK_BIT_SHIFT: u32 = 4;
const CHUNK_DIAMETER: i32 = 1 << CHUNK_BIT_SHIFT as i32;
const NUM_BLOCKS: usize =
    CHUNK_DIAMETER as usize * CHUNK_DIAMETER as usize * CHUNK_DIAMETER as usize;

fn calculate_block_indexes(coordinate: VoxelCoordinate) -> (ChunkIndex, LocalVoxelCoordinate) {
    let chunk_index: ChunkIndex = coordinate >> CHUNK_BIT_SHIFT;
    let local_block_coordinate = coordinate.as_uvec3() & (!0u32 >> (32u32 - CHUNK_BIT_SHIFT));

    (chunk_index, local_block_coordinate)
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct TerrainPlugin;

impl Plugin for TerrainPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<ChunkPosition>();
        app.register_type::<VoxelStorage>();
        app.register_type::<TerrainAABB>();
        app.register_type::<Structure>();
        app.register_type::<StructureStorage>();
    }
}

#[derive(Component, Default, Reflect)]
pub struct ChunkPosition {
    pub index: ChunkIndex,
}

#[derive(Component, Reflect)]
#[require(ChunkPosition)]
pub struct VoxelStorage {
    blocks: [[[Option<VoxelID>; CHUNK_DIAMETER as usize]; CHUNK_DIAMETER as usize];
        CHUNK_DIAMETER as usize],
}

#[derive(
    Component, Default, Clone, Debug, PartialEq, Eq, Hash, Reflect, Serialize, Deserialize,
)]
pub struct TerrainAABB {
    pub a: VoxelCoordinate,
    pub b: VoxelCoordinate,
}

impl TerrainAABB {
    const KEY_LENGTH: usize = 4 * 6;
    pub fn storage_key(&self) -> [u8; Self::KEY_LENGTH] {
        let mut key = [0; Self::KEY_LENGTH];

        let ax = self.a.x.to_le_bytes();
        let ay = self.a.y.to_le_bytes();
        let az = self.a.z.to_le_bytes();
        let bx = self.b.x.to_le_bytes();
        let by = self.b.y.to_le_bytes();
        let bz = self.b.z.to_le_bytes();

        key[0..=3].copy_from_slice(&ax);
        key[4..=7].copy_from_slice(&ay);
        key[8..=11].copy_from_slice(&az);
        key[12..=15].copy_from_slice(&bx);
        key[16..=19].copy_from_slice(&by);
        key[20..=23].copy_from_slice(&bz);

        key
    }

    // pub fn intersected_chunks(&self) -> impl Iterator<Item = ChunkIndex> {
    //     let (chunk_index_a, _local_voxel_coordinate) = calculate_block_indexes(self.a);
    //     let (chunk_index_b, _local_voxel_coordinate) = calculate_block_indexes(self.b);

    //     todo!()
    // }
}

#[derive(Component, Reflect)]
#[require(TerrainAABB)]
pub struct Structure;

#[derive(Component, Reflect)]
pub struct StructureStorage {
    /// Stores references to multi-block structures.
    structures: HashMap<TerrainAABB, PersistantId>,
}

#[derive(Serialize, Deserialize)]
pub enum UpdateOperation {
    FillVoxels {
        space: TerrainAABB,
        voxel: Option<VoxelID>,
    },
}

pub struct TerrainUpdate {
    operations: Vec<UpdateOperation>,
}

impl TerrainUpdate {
    pub fn push_operation(&mut self, operation: UpdateOperation) {
        self.operations.push(operation);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn terrain_aabb_key() {
        let aabb = TerrainAABB {
            a: VoxelCoordinate::new(0x00_00_00_00, 0x11_11_11_11, 0x22_22_22_22),
            b: VoxelCoordinate::new(0x33_33_33_33, 0x44_44_44_44, 0x55_55_55_55),
        };

        assert_eq!(
            aabb.storage_key(),
            [
                0x00, 0x00, 0x00, 0x00, 0x11, 0x11, 0x11, 0x11, 0x22, 0x22, 0x22, 0x22, 0x33, 0x33,
                0x33, 0x33, 0x44, 0x44, 0x44, 0x44, 0x55, 0x55, 0x55, 0x55
            ]
        );
    }

    #[test]
    fn block_index_calculation() {
        let (chunk_index, local_block_coordinate) =
            calculate_block_indexes(VoxelCoordinate::new(0, 0, 0));
        assert_eq!(chunk_index, ChunkIndex::new(0, 0, 0));
        assert_eq!(local_block_coordinate, LocalVoxelCoordinate::new(0, 0, 0));

        let (chunk_index, local_block_coordinate) =
            calculate_block_indexes(VoxelCoordinate::new(CHUNK_DIAMETER, 0, 0));
        assert_eq!(chunk_index, ChunkIndex::new(1, 0, 0));
        assert_eq!(local_block_coordinate, LocalVoxelCoordinate::new(0, 0, 0));

        let (chunk_index, local_block_coordinate) =
            calculate_block_indexes(VoxelCoordinate::new(-CHUNK_DIAMETER, 0, 0));
        assert_eq!(chunk_index, ChunkIndex::new(-1, 0, 0));
        assert_eq!(local_block_coordinate, LocalVoxelCoordinate::new(0, 0, 0));

        let (chunk_index, local_block_coordinate) =
            calculate_block_indexes(VoxelCoordinate::new(-CHUNK_DIAMETER + 1, 0, 0));
        assert_eq!(chunk_index, ChunkIndex::new(-1, 0, 0));
        assert_eq!(local_block_coordinate, LocalVoxelCoordinate::new(1, 0, 0));

        let (chunk_index, local_block_coordinate) =
            calculate_block_indexes(VoxelCoordinate::new(-CHUNK_DIAMETER - 1, 0, 0));
        assert_eq!(chunk_index, ChunkIndex::new(-2, 0, 0));
        assert_eq!(
            local_block_coordinate,
            LocalVoxelCoordinate::new(CHUNK_DIAMETER as u32 - 1, 0, 0)
        );
    }
}
