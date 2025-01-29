use std::{num::NonZeroU16, ops::RangeInclusive};

use bevy::{prelude::*, utils::HashSet};
use itertools::Itertools;
use serde::{Deserialize, Serialize};

use super::storage::PersistantId;

type VoxelID = NonZeroU16;
pub type VoxelIndex = IVec3;
type ChunkIndex = IVec3;
type LocalVoxelIndex = UVec3;

const CHUNK_BIT_SHIFT: u32 = 4;
const CHUNK_DIAMETER: i32 = 1 << CHUNK_BIT_SHIFT as i32;
const NUM_BLOCKS: usize =
    CHUNK_DIAMETER as usize * CHUNK_DIAMETER as usize * CHUNK_DIAMETER as usize;

fn calculate_block_indexes(coordinate: VoxelIndex) -> (ChunkIndex, LocalVoxelIndex) {
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
    pub a: VoxelIndex,
    pub b: VoxelIndex,
}

impl TerrainAABB {
    pub fn intersected_chunks(&self) -> impl Iterator<Item = ChunkIndex> {
        let (chunk_index_a, _local_voxel_coordinate) = calculate_block_indexes(self.a);
        let (chunk_index_b, _local_voxel_coordinate) = calculate_block_indexes(self.b);

        fn axis_range<T: Ord + Copy>(a: T, b: T) -> RangeInclusive<T> {
            let min = a.min(b);
            let max = a.max(b);

            min..=max
        }

        let x_range = axis_range(chunk_index_a.x, chunk_index_b.x);
        let y_range = axis_range(chunk_index_a.y, chunk_index_b.y);
        let z_range = axis_range(chunk_index_a.z, chunk_index_b.z);

        x_range
            .cartesian_product(y_range)
            .cartesian_product(z_range)
            .map(|((x, y), z)| ChunkIndex::new(x, y, z))
    }
}

#[derive(Component, Reflect)]
#[require(TerrainAABB)]
pub struct Structure;

#[derive(Component, Serialize, Deserialize)]
pub struct StructureStorage {
    /// Stores references to multi-block structures.
    structures: HashSet<PersistantId>,
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
    fn aabb_chunk_coverage() {
        assert_eq!(
            TerrainAABB {
                a: VoxelIndex::new(0, 0, 0),
                b: VoxelIndex::new(0, 0, 0)
            }
            .intersected_chunks()
            .collect::<HashSet<_>>(),
            HashSet::from([ChunkIndex::new(0, 0, 0)])
        );

        assert_eq!(
            TerrainAABB {
                a: VoxelIndex::new(0, 0, 0),
                b: VoxelIndex::new(CHUNK_DIAMETER, CHUNK_DIAMETER, CHUNK_DIAMETER)
            }
            .intersected_chunks()
            .collect::<HashSet<_>>(),
            HashSet::from([
                ChunkIndex::new(0, 0, 0),
                ChunkIndex::new(0, 0, 1),
                ChunkIndex::new(0, 1, 0),
                ChunkIndex::new(0, 1, 1),
                ChunkIndex::new(1, 0, 0),
                ChunkIndex::new(1, 0, 1),
                ChunkIndex::new(1, 1, 0),
                ChunkIndex::new(1, 1, 1)
            ])
        );

        assert_eq!(
            TerrainAABB {
                a: VoxelIndex::new(CHUNK_DIAMETER, CHUNK_DIAMETER, CHUNK_DIAMETER),
                b: VoxelIndex::new(0, 0, 0)
            }
            .intersected_chunks()
            .collect::<HashSet<_>>(),
            HashSet::from([
                ChunkIndex::new(0, 0, 0),
                ChunkIndex::new(0, 0, 1),
                ChunkIndex::new(0, 1, 0),
                ChunkIndex::new(0, 1, 1),
                ChunkIndex::new(1, 0, 0),
                ChunkIndex::new(1, 0, 1),
                ChunkIndex::new(1, 1, 0),
                ChunkIndex::new(1, 1, 1)
            ])
        );

        assert_eq!(
            TerrainAABB {
                a: VoxelIndex::new(0, 0, 0),
                b: VoxelIndex::new(-CHUNK_DIAMETER, -CHUNK_DIAMETER, -CHUNK_DIAMETER)
            }
            .intersected_chunks()
            .collect::<HashSet<_>>(),
            HashSet::from([
                ChunkIndex::new(0, 0, 0),
                ChunkIndex::new(0, 0, -1),
                ChunkIndex::new(0, -1, 0),
                ChunkIndex::new(0, -1, -1),
                ChunkIndex::new(-1, 0, 0),
                ChunkIndex::new(-1, 0, -1),
                ChunkIndex::new(-1, -1, 0),
                ChunkIndex::new(-1, -1, -1)
            ])
        );
        assert_eq!(
            TerrainAABB {
                a: VoxelIndex::new(CHUNK_DIAMETER, CHUNK_DIAMETER, CHUNK_DIAMETER),
                b: VoxelIndex::new(-CHUNK_DIAMETER, -CHUNK_DIAMETER, -CHUNK_DIAMETER)
            }
            .intersected_chunks()
            .collect::<HashSet<_>>(),
            HashSet::from([
                ChunkIndex::new(-1, -1, -1),
                ChunkIndex::new(-1, -1, 0),
                ChunkIndex::new(-1, -1, 1),
                ChunkIndex::new(-1, 0, -1),
                ChunkIndex::new(-1, 0, 0),
                ChunkIndex::new(-1, 0, 1),
                ChunkIndex::new(-1, 1, -1),
                ChunkIndex::new(-1, 1, 0),
                ChunkIndex::new(-1, 1, 1),
                ChunkIndex::new(0, -1, -1),
                ChunkIndex::new(0, -1, 0),
                ChunkIndex::new(0, -1, 1),
                ChunkIndex::new(0, 0, -1),
                ChunkIndex::new(0, 0, 0),
                ChunkIndex::new(0, 0, 1),
                ChunkIndex::new(0, 1, -1),
                ChunkIndex::new(0, 1, 0),
                ChunkIndex::new(0, 1, 1),
                ChunkIndex::new(1, -1, -1),
                ChunkIndex::new(1, -1, 0),
                ChunkIndex::new(1, -1, 1),
                ChunkIndex::new(1, 0, -1),
                ChunkIndex::new(1, 0, 0),
                ChunkIndex::new(1, 0, 1),
                ChunkIndex::new(1, 1, -1),
                ChunkIndex::new(1, 1, 0),
                ChunkIndex::new(1, 1, 1),
            ])
        );
    }

    #[test]
    fn block_index_calculation() {
        let (chunk_index, local_block_coordinate) =
            calculate_block_indexes(VoxelIndex::new(0, 0, 0));
        assert_eq!(chunk_index, ChunkIndex::new(0, 0, 0));
        assert_eq!(local_block_coordinate, LocalVoxelIndex::new(0, 0, 0));

        let (chunk_index, local_block_coordinate) =
            calculate_block_indexes(VoxelIndex::new(CHUNK_DIAMETER, 0, 0));
        assert_eq!(chunk_index, ChunkIndex::new(1, 0, 0));
        assert_eq!(local_block_coordinate, LocalVoxelIndex::new(0, 0, 0));

        let (chunk_index, local_block_coordinate) =
            calculate_block_indexes(VoxelIndex::new(-CHUNK_DIAMETER, 0, 0));
        assert_eq!(chunk_index, ChunkIndex::new(-1, 0, 0));
        assert_eq!(local_block_coordinate, LocalVoxelIndex::new(0, 0, 0));

        let (chunk_index, local_block_coordinate) =
            calculate_block_indexes(VoxelIndex::new(-CHUNK_DIAMETER + 1, 0, 0));
        assert_eq!(chunk_index, ChunkIndex::new(-1, 0, 0));
        assert_eq!(local_block_coordinate, LocalVoxelIndex::new(1, 0, 0));

        let (chunk_index, local_block_coordinate) =
            calculate_block_indexes(VoxelIndex::new(-CHUNK_DIAMETER - 1, 0, 0));
        assert_eq!(chunk_index, ChunkIndex::new(-2, 0, 0));
        assert_eq!(
            local_block_coordinate,
            LocalVoxelIndex::new(CHUNK_DIAMETER as u32 - 1, 0, 0)
        );
    }
}
