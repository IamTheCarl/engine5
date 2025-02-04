use bevy::prelude::*;

use super::{VoxelID, VoxelIndex};

pub type ChunkIndex = IVec3;
type LocalVoxelIndex = UVec3;

const CHUNK_BIT_SHIFT: u32 = 4;
const CHUNK_DIAMETER: i32 = 1 << CHUNK_BIT_SHIFT as i32;
const NUM_BLOCKS: usize =
    CHUNK_DIAMETER as usize * CHUNK_DIAMETER as usize * CHUNK_DIAMETER as usize;

pub fn calculate_block_indexes(coordinate: VoxelIndex) -> (ChunkIndex, LocalVoxelIndex) {
    let chunk_index: ChunkIndex = coordinate >> CHUNK_BIT_SHIFT;
    let local_block_coordinate = coordinate.as_uvec3() & (!0u32 >> (32u32 - CHUNK_BIT_SHIFT));

    (chunk_index, local_block_coordinate)
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

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use crate::map::terrain::TerrainAABB;

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
