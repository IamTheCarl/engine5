use std::{num::NonZeroU16, ops::RangeInclusive};

use bevy::{prelude::*, utils::HashSet};
use chunks::{calculate_block_indexes, ChunkPosition, VoxelStorage};
use itertools::Itertools;
use serde::{Deserialize, Serialize};

mod chunks;

pub use chunks::ChunkIndex;

pub type VoxelID = NonZeroU16;
pub type VoxelIndex = IVec3;

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

pub type TerrainSpaceId = u16;

#[derive(Component, Reflect)]
#[require(Transform)]
pub struct TerrainSpace {
    id: TerrainSpaceId,
}

#[derive(Debug, Reflect, Serialize, Deserialize)]
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
pub struct Structure {
    pub aabb: TerrainAABB,
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
