use std::{
    collections::HashMap,
    num::{NonZeroU16, NonZeroU64},
};

use bevy::prelude::*;
use serde::{Deserialize, Serialize};

type VoxelID = NonZeroU16;
pub type VoxelCoordinate = IVec3;
type StructureID = NonZeroU64;

type ChunkIndex = IVec3;

const CHUNK_BIT_SHIFT: u32 = 4;
const CHUNK_DIAMETER: i32 = 1 << CHUNK_BIT_SHIFT as i32;
const NUM_BLOCKS: usize =
    CHUNK_DIAMETER as usize * CHUNK_DIAMETER as usize * CHUNK_DIAMETER as usize;

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
}

#[derive(Component, Reflect)]
#[require(TerrainAABB)]
pub struct Structure;

#[derive(Component, Reflect)]
pub struct StructureStorage {
    /// Stores multi-block structures. Structures can be a single block, or
    /// exceed the bounds of a chunk. The chunk that owns the structure is
    /// determined by the A coordinate of the AABB.
    structures: HashMap<TerrainAABB, StructureID>,
}

#[derive(Default, Resource)]
pub struct StructureTracker {
    structures: HashMap<StructureID, Entity>,
}

#[derive(Serialize, Deserialize)]
pub enum UpdateOperation {
    AddVoxels {
        space: TerrainAABB,
        voxels: Vec<VoxelID>,
    },
    AddStructure {
        space: TerrainAABB,
        structure_id: StructureID,
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
}
