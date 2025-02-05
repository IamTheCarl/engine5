use std::{
    collections::HashMap,
    num::{NonZeroU16, NonZeroU32},
    ops::RangeInclusive,
    sync::atomic::{AtomicU32, Ordering},
};

use bevy::{ecs::removal_detection::RemovedComponentEntity, prelude::*};
use chunks::{calculate_block_indexes, ChunkPosition, VoxelStorage};
use itertools::Itertools;
use serde::{Deserialize, Serialize};

mod chunks;

pub use chunks::ChunkIndex;

use super::storage::PersistantId;

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

        app.insert_resource(TerrainTime::default());
        app.insert_resource(TerrainSpaceTracker::default());
        app.insert_resource(ChunkTracker::default());
        app.add_systems(
            Update,
            (
                TerrainTime::tick,
                TerrainSpaceTracker::insert,
                TerrainSpaceTracker::remove,
                add_chunk_to_space.after(TerrainSpaceTracker::insert),
                remove_chunk_from_space.before(TerrainSpaceTracker::remove),
            ),
        );

        app.add_event::<UpdateComplete>();
    }
}

pub type TerrainSpaceId = u16;
pub type UpdateId = NonZeroU32;

#[derive(Component, Reflect)]
#[require(Transform, UpdateQueue)]
pub struct TerrainSpace {
    id: TerrainSpaceId,
    chunks: HashMap<ChunkIndex, Entity>,
}

/// An arbitrary time tracker that determines when and what order terrain should be
/// freed from memory. It is tied to framerate, so it should not be used to drive animation
/// or physics.
#[derive(Resource, Default)]
struct TerrainTime {
    time: usize,
}

impl TerrainTime {
    fn tick(mut terrain_time: ResMut<Self>) {
        terrain_time.time += 1;
    }
}

#[derive(Component, Default)]
pub struct UpdateQueue {
    operations: Vec<Update>,
}

impl UpdateQueue {
    pub fn push(&mut self, update: Update) {
        self.operations.push(update);
    }
}

#[derive(Debug, Event)]
pub struct UpdateComplete {
    /// Id of the operation that has been completed.
    pub id: UpdateId,
}

#[derive(Debug)]
pub struct Update {
    /// If set, this update will kick back an "UpdateComplete" event with the set ID.
    id: Option<UpdateId>,

    /// The operation to be done with this update.
    operation: UpdateOperation,
}

impl Update {
    pub fn new(operation: UpdateOperation) -> Self {
        Self {
            id: None,
            operation,
        }
    }

    pub fn new_notifying(&self, operation: UpdateOperation) -> (Self, UpdateId) {
        static NEXT_UPDATE_ID: AtomicU32 = AtomicU32::new(1);

        let id = UpdateId::new(NEXT_UPDATE_ID.fetch_add(1, Ordering::SeqCst))
            .unwrap_or(UpdateId::new(1).unwrap()); // If it's zero, that means we wrapped
                                                   // around and need to restart our
                                                   // count.

        (
            Self {
                id: Some(id),
                operation,
            },
            id,
        )
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum UpdateOperation {
    FillVoxels {
        space: TerrainAABB,
        voxel: Option<VoxelID>,
    },
    InsertStructure {
        space: TerrainAABB,
        persistant_entity_id: PersistantId,
    },
}

#[derive(Debug, Reflect, Serialize, Deserialize)]
pub struct TerrainAABB {
    pub a: VoxelIndex,
    pub b: VoxelIndex,
}

fn apply_updates(
    spaces: Query<(&Children, &mut UpdateQueue), With<TerrainSpace>>,
    chunks: Query<(&ChunkPosition, &VoxelStorage), With<Parent>>,
) {
}

#[derive(Resource, Default)]
struct TerrainSpaceTracker {
    spaces_to_entities: HashMap<TerrainSpaceId, Entity>,
    entities_to_spaces: HashMap<Entity, TerrainSpaceId>,
}

impl TerrainSpaceTracker {
    fn insert(
        added: Query<(Entity, &TerrainSpace), Added<TerrainSpace>>,
        mut tracker: ResMut<Self>,
    ) {
        for (space_entity_id, space) in added.iter() {
            let old = tracker.spaces_to_entities.insert(space.id, space_entity_id);
            assert!(old.is_none(), "Space was already being tracked.");

            let old = tracker.entities_to_spaces.insert(space_entity_id, space.id);
            assert!(old.is_none(), "Space was already being tracked.");
        }
    }

    fn remove(mut removed: RemovedComponents<TerrainSpace>, mut tracker: ResMut<Self>) {
        for removed in removed.read() {
            let terrain_space_id = tracker
                .entities_to_spaces
                .remove(&removed)
                .expect("Space was not being tracked.");

            tracker
                .spaces_to_entities
                .remove(&terrain_space_id)
                .expect("Space was not being tracked.");
        }
    }
}

#[derive(Resource, Default)]
struct ChunkTracker {
    chunks: HashMap<Entity, (TerrainSpaceId, ChunkIndex)>,
}

fn add_chunk_to_space(
    added: Query<(Entity, &ChunkPosition, &Parent), Added<ChunkPosition>>,
    mut spaces: Query<&mut TerrainSpace>,
    mut tracker: ResMut<ChunkTracker>,
) {
    for (added_entity_id, added_position, added_parent) in added.iter() {
        let mut space = spaces
            .get_mut(added_parent.get())
            .expect("Chunk parent was not a terrain space");

        let old = space.chunks.insert(added_position.index, added_entity_id);
        assert!(old.is_none(), "A chunk has been overwritten.");

        let old = tracker
            .chunks
            .insert(added_entity_id, (space.id, added_position.index));
        assert!(old.is_none(), "This chunk was already being tracked");
    }
}

fn remove_chunk_from_space(
    mut removed: RemovedComponents<ChunkPosition>,
    mut spaces: Query<&mut TerrainSpace>,
    mut chunk_tracker: ResMut<ChunkTracker>,
    space_tracker: Res<TerrainSpaceTracker>,
) {
    for removed in removed.read() {
        let (terrain_space_id, chunk_index) = chunk_tracker
            .chunks
            .remove(&removed)
            .expect("Removed a chunk that was never being tracked.");

        // It is entirely possible that a terrain space may have been removed with the chunk, so
        // it's not too unexpected if this fails.
        if let Some(terrain_space_entity_id) =
            space_tracker.spaces_to_entities.get(&terrain_space_id)
        {
            // Okay, this chunk exists. This shouldn't ever fail.
            let mut terrain_space = spaces
                .get_mut(*terrain_space_entity_id)
                .expect("Terrain space was not being tracked");

            let old = terrain_space.chunks.remove(&chunk_index);
            assert!(old.is_none(), "Terrain space was not tracking its chunk");
        }
    }
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

#[cfg(test)]
mod test {
    use crate::map::MapPlugin;

    use super::*;

    // TODO test that chunks are added to the terrain space when spawned.
    // TODO test that chunks are removed from the terrain space when despawned.
    // TODO test what happens if the whole terrain space is removed. All the child chunks should
    // also be removed and no error should happen if the child chunks fail to find the space on
    // their removal. Those children should also be removed from the chunk tracker.
    // TODO track terrain spaces.

    #[test]
    fn track_terrain_spaces() {
        let mut app = App::new();
        app.add_plugins((MapPlugin,));

        let space_id = 12;

        let space_entity = {
            let world = app.world_mut();
            world
                .spawn(TerrainSpace {
                    id: space_id,
                    chunks: HashMap::new(),
                })
                .id()
        };

        assert_eq!(
            app.world()
                .resource::<TerrainSpaceTracker>()
                .entities_to_spaces,
            HashMap::from([])
        );
        assert_eq!(
            app.world()
                .resource::<TerrainSpaceTracker>()
                .spaces_to_entities,
            HashMap::from([])
        );

        app.update();

        assert_eq!(
            app.world()
                .resource::<TerrainSpaceTracker>()
                .entities_to_spaces,
            HashMap::from([(space_entity, space_id)])
        );
        assert_eq!(
            app.world()
                .resource::<TerrainSpaceTracker>()
                .spaces_to_entities,
            HashMap::from([(space_id, space_entity)])
        );

        {
            let world = app.world_mut();
            world.despawn(space_entity);
        }

        app.update();

        assert_eq!(
            app.world()
                .resource::<TerrainSpaceTracker>()
                .entities_to_spaces,
            HashMap::from([])
        );
        assert_eq!(
            app.world()
                .resource::<TerrainSpaceTracker>()
                .spaces_to_entities,
            HashMap::from([])
        );
    }
}
