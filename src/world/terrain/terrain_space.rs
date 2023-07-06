use super::{
    storage::{TerrainStorage, ToLoadTerrain, ToSaveTerrain},
    Block, Chunk, ChunkIndex, ChunkPosition, GlobalBlockCoordinate, LocalBlockCoordinate,
    PreChunkBundle, TerrainTime,
};
use crate::world::{
    physics::Position,
    spatial_entities::storage::{Storable, ToLoadSpatial},
};
use bevy::{
    ecs::query::{ReadOnlyWorldQuery, WorldQuery},
    prelude::*,
};
use std::collections::{HashMap, HashSet};

pub const CHUNK_TIME_TO_LIVE_TICKS: usize = 30;

#[derive(Component, Reflect, Default)]
#[reflect(Component)]
pub struct LoadsTerrain {
    pub radius: i32,
}

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct LoadAllTerrain;

#[derive(Component)]
struct ActiveTerrainTimer {
    /// How many ticks are we to keep it alive for.
    deadline: usize,
}

#[derive(Component, Default)]
pub struct TerrainSpace {
    loaded_terrain: HashMap<ChunkIndex, Entity>,
    pub(crate) non_empty_chunks: HashSet<Entity>,
}

impl TerrainSpace {
    pub fn num_non_empty_chunks(&self) -> usize {
        self.non_empty_chunks.len()
    }

    pub fn iter_loaded_chunks(&self) -> impl Iterator<Item = &Entity> {
        self.loaded_terrain.values()
    }

    fn load_chunk(
        &mut self,
        commands: &mut Commands,
        space_entity: Entity,
        chunk_position: ChunkPosition,
        despawn_deadline: Option<usize>,
    ) -> bool {
        match self.loaded_terrain.entry(chunk_position.index) {
            std::collections::hash_map::Entry::Occupied(already_loaded) => {
                // Oh, already loaded? Just update the timer then.
                if let Some(despawn_deadline) = despawn_deadline {
                    commands
                        .entity(*already_loaded.get())
                        .insert(ActiveTerrainTimer {
                            deadline: despawn_deadline,
                        });
                }

                false
            }
            std::collections::hash_map::Entry::Vacant(to_load) => {
                let mut entity = commands.spawn((
                    PreChunkBundle {
                        chunk_position,
                        ..Default::default()
                    },
                    ToLoadTerrain, // Mark that this terrain needs to be loaded.
                    ToLoadSpatial,
                ));
                entity.set_parent(space_entity);

                if let Some(despawn_deadline) = despawn_deadline {
                    entity.insert(ActiveTerrainTimer {
                        deadline: despawn_deadline,
                    });
                }

                let entity = entity.id();

                // We add as children so we can despawn recursively.
                to_load.insert(entity);

                // Indicate that we're going to load this.
                true
            }
        }
    }

    fn calculate_block_indexes(
        coordinate: GlobalBlockCoordinate,
    ) -> (ChunkIndex, LocalBlockCoordinate) {
        let chunk_index: ChunkIndex = coordinate >> Chunk::CHUNK_BIT_SHIFT;
        let local_block_coordinate =
            coordinate.as_uvec3() & (!0u32 >> (32u32 - Chunk::CHUNK_BIT_SHIFT));

        (chunk_index, local_block_coordinate.as_ivec3())
    }

    pub fn get_block<'a, Q: WorldQuery, F: ReadOnlyWorldQuery>(
        &self,
        query: &'a Query<Q, F>,
        query_wrapper: impl FnOnce(&'a Query<Q, F>, Entity) -> Option<&'a Chunk>,
        coordinate: GlobalBlockCoordinate,
    ) -> Option<Block> {
        let (chunk_index, local_block_coordinate) = Self::calculate_block_indexes(coordinate);

        let chunk_entity = self.loaded_terrain.get(&chunk_index)?;
        let chunk = query_wrapper(query, *chunk_entity)?;
        chunk.get_block_local(local_block_coordinate)
    }

    pub fn get_block_mut<'a, Q: WorldQuery, F: ReadOnlyWorldQuery>(
        &self,
        query: &'a mut Query<Q, F>,
        query_wrapper: impl FnOnce(&'a mut Query<Q, F>, Entity) -> Option<Mut<'a, Chunk>>,
        coordinate: GlobalBlockCoordinate,
        callback: impl FnOnce(Option<(Entity, &mut Option<Block>)>),
    ) {
        let (chunk_index, local_block_coordinate) = Self::calculate_block_indexes(coordinate);

        if let Some(chunk_entity) = self.loaded_terrain.get(&chunk_index) {
            if let Some(mut chunk) = query_wrapper(query, *chunk_entity) {
                if let Some(block) = chunk.get_block_local_mut(local_block_coordinate) {
                    callback(Some((*chunk_entity, block)))
                } else {
                    callback(None)
                }
            } else {
                callback(None)
            }
        } else {
            callback(None)
        }
    }
}

#[test]
fn block_index_calculation() {
    let (chunk_index, local_block_coordinate) =
        TerrainSpace::calculate_block_indexes(GlobalBlockCoordinate::new(0, 0, 0));
    assert_eq!(chunk_index, IVec3::new(0, 0, 0));
    assert_eq!(local_block_coordinate, IVec3::new(0, 0, 0));

    let (chunk_index, local_block_coordinate) = TerrainSpace::calculate_block_indexes(
        GlobalBlockCoordinate::new(Chunk::CHUNK_DIAMETER, 0, 0),
    );
    assert_eq!(chunk_index, IVec3::new(1, 0, 0));
    assert_eq!(local_block_coordinate, IVec3::new(0, 0, 0));

    let (chunk_index, local_block_coordinate) = TerrainSpace::calculate_block_indexes(
        GlobalBlockCoordinate::new(-Chunk::CHUNK_DIAMETER, 0, 0),
    );
    assert_eq!(chunk_index, IVec3::new(-1, 0, 0));
    assert_eq!(local_block_coordinate, IVec3::new(0, 0, 0));

    let (chunk_index, local_block_coordinate) = TerrainSpace::calculate_block_indexes(
        GlobalBlockCoordinate::new(-Chunk::CHUNK_DIAMETER + 1, 0, 0),
    );
    assert_eq!(chunk_index, IVec3::new(-1, 0, 0));
    assert_eq!(local_block_coordinate, IVec3::new(1, 0, 0));

    let (chunk_index, local_block_coordinate) = TerrainSpace::calculate_block_indexes(
        GlobalBlockCoordinate::new(-Chunk::CHUNK_DIAMETER - 1, 0, 0),
    );
    assert_eq!(chunk_index, IVec3::new(-2, 0, 0));
    assert_eq!(
        local_block_coordinate,
        IVec3::new(Chunk::CHUNK_DIAMETER - 1, 0, 0)
    );
}

#[derive(Bundle)]
pub struct TerrainSpaceBundle {
    pub terrain_space: TerrainSpace,
    pub position: Position,
    pub file: TerrainStorage,
    // pub storable: Storable, // FIXME we can't store this yet with the current system. Implement that later.
    pub transform: Transform,
    pub global_transform: GlobalTransform,
    pub visibility: Visibility,
    pub computed_visibility: ComputedVisibility,
}

fn load_all_terrain(
    mut commands: Commands,
    mut spaces: Query<(
        Entity,
        &mut TerrainSpace,
        &TerrainStorage,
        With<LoadAllTerrain>,
    )>,
) {
    for (space_entity, mut space, terrain_file, _keep_all_terrain_loaded) in spaces.iter_mut() {
        for position in terrain_file.iter_chunk_indexes() {
            // Returns false if the chunk was already loaded.
            // We'll just assume that if anything is loaded, everything is loaded.
            // Also we don't put a despawn deadline on this, because we assume it'll never unload.
            if !space.load_chunk(&mut commands, space_entity, position, None) {
                break;
            }
        }

        commands.entity(space_entity).remove::<LoadAllTerrain>();
    }
}

fn load_terrain(
    mut commands: Commands,
    terrain_loaders: Query<(&Position, &LoadsTerrain)>,
    mut terrain_spaces: Query<(Entity, &Position, &mut TerrainSpace, With<TerrainStorage>)>,
    terrain_time: Res<TerrainTime>,
) {
    let despawn_deadline = terrain_time.time + CHUNK_TIME_TO_LIVE_TICKS;

    for (space_entity, space_position, mut space, _terrain_file) in terrain_spaces.iter_mut() {
        for (loader_position, loads_terrain) in terrain_loaders.iter() {
            let loader_position_in_chunk_space =
                space_position.quat() * (loader_position.translation - space_position.translation);
            let base_chunk_index =
                (loader_position_in_chunk_space / Chunk::CHUNK_DIAMETER as f32).as_ivec3();

            let radius_squared = (loads_terrain.radius * loads_terrain.radius) as f32;
            for x in -loads_terrain.radius..loads_terrain.radius {
                let z_range = (radius_squared - (x * x) as f32).sqrt().ceil() as i32;
                for z in -z_range..z_range {
                    let y_range = (radius_squared - (x * x) as f32 - (z * z) as f32)
                        .sqrt()
                        .ceil() as i32;

                    for y in -y_range..y_range {
                        let chunk_index = base_chunk_index + ChunkIndex::new(x, y, z);

                        space.load_chunk(
                            &mut commands,
                            space_entity,
                            ChunkPosition { index: chunk_index },
                            Some(despawn_deadline),
                        );
                    }
                }
            }
        }
    }
}

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToDelete;

fn mark_chunk_for_save_and_delete(
    mut commands: Commands,
    chunks: Query<(Entity, With<Chunk>, &ActiveTerrainTimer)>,
    terrain_time: Res<TerrainTime>,
) {
    for (chunk_entity, _chunk, timer) in chunks.iter() {
        if timer.deadline <= terrain_time.time {
            commands
                .entity(chunk_entity)
                .remove::<ActiveTerrainTimer>()
                .insert((ToSaveTerrain, ToDelete));
        }
    }
}

type DeleteChunksQuery<'a, 'b, 'c, 'd> = Query<
    'a,
    'b,
    (
        Entity,
        With<ToDelete>,
        Without<ToSaveTerrain>,
        &'c ChunkPosition,
        &'d Parent,
    ),
>;

fn delete_chunks(
    mut commands: Commands,
    chunks: DeleteChunksQuery,
    mut terrain_spaces: Query<&mut TerrainSpace>,
) {
    for (chunk_entity, _with_to_delete, _without_to_save, position, terrain_space) in chunks.iter()
    {
        // Your time has come. It is time to die.
        let mut space = terrain_spaces
            .get_mut(terrain_space.get())
            .expect("Chunk without a space found.");

        space.loaded_terrain.remove(&position.index);
        space.non_empty_chunks.remove(&chunk_entity);
        commands.entity(chunk_entity).despawn();
    }
}

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct Initialized;

/// Just loads the first chunk to get the whole chunk loading.
/// This will probably be replaced with something more robust later.
fn bootstrap_terrain_space(
    mut commands: Commands,
    mut spaces: Query<(
        Entity,
        &mut TerrainSpace,
        Without<Initialized>,
        Without<LoadAllTerrain>,
    )>,
    terrain_time: Res<TerrainTime>,
) {
    let despawn_deadline = terrain_time.time + CHUNK_TIME_TO_LIVE_TICKS;

    for (space_entity, mut space, _without_initialized, _without_load_all_terrain) in
        spaces.iter_mut()
    {
        space.load_chunk(
            &mut commands,
            space_entity,
            ChunkPosition {
                index: ChunkIndex::ZERO,
            },
            Some(despawn_deadline),
        );

        commands.entity(space_entity).insert(Initialized);
    }
}

fn setup_system(mut commands: Commands) {
    commands.insert_resource(TerrainTime { time: 0 })
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct LoadTerrain;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct UnloadTerrain;

pub fn register_terrain_space(app: &mut App) {
    app.add_startup_system(setup_system);

    app.add_system(load_all_terrain.in_set(LoadTerrain));
    app.add_system(load_terrain.in_set(LoadTerrain));
    app.add_system(bootstrap_terrain_space.in_set(LoadTerrain));

    // We check to clean up chunks once a second.
    // We don't run this after `load_all_terrain` because that terrain doesn't dynamically unload.
    app.configure_set(UnloadTerrain.after(LoadTerrain));

    app.add_system(
        delete_chunks
            .before(super::terrain_time_tick)
            .in_set(UnloadTerrain),
    );
    app.add_system(
        mark_chunk_for_save_and_delete
            .before(delete_chunks)
            .in_set(UnloadTerrain),
    );
}
