use super::{
    storage::{TerrainStorage, ToLoadTerrain, ToSaveTerrain},
    Block, Chunk, ChunkIndex, ChunkPosition, GlobalBlockCoordinate, LocalBlockCoordinate,
    PreChunkBundle, TerrainTime,
};
use crate::world::{
    generation::WorldGeneratorEnum, physics::Position, spatial_entities::storage::ToLoadSpatial,
    ViewRadius,
};
use bevy::{
    ecs::query::{ReadOnlyWorldQuery, WorldQuery},
    prelude::*,
};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    num::NonZeroUsize,
};

pub const CHUNK_TIME_TO_LIVE_TICKS: usize = 30;

#[derive(Component, Reflect, Default)]
#[reflect(Component)]
pub struct LoadsTerrain;
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
    pub(crate) generator: WorldGeneratorEnum,
    loaded_terrain: HashMap<ChunkIndex, Entity>,
    chunk_time_to_live_ticks: Option<NonZeroUsize>,
    is_global: bool,
    pub(crate) non_empty_chunks: HashSet<Entity>,
}

impl TerrainSpace {
    pub fn global(generator: WorldGeneratorEnum) -> Self {
        Self {
            generator,
            chunk_time_to_live_ticks: NonZeroUsize::new(CHUNK_TIME_TO_LIVE_TICKS),
            is_global: true,
            ..Default::default()
        }
    }

    pub fn local(generator: impl Into<WorldGeneratorEnum>) -> Self {
        Self {
            generator: generator.into(),
            ..Default::default()
        }
    }

    pub fn num_non_empty_chunks(&self) -> usize {
        self.non_empty_chunks.len()
    }

    pub fn iter_loaded_chunks(&self) -> impl Iterator<Item = &Entity> {
        self.loaded_terrain.values()
    }

    pub fn get_loaded_chunk_entity(&self, index: &ChunkIndex) -> Option<Entity> {
        self.loaded_terrain.get(index).copied()
    }

    fn mark_chunk_for_load(
        &mut self,
        commands: &mut Commands,
        space_entity: Entity,
        chunk_position: ChunkPosition,
        chunk_time: &TerrainTime,
    ) -> (bool, Entity) {
        let despawn_deadline = self
            .chunk_time_to_live_ticks
            .map(|time_to_live| chunk_time.time + time_to_live.get());

        match self.loaded_terrain.entry(chunk_position.index) {
            std::collections::hash_map::Entry::Occupied(already_loaded) => {
                // Oh, already loaded? Just update the timer then.
                let entity = *already_loaded.get();

                if let Some(despawn_deadline) = despawn_deadline {
                    commands.entity(entity).insert(ActiveTerrainTimer {
                        deadline: despawn_deadline,
                    });
                }

                (false, entity)
            }
            std::collections::hash_map::Entry::Vacant(to_load) => {
                let mut entity = commands.spawn((
                    PreChunkBundle {
                        chunk_position,
                        ..Default::default()
                    },
                    ToLoadTerrain, // Mark that this terrain needs to be loaded.
                ));

                if self.is_global {
                    // Oh, we'll need to load spatial entities with that.
                    entity.insert(ToLoadSpatial);
                }

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
                (true, entity)
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
}

#[derive(Serialize, Deserialize)]
pub enum SpaceModificationRequest {
    /// Set a block to a value, replacing whatever was there before.
    ReplaceBlock {
        coordinate: GlobalBlockCoordinate,
        new_block: Option<Block>,
    },
}

impl SpaceModificationRequest {
    fn into_chunk_modification_requests(
        self,
    ) -> impl Iterator<Item = (ChunkIndex, ChunkModificationRequest)> {
        match self {
            SpaceModificationRequest::ReplaceBlock {
                coordinate,
                new_block,
            } => {
                let (chunk_index, local_block_coordinate) =
                    TerrainSpace::calculate_block_indexes(coordinate);

                [(
                    chunk_index,
                    ChunkModificationRequest::ReplaceBlock {
                        coordinate: local_block_coordinate,
                        new_block,
                    },
                )]
                .into_iter()
            }
        }
    }
}

#[derive(Component, Default)]
pub struct SpaceModificationRequestList {
    requests: Vec<SpaceModificationRequest>,
}

impl SpaceModificationRequestList {
    pub fn push(&mut self, request: SpaceModificationRequest) {
        self.requests.push(request);
    }
}

fn distribute_space_modification_requests_to_chunks(
    mut commands: Commands,
    mut spaces: Query<(Entity, &mut TerrainSpace, &mut SpaceModificationRequestList)>,
    mut chunks: Query<&mut ChunkModificationRequestList>,
    terrain_time: Res<TerrainTime>,
) {
    for (space_entity, mut space, mut modification_request_list) in spaces.iter_mut() {
        for modification_request in modification_request_list.requests.drain(..) {
            for (chunk_index, modification_request) in
                modification_request.into_chunk_modification_requests()
            {
                // As an added benifit (probably), this makes it so that modifications to terrain will refresh it and keep it loaded for
                // a little longer.
                let (_created, chunk_entity) = space.mark_chunk_for_load(
                    &mut commands,
                    space_entity,
                    ChunkPosition { index: chunk_index },
                    &terrain_time,
                );

                let mut chunk_modification_request = chunks
                    .get_mut(chunk_entity)
                    .expect("Chunk was not created.");
                chunk_modification_request.push(modification_request);
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub enum ChunkModificationRequest {
    /// Set a block to a value, replacing whatever was there before.
    ReplaceBlock {
        coordinate: LocalBlockCoordinate,
        new_block: Option<Block>,
    },
}

#[derive(Component, Default)]
pub struct ChunkModificationRequestList {
    requests: Vec<ChunkModificationRequest>,
}

impl ChunkModificationRequestList {
    pub fn push(&mut self, request: ChunkModificationRequest) {
        self.requests.push(request);
    }
}

fn apply_modification_request_lists_to_chunks(
    mut chunks: Query<(&mut Chunk, &mut ChunkModificationRequestList)>,
) {
    for (mut chunk, mut request_list) in chunks.iter_mut() {
        for request in request_list.requests.drain(..) {
            match request {
                ChunkModificationRequest::ReplaceBlock {
                    coordinate,
                    new_block,
                } => {
                    let block_memory = chunk
                        .get_block_local_mut(coordinate)
                        .expect("Local coordinate was outside of chunk range.");
                    *block_memory = new_block;
                }
            }
        }
    }
}

#[allow(clippy::complexity)]
fn insert_empty_chunks_for_modification(
    mut commands: Commands,
    chunks: Query<
        (Entity, &Parent, &ChunkModificationRequestList),
        (
            Without<Chunk>,
            Without<ToLoadTerrain>,
            Changed<ChunkModificationRequestList>,
        ),
    >,
    mut terrain_spaces: Query<&mut TerrainSpace>,
) {
    for (chunk_entity, chunk_parent, request_list) in chunks.iter() {
        if !request_list.requests.is_empty() {
            commands.entity(chunk_entity).insert(Chunk::new(None));

            let terrain_space_entity = chunk_parent.get();
            let mut terrain_space = terrain_spaces
                .get_mut(terrain_space_entity)
                .expect("Chunk did not have terrain space parent.");
            terrain_space.non_empty_chunks.insert(chunk_entity);
        }
    }
}

fn remove_empty_chunks_after_modification(
    mut commands: Commands,
    chunks: Query<(Entity, &Parent, &Chunk), Changed<Chunk>>,
    mut terrain_spaces: Query<&mut TerrainSpace>,
) {
    const EMPTY_CHUNK: Chunk = Chunk::new(None);

    for (chunk_entity, chunk_parent, chunk) in chunks.iter() {
        // I have found that the fastest way to verify that an array of fixed length is all of a specific
        // sequence of values (in this case, all being None) is to just compare it to another array.
        // Yes, this is even faster than folding the values with xor. I suspect this is using memcmp under the hood.
        if *chunk == EMPTY_CHUNK {
            // That's an empty chunk!
            commands
                .entity(chunk_entity)
                .remove::<Chunk>()
                .insert(ToSaveTerrain);
        }

        let terrain_space_entity = chunk_parent.get();
        let mut terrain_space = terrain_spaces
            .get_mut(terrain_space_entity)
            .expect("Chunk did not have terrain space parent.");
        terrain_space.non_empty_chunks.remove(&chunk_entity);
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
    pub storage: TerrainStorage,
    pub modification_request_list: SpaceModificationRequestList,
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
    terrain_time: Res<TerrainTime>,
) {
    for (space_entity, mut space, terrain_file, _keep_all_terrain_loaded) in spaces.iter_mut() {
        for position in terrain_file.iter_chunk_indexes() {
            // Returns false if the chunk was already loaded.
            // We'll just assume that if anything is loaded, everything is loaded.
            if !space
                .mark_chunk_for_load(&mut commands, space_entity, position, &terrain_time)
                .0
            {
                break;
            }
        }

        commands.entity(space_entity).remove::<LoadAllTerrain>();
    }
}

fn mark_terrain_for_load(
    mut commands: Commands,
    terrain_loaders: Query<(&Position, &ViewRadius), With<LoadsTerrain>>,
    mut terrain_spaces: Query<(Entity, &Position, &mut TerrainSpace, With<TerrainStorage>)>,
    terrain_time: Res<TerrainTime>,
) {
    for (space_entity, space_position, mut space, _terrain_file) in terrain_spaces.iter_mut() {
        for (loader_position, view_radius) in terrain_loaders.iter() {
            let loader_position_in_chunk_space =
                space_position.quat() * (loader_position.translation - space_position.translation);
            let base_chunk_index =
                (loader_position_in_chunk_space / Chunk::CHUNK_DIAMETER as f32).as_ivec3();

            let radius_squared = (view_radius.chunks * view_radius.chunks) as f32;
            for x in -view_radius.chunks..view_radius.chunks {
                let z_range = (radius_squared - (x * x) as f32).sqrt().ceil() as i32;
                for z in -z_range..z_range {
                    let y_range = (radius_squared - (x * x) as f32 - (z * z) as f32)
                        .sqrt()
                        .ceil() as i32;

                    for y in -y_range..y_range {
                        let chunk_index = base_chunk_index + ChunkIndex::new(x, y, z);

                        space.mark_chunk_for_load(
                            &mut commands,
                            space_entity,
                            ChunkPosition { index: chunk_index },
                            &terrain_time,
                        );
                    }
                }
            }
        }
    }
}

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToUnloadTerrain;

fn mark_chunk_for_save_and_unload(
    mut commands: Commands,
    chunks: Query<(Entity, With<Chunk>, &ActiveTerrainTimer)>,
    terrain_time: Res<TerrainTime>,
) {
    for (chunk_entity, _chunk, timer) in chunks.iter() {
        if timer.deadline <= terrain_time.time {
            commands
                .entity(chunk_entity)
                .remove::<ActiveTerrainTimer>()
                .insert((ToSaveTerrain, ToUnloadTerrain));
        }
    }
}

type DeleteChunksQuery<'a, 'b, 'c, 'd> = Query<
    'a,
    'b,
    (
        Entity,
        With<ToUnloadTerrain>,
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
    for (space_entity, mut space, _without_initialized, _without_load_all_terrain) in
        spaces.iter_mut()
    {
        space.mark_chunk_for_load(
            &mut commands,
            space_entity,
            ChunkPosition {
                index: ChunkIndex::ZERO,
            },
            &terrain_time,
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
pub struct ModifyTerrain;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct PreModifyTerrain;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct UnloadTerrain;

pub fn register_terrain_space(app: &mut App) {
    app.add_systems(Startup, setup_system);

    app.add_systems(
        Update,
        (
            load_all_terrain.in_set(LoadTerrain),
            mark_terrain_for_load.in_set(LoadTerrain),
            bootstrap_terrain_space.in_set(LoadTerrain),
        ),
    );

    // We check to clean up chunks once a second.
    // We don't run this after `load_all_terrain` because that terrain doesn't dynamically unload.
    app.configure_set(Update, UnloadTerrain.after(LoadTerrain));
    app.configure_set(Update, PreModifyTerrain.before(ModifyTerrain));
    app.configure_set(Update, ModifyTerrain.after(LoadTerrain));

    app.add_systems(
        Update,
        (
            delete_chunks
                .before(super::terrain_time_tick)
                .in_set(UnloadTerrain),
            distribute_space_modification_requests_to_chunks.in_set(PreModifyTerrain),
            insert_empty_chunks_for_modification
                .in_set(ModifyTerrain)
                .before(apply_modification_request_lists_to_chunks),
            apply_modification_request_lists_to_chunks.in_set(ModifyTerrain),
            remove_empty_chunks_after_modification
                .in_set(ModifyTerrain)
                .after(apply_modification_request_lists_to_chunks),
            mark_chunk_for_save_and_unload
                .before(delete_chunks)
                .in_set(UnloadTerrain),
        ),
    );
}
