use anyhow::{Context, Result};
use bevy::{
    ecs::{entity::EntityMap, system::SystemState},
    prelude::*,
    scene::serde::{SceneDeserializer, SceneSerializer},
};
use bincode::Options;
use serde::{de::DeserializeSeed, Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    mem::size_of,
    sync::atomic::{AtomicBool, Ordering},
};

use crate::world::{
    generation::GenerateSpatial,
    terrain::{Chunk, ChunkPosition},
};

use super::{SpatialEntityTracker, SpatialHash};

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToLoadSpatial;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToSaveSpatial;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct SaveWithParent;

type TracerId = u64;

#[derive(Component, Reflect, Clone, Default)]
#[reflect(Component)]
pub struct Storable {
    id: TracerId,
    bootstrap: BootstrapEntityInfo,
    type_name: Cow<'static, str>,
}

pub trait SpatialEntity {
    fn load(commands: &mut Commands) -> Result<()>;
}

#[derive(Serialize, Deserialize, Reflect, Clone, Copy, Debug)]
pub enum BootstrapEntityInfo {
    NonBootstrap,
    LocalPlayer,
    GlobalTerrain,
}

impl Default for BootstrapEntityInfo {
    fn default() -> Self {
        Self::NonBootstrap
    }
}

#[derive(Resource)]
pub struct SpatialEntityStorage {
    tracers_to_entities: HashMap<TracerId, Entity>,
    entities_to_tracers: HashMap<Entity, TracerId>,
    database: sled::Db,
    spatial_hash_storage: sled::Tree,
    spatial_entities_storage: sled::Tree,

    bootstrap_entities: HashMap<TracerId, BootstrapEntityInfo>,
    bootstrap_entities_modified: bool,
    bootstrap_complete: AtomicBool,
}

impl SpatialEntityStorage {
    pub fn new(database: &sled::Db) -> Result<Self> {
        let spatial_entities_storage = database
            .open_tree("spatial_entities")
            .context("Failed to open tree for spatial entities.")?;

        let bootstrap_entities = Self::load_bootstrap_entities_info(&spatial_entities_storage)
            .context("Failed to load bootstrap entities.")?;

        Ok(SpatialEntityStorage {
            tracers_to_entities: HashMap::new(),
            entities_to_tracers: HashMap::new(),
            database: database.clone(),
            spatial_hash_storage: database
                .open_tree("spatial_hash")
                .context("Failed to open tree for chunk entities.")?,
            spatial_entities_storage,
            bootstrap_entities,
            bootstrap_entities_modified: false,
            bootstrap_complete: AtomicBool::new(false),
        })
    }

    fn load_bootstrap_entities_info(
        tree: &sled::Tree,
    ) -> Result<HashMap<TracerId, BootstrapEntityInfo>> {
        let bootstrap = tree
            .get(b"bootstrap_entities")
            .context("Failed to read chunk entity list from database.")?;

        if let Some(bootstrap) = bootstrap {
            let bootstrap = bincode::deserialize(&bootstrap)
                .context("Failed to deserialize chunk entity list from database.")?;

            dbg!(&bootstrap);

            Ok(bootstrap)
        } else {
            Ok(HashMap::new())
        }
    }

    fn save_bootstrap_entities_list(&mut self) -> Result<()> {
        if self.bootstrap_entities_modified {
            let bootstrap_bytes = bincode::serialize(&self.bootstrap_entities)
                .context("Failed to serialize bootstrap entity list.")?;
            self.spatial_entities_storage
                .insert(b"bootstrap_entities", bootstrap_bytes)
                .context("Failed to write bootstrap entity list to database.")?;

            self.bootstrap_entities_modified = false;
        }

        Ok(())
    }

    pub fn new_storable_component(
        &mut self,
        type_name: impl Into<Cow<'static, str>>,
    ) -> Result<Storable> {
        self.new_bootstrapped_storable_component(BootstrapEntityInfo::NonBootstrap, type_name)
    }

    pub fn new_bootstrapped_storable_component(
        &mut self,
        bootstrap: BootstrapEntityInfo,
        type_name: impl Into<Cow<'static, str>>,
    ) -> Result<Storable> {
        let type_name = type_name.into();

        // Start by getting a valid entity ID.
        let tracer_id = self
            .database
            .fetch_and_update("next_tracer_id", |next_tracer_id| {
                let next_tracer_id = match next_tracer_id {
                    Some(next_tracer_id) => {
                        let next_tracer_id: [u8; size_of::<TracerId>()] = next_tracer_id
                            .try_into()
                            .unwrap_or([0u8; size_of::<TracerId>()]);
                        let next_tracer_id = TracerId::from_be_bytes(next_tracer_id);
                        next_tracer_id + 1
                    }
                    None => 1,
                };

                Some(next_tracer_id.to_be_bytes().to_vec())
            })
            .context("Failed to read next tracer ID from database.")?
            .map(|next_tracer_id| {
                let next_tracer_id = next_tracer_id.as_ref();
                let next_tracer_id: [u8; size_of::<TracerId>()] = next_tracer_id
                    .try_into()
                    .unwrap_or([0u8; size_of::<TracerId>()]);
                TracerId::from_be_bytes(next_tracer_id)
            })
            .unwrap_or(0);

        // We do not store the tracings here. A system will handle that later.
        // Why do we do it this way? Because an entity can also be loaded from a file, and that would bypass this bit of code.
        // I'd rather not have copies of this in multiple places so we'll just have one system to do both jobs.

        dbg!(tracer_id, bootstrap, &type_name);
        match bootstrap {
            BootstrapEntityInfo::NonBootstrap => {}
            _ => {
                self.bootstrap_entities.insert(tracer_id, bootstrap);
                self.bootstrap_entities_modified = true;
            }
        }

        Ok(Storable {
            id: tracer_id,
            bootstrap,
            type_name,
        })
    }

    pub fn load_entity(
        &self,
        type_registry: &AppTypeRegistry,
        tracer_id: u64,
    ) -> Result<Option<DynamicScene>> {
        if !self.tracers_to_entities.contains_key(&tracer_id) {
            let tracer_key = tracer_id.to_be_bytes();

            let entity_bytes = self
                .spatial_entities_storage
                .get(tracer_key)?
                .context("Entity was not present in database.")?;

            let mut bincode_deserializer = bincode::Deserializer::from_slice(
                &entity_bytes,
                bincode::DefaultOptions::new()
                    .with_fixint_encoding()
                    .allow_trailing_bytes(),
            );
            let scene_deserializer = SceneDeserializer {
                type_registry: &type_registry.read(),
            };

            let dynamic_scene = scene_deserializer
                .deserialize(&mut bincode_deserializer)
                .context("Failed to deserialize entity.")?;

            Ok(Some(dynamic_scene))
        } else {
            // The entity has already been loaded.
            Ok(None)
        }
    }

    pub fn delete_entity(&mut self, trace_id: TracerId) {
        // Note that this will leave a reference to the entity in the terrain chunk. That's okay, it's invalidated and won't cause a crash.
        // It will eventually be cleaned up when it's not spawned.
        self.bootstrap_entities.remove(&trace_id);
        if let Err(error) = self.spatial_entities_storage.remove(trace_id.to_be_bytes()) {
            log::warn!("Failed to delete entity {}: {:?}", trace_id, error);
        }
    }

    /// Gets the Entity ID for a traceable object.
    /// Returning None doesn't mean the entity doesn't exist anymore. It's just not loaded.
    pub fn get_entity(&self, trace_id: Storable) -> Option<Entity> {
        self.tracers_to_entities.get(&trace_id.id).copied()
    }

    /// An entity may need to store complicated data and manipulate it on the fly. You can use this to gain
    /// access to a tree to do that with. You can't create namespaces within the tree, so you'll need to use
    /// a prefix to keep your component's entries from stomping over the entries of other components.
    pub fn get_entity_tree(&self, storable: &Storable) -> Result<sled::Tree> {
        let tree = self.database.open_tree(format!("entity_{}", storable.id))?;
        Ok(tree)
    }
}

fn new_tracings(
    new_tracings: Query<(Entity, &Storable), Changed<Storable>>,
    storage: Option<ResMut<SpatialEntityStorage>>,
) {
    if let Some(mut storage) = storage {
        for (entity_id, traceable) in new_tracings.iter() {
            let tracer_id = traceable.id;

            storage.tracers_to_entities.insert(tracer_id, entity_id);
            storage.entities_to_tracers.insert(entity_id, tracer_id);
        }
    }
}

fn clean_up_tracings(
    mut removed: RemovedComponents<Storable>,
    storage: Option<ResMut<SpatialEntityStorage>>,
) {
    if let Some(mut storage) = storage {
        for entity_id in removed.iter() {
            if let Some(tracker_id) = storage.entities_to_tracers.remove(&entity_id) {
                storage.tracers_to_entities.remove(&tracker_id);
            } else {
                log::warn!("Removed traceable without an associated entity.");
            }
        }
    }
}

type LoadSystemState<'a, 'b, 'c> = SystemState<(
    Commands<'a, 'b>,
    Query<'a, 'b, (Entity, &'c ChunkPosition, With<ToLoadSpatial>)>,
    Option<Res<'a, SpatialEntityStorage>>,
    Res<'a, AppTypeRegistry>,
)>;

#[derive(Resource)]
struct CachedLoadSystemState {
    state: LoadSystemState<'static, 'static, 'static>,
}

fn load_chunk(world: &mut World) {
    fn load_spatial_hash(
        position: &ChunkPosition,
        storage: &SpatialEntityStorage,
    ) -> Result<Option<Vec<TracerId>>> {
        let chunk_position_encoding = position.to_database_key();

        let entity_set_bytes = storage
            .spatial_hash_storage
            .get(chunk_position_encoding)
            .context("Failed to read chunk entity list from database.")?;

        if let Some(entity_set_bytes) = entity_set_bytes {
            let entity_set = bincode::deserialize(&entity_set_bytes)
                .context("Failed to deserialize chunk entity list from database.")?;

            Ok(Some(entity_set))
        } else {
            Ok(None)
        }
    }

    world.resource_scope(|world, mut system_state: Mut<CachedLoadSystemState>| {
        let (mut commands, to_load, storage, type_registry) = system_state.state.get_mut(world);

        if let Some(storage) = storage {
            let mut dynamic_scenes = Vec::new();

            for (entity, position, _with_to_load_spatial) in to_load.iter() {
                let mut chunk_entity_commands = commands.entity(entity);
                chunk_entity_commands.remove::<ToLoadSpatial>();

                match load_spatial_hash(position, &storage) {
                    Ok(Some(tracer_set)) => {
                        for tracer_id in tracer_set {
                            match storage.load_entity(&type_registry, tracer_id) {
                                Ok(Some(dynamic_scene)) => {
                                    dynamic_scenes.push((tracer_id, position.index, dynamic_scene))
                                }
                                Ok(None) => {} // The entity was already loaded.
                                Err(error) => log::error!(
                                    "Failed to load entity {} in chunk {}: {:?}",
                                    tracer_id,
                                    position.index,
                                    error
                                ),
                            }
                        }
                    }
                    Ok(None) => {
                        // There was no error but no data has been saved to the database.
                        // We need to generate the chunk.
                        chunk_entity_commands.insert(GenerateSpatial);
                    }
                    Err(error) => {
                        log::error!(
                            "Failed to load entity list for chunk {}: {:?}",
                            position.index,
                            error
                        );
                        chunk_entity_commands.insert(GenerateSpatial);
                    }
                }
            }

            for (tracer_id, position, dynamic_scene) in dynamic_scenes {
                if let Err(error) = dynamic_scene.write_to_world(world, &mut EntityMap::default()) {
                    log::error!(
                        "Failed to spawn spatial entity {} in chunk {}: {:?}",
                        tracer_id,
                        position,
                        error
                    );
                }
            }
        }

        system_state.state.apply(world);
    });
}

type SaveSystemState<'a, 'b, 'c, 'd, 'e> = SystemState<(
    Commands<'a, 'b>,
    Query<'a, 'b, (Entity, &'c ChunkPosition, With<ToSaveSpatial>)>,
    Query<'a, 'b, (Entity, &'d Storable)>,
    Query<'a, 'b, &'e Children>,
    Query<'a, 'b, (Entity, With<SaveWithParent>)>,
    Option<Res<'a, SpatialEntityStorage>>,
    Res<'a, SpatialEntityTracker>,
    Res<'a, AppTypeRegistry>,
)>;

#[derive(Resource)]
struct CachedSaveSystemState {
    state: SaveSystemState<'static, 'static, 'static, 'static, 'static>,
}

fn save_chunk(world: &mut World) {
    fn save_spatial_hash(
        entity_set: &HashSet<Entity>,
        spatial_entities: &Query<(Entity, &Storable)>,
        position: &ChunkPosition,
        storage: &SpatialEntityStorage,
    ) -> Result<()> {
        let tracer_set = {
            let mut tracer_set = Vec::new();
            tracer_set.reserve(entity_set.len());

            for entity in entity_set {
                // We can only store storable entities.
                if let Ok((_entity, storable)) = spatial_entities.get(*entity) {
                    tracer_set.push(storable.id);
                }
            }

            tracer_set
        };

        let tracer_set_bytes = bincode::serialize(&tracer_set)?;
        let chunk_position_encoding = position.to_database_key();

        storage
            .spatial_hash_storage
            .insert(chunk_position_encoding, tracer_set_bytes)?;
        storage.spatial_hash_storage.flush()?;

        Ok(())
    }

    world.resource_scope(|world, mut system_state: Mut<CachedSaveSystemState>| {
        let (mut commands, chunks_to_save, spatial_entities, children, to_save_with_parents, storage, spatial_tracking, type_registry) =
            system_state.state.get(world);

        if let Some(storage) = storage {
            for (chunk_entity, position, _with_to_save_spatial) in chunks_to_save.iter() {
                commands.entity(chunk_entity).remove::<ToSaveSpatial>();
                if let Some(entity_set) =
                    spatial_tracking.get_cell_entities(&SpatialHash::from(position.index))
                {
                    match save_spatial_hash(entity_set, &spatial_entities, position, &storage) {
                        Ok(_) => {
                            // We would skip saving the entities if we can't even save a reference to them.
                            // I want to avoid garbage data that we can never clean up ending up in the database.
                            for cell_entity in entity_set.iter() {
                                // dbg!(cell_entity);
                                if let Ok((entity, storable)) = spatial_entities.get(*cell_entity) {
                                    let mut builder = DynamicSceneBuilder::from_world(world);
                                    dbg!(entity);
                                    builder.extract_entity(entity);


                                    for descendant in children.iter_descendants(entity) {
                                        if let Ok((descendant, _with_save_with_parent)) = to_save_with_parents.get(descendant) {
                                            builder.extract_entity(descendant);
                                        }
                                    }

                                    let dynamic_scene = builder.build();

                                    dbg!(entity, storable.id, &storable.type_name);

                                    for entity in dynamic_scene.entities.iter() {
                                        dbg!(entity.entity);
                                        for component in entity.components.iter() {
                                            dbg!(component.type_name(), component);
                                        }
                                    }

                                    let mut serialized_data = Vec::new();

                                    let mut bincode_serializer = bincode::Serializer::new(
                                        &mut serialized_data,
                                        bincode::DefaultOptions::new().with_fixint_encoding().allow_trailing_bytes(),
                                    );

                                    let scene_serializer =
                                        SceneSerializer::new(&dynamic_scene, &type_registry);

                                    if let Err(error) = scene_serializer.serialize(&mut bincode_serializer) {
                                        log::error!(
                                            "Failed to serialize spatial entity {} in chunk {}: {:?}",
                                            storable.id,
                                            position.index,
                                            error
                                        );
                                        continue;
                                    }

                                    if let Err(error) = storage
                                        .spatial_entities_storage
                                        .insert(storable.id.to_be_bytes(), serialized_data)
                                    {
                                        log::error!(
                                            "Failed to write spatial entity {} in chunk {} to database: {:?}",
                                            storable.id,
                                            position.index,
                                            error
                                        );
                                    }
                                }
                            }
                        }
                        Err(error) => log::error!(
                            "Failed to save spatial entity hashes for chunk {}: {:?}",
                            position.index,
                            error
                        ),
                    }
                } else {
                    match save_spatial_hash(&HashSet::new(), &spatial_entities, position, &storage) {
                        Ok(_) => {
                            // Cool, just wanted to make sure we don't regenerate it again later.
                        }
                        Err(error) => log::error!(
                            "Failed to save spatial entity hashes for chunk {}: {:?}",
                            position.index,
                            error
                        ),
                    }
                }
            }

            if let Err(error) = storage.spatial_entities_storage.flush() {
                log::error!("Failed to flush spatial entity storage: {:?}", error);
            }
        }

        system_state.state.apply(world);
    });
}

fn save_bootstrap_entity_list(storage: Option<ResMut<SpatialEntityStorage>>) {
    if let Some(mut storage) = storage {
        if let Err(error) = storage.save_bootstrap_entities_list() {
            log::error!("Failed to save bootstrap entities list: {:?}", error);
        }
    }
}

type BootstrapEntitiesSystemState<'a> = SystemState<(
    Option<Res<'a, SpatialEntityStorage>>,
    Res<'a, AppTypeRegistry>,
)>;

#[derive(Resource)]
struct CachedBootstrapEntitiesSystemState {
    state: BootstrapEntitiesSystemState<'static>,
}

fn bootstrap_entities(world: &mut World) {
    world.resource_scope(
        |world, mut system_state: Mut<CachedBootstrapEntitiesSystemState>| {
            let (storage, type_registry) = system_state.state.get_mut(world);

            if let Some(storage) = storage {
                if !storage.bootstrap_complete.swap(true, Ordering::SeqCst) {
                    let mut entity_map = EntityMap::default();
                    let mut dynamic_scenes = Vec::new();

                    for tracer_id in storage.bootstrap_entities.keys() {
                        match storage.load_entity(&type_registry, *tracer_id) {
                            Ok(Some(dynamic_scene)) => {
                                dynamic_scenes.push((*tracer_id, dynamic_scene))
                            }
                            Ok(None) => {} // The entity was already loaded.
                            Err(error) => {
                                log::error!(
                                    "Failed to load spatial entity {} for bootstrapping: {:?}",
                                    tracer_id,
                                    error
                                );
                            }
                        }
                    }

                    for (tracer_id, dynamic_scene) in dynamic_scenes {
                        dbg!(tracer_id);

                        for entity in dynamic_scene.entities.iter() {
                            dbg!(entity.entity);
                            for component in entity.components.iter() {
                                dbg!(component.type_name(), component);
                            }
                        }

                        if let Err(error) = dynamic_scene.write_to_world(world, &mut entity_map) {
                            log::error!(
                                "Failed to bootstrap spatial entity {}: {:?}",
                                tracer_id,
                                error
                            );
                        }
                    }
                }
            }

            system_state.state.apply(world);
        },
    );
}

const SAVE_INTERVAL_TICKS: usize = 60;

#[derive(Resource)]
struct EntitySaveTimer {
    ticks_since_last_save: usize,
}

/// The timer went off, we need to mark the chunk as needing to be saved.
fn save_timer_trigger(
    mut commands: Commands,
    chunks: Query<(Entity, With<Chunk>)>,
    mut save_timer: ResMut<EntitySaveTimer>,
) {
    save_timer.ticks_since_last_save += 1;

    if save_timer.ticks_since_last_save >= SAVE_INTERVAL_TICKS {
        save_timer.ticks_since_last_save -= SAVE_INTERVAL_TICKS;

        for (entity, _with_chunk) in chunks.iter() {
            commands.entity(entity).insert(ToSaveSpatial);
        }
    }
}

pub(super) fn register_storage(app: &mut App) {
    app.add_startup_system(|world: &mut World| {
        let initial_state: SaveSystemState = SystemState::new(world);

        // The system state is cached in a resource
        world.insert_resource(CachedSaveSystemState {
            state: initial_state,
        });

        let initial_state: LoadSystemState = SystemState::new(world);

        // The system state is cached in a resource
        world.insert_resource(CachedLoadSystemState {
            state: initial_state,
        });

        let initial_state: BootstrapEntitiesSystemState = SystemState::new(world);

        // The system state is cached in a resource
        world.insert_resource(CachedBootstrapEntitiesSystemState {
            state: initial_state,
        });

        world.insert_resource(EntitySaveTimer {
            ticks_since_last_save: 0,
        });
    });

    app.add_system(new_tracings);
    app.add_system(clean_up_tracings);
    app.add_system(load_chunk);
    app.add_system(
        save_chunk
            .after(super::update_spatial_hash_entities)
            .after(super::update_spatial_hash_entities_with_offset),
    );
    app.add_system(save_timer_trigger.before(save_chunk));

    app.add_system(save_bootstrap_entity_list.in_schedule(CoreSchedule::FixedUpdate));
    app.add_system(bootstrap_entities.before(load_chunk));
}
