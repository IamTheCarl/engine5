use anyhow::{Context, Result};
use bevy::{app::AppExit, ecs::query::WorldQuery, prelude::*};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    mem::size_of,
    sync::{
        atomic::{AtomicBool, Ordering},
        Mutex,
    },
};

use crate::world::{
    generation::ToGenerateSpatial,
    terrain::{storage::CHUNK_TIME_TO_SAVE, ChunkPosition},
    WorldEntity,
};

use super::SpatialEntityTracker;

const ENTITY_TREE_PREFIX: &[u8] = b"ENTITY-";

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToLoadSpatial;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToSaveSpatial;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToUnloadSpatial;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToDeleteSpatial;

pub type TracerId = u64;

fn tree_name_for_entity(
    tracer_id: TracerId,
) -> [u8; ENTITY_TREE_PREFIX.len() + std::mem::size_of::<TracerId>()] {
    let mut entity_tree_id = [0u8; ENTITY_TREE_PREFIX.len() + std::mem::size_of::<TracerId>()];

    entity_tree_id[..ENTITY_TREE_PREFIX.len()].copy_from_slice(ENTITY_TREE_PREFIX);
    entity_tree_id[ENTITY_TREE_PREFIX.len()..].copy_from_slice(&tracer_id.to_be_bytes());

    entity_tree_id
}

#[derive(Component)]
pub struct Storable {
    id: TracerId,
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

#[derive(Debug)]
struct BootstrapList {
    bootstrap_entities: HashMap<TracerId, BootstrapEntityInfo>,
    bootstrap_entities_modified: bool,
}

#[derive(Resource)]
pub struct EntityStorage {
    tracers_to_entities: HashMap<TracerId, Entity>,
    entities_to_tracers: HashMap<Entity, TracerId>,
    database: sled::Db,
    spatial_hash_storage: sled::Tree,
    spatial_entities_storage: sled::Tree,

    bootstrap_list: Mutex<BootstrapList>,
    bootstrap_complete: AtomicBool,
}

impl EntityStorage {
    pub fn new(database: &sled::Db) -> Result<Self> {
        let spatial_entities_storage = database
            .open_tree("spatial_entities")
            .context("Failed to open tree for spatial entities.")?;

        let bootstrap_entities = Self::load_bootstrap_entities_info(&spatial_entities_storage)
            .context("Failed to load bootstrap entities.")?;

        Ok(EntityStorage {
            tracers_to_entities: HashMap::new(),
            entities_to_tracers: HashMap::new(),
            database: database.clone(),
            spatial_hash_storage: database
                .open_tree("spatial_hash")
                .context("Failed to open tree for chunk entities.")?,
            spatial_entities_storage,
            bootstrap_list: Mutex::new(BootstrapList {
                bootstrap_entities,
                bootstrap_entities_modified: false,
            }),
            bootstrap_complete: AtomicBool::new(true),
        })
    }

    pub fn request_bootstrap(&self) {
        self.bootstrap_complete.store(false, Ordering::SeqCst);
    }

    fn load_bootstrap_entities_info(
        tree: &sled::Tree,
    ) -> Result<HashMap<TracerId, BootstrapEntityInfo>> {
        let bootstrap = tree
            .get(b"bootstrap_entities")
            .context("Failed to read chunk entity list from database.")?;

        if let Some(bootstrap) = bootstrap {
            let bootstrap = ciborium::from_reader(&*bootstrap)
                .context("Failed to deserialize chunk entity list from database.")?;

            Ok(bootstrap)
        } else {
            Ok(HashMap::new())
        }
    }

    fn save_bootstrap_entities_list(&self) -> Result<()> {
        let mut bootstrap_list = self
            .bootstrap_list
            .lock()
            .expect("Bootstrap list has been poisoned!");

        if bootstrap_list.bootstrap_entities_modified {
            let mut bootstrap_bytes = Vec::new();
            ciborium::into_writer(&bootstrap_list.bootstrap_entities, &mut bootstrap_bytes)
                .context("Failed to serialize bootstrap entity list.")?;
            self.spatial_entities_storage
                .insert(b"bootstrap_entities", bootstrap_bytes)
                .context("Failed to write bootstrap entity list to database.")?;

            bootstrap_list.bootstrap_entities_modified = false;
        }

        Ok(())
    }

    pub fn new_storable_component<E, Q>(&self) -> Result<Storable>
    where
        E: SpatialEntity<Q>,
        Q: WorldQuery,
    {
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
        match E::BOOTSTRAP {
            BootstrapEntityInfo::NonBootstrap => {}
            _ => {
                let mut bootstrap_list = self
                    .bootstrap_list
                    .lock()
                    .expect("Bootstrap list has been poisoned!");
                bootstrap_list
                    .bootstrap_entities
                    .insert(tracer_id, E::BOOTSTRAP);
                bootstrap_list.bootstrap_entities_modified = true;
            }
        }

        Ok(Storable { id: tracer_id })
    }

    pub fn new_storable_component_with_tree<E, Q>(&self) -> Result<(Storable, sled::Tree)>
    where
        E: SpatialEntity<Q>,
        Q: WorldQuery,
    {
        let storable = self.new_storable_component::<E, Q>()?;
        let tree_name = tree_name_for_entity(storable.id);

        let tree = self
            .database
            .open_tree(tree_name)
            .context("Failed to open tree for entity.")?;

        Ok((storable, tree))
    }

    pub fn delete_entity(&self, tracer_id: TracerId) {
        // FIXME that this will leave a reference to the entity in the terrain chunk. That's okay for now, it's invalidated and won't cause a crash.
        // It will eventually be cleaned up when it's not spawned, but it will print error messages.

        {
            let mut bootstrap_list = self
                .bootstrap_list
                .lock()
                .expect("Bootstrap list is poisoned!");
            bootstrap_list.bootstrap_entities.remove(&tracer_id);
        }

        if let Err(error) = self
            .spatial_entities_storage
            .remove(tracer_id.to_be_bytes())
        {
            log::warn!("Failed to delete entity {}: {:?}", tracer_id, error);
        }

        let tree_name = tree_name_for_entity(tracer_id);

        if let Err(error) = self.database.drop_tree(tree_name) {
            log::warn!("Failed to delete tree entity {}: {:?}", tracer_id, error);
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
    storage: Option<ResMut<EntityStorage>>,
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
    storage: Option<ResMut<EntityStorage>>,
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

fn save_bootstrap_entity_list(storage: Option<Res<EntityStorage>>) {
    if let Some(storage) = storage {
        if let Err(error) = storage.save_bootstrap_entities_list() {
            log::error!("Failed to save bootstrap entities list: {:?}", error);
        }
    }
}

fn bootstrap_entities(
    mut commands: Commands,
    storage: Option<Res<EntityStorage>>,
    serialization_manager: Res<EntitySerializationManager>,
    world_entity: Option<Res<WorldEntity>>,
) {
    if let (Some(storage), Some(world_entity)) = (storage, world_entity) {
        if !storage.bootstrap_complete.swap(true, Ordering::SeqCst) {
            let bootstrap_list = storage
                .bootstrap_list
                .lock()
                .expect("Bootstrap list has been poisoned!");
            for tracer_id in bootstrap_list.bootstrap_entities.keys() {
                if let Err(error) = serialization_manager.load_entity(
                    *tracer_id,
                    &storage,
                    world_entity.entity,
                    &mut commands,
                ) {
                    log::error!("Failed to load entity {}: {:?}", tracer_id, error);
                }
            }
        }
    }
}

fn save_spatial_hashes(
    mut commands: Commands,
    spatial_entity_tracker: Res<SpatialEntityTracker>,
    spatial_entities: Query<(Entity, &Storable)>,
    chunks: Query<(Entity, &ChunkPosition), With<ToSaveSpatial>>,
    storage: Option<Res<EntityStorage>>,
) {
    fn save_spatial_hash(
        commands: &mut Commands,
        entity_set: &HashSet<Entity>,
        spatial_entities: &Query<(Entity, &Storable)>,
        position: &ChunkPosition,
        storage: &EntityStorage,
    ) -> Result<()> {
        let tracer_set = {
            let mut tracer_set = Vec::new();
            tracer_set.reserve(entity_set.len());

            for entity in entity_set {
                // We can only store storable entities.
                if let Ok((entity, storable)) = spatial_entities.get(*entity) {
                    tracer_set.push(storable.id);
                    commands.entity(entity).insert(ToSaveSpatial);
                }
            }

            tracer_set
        };

        let mut tracer_set_bytes = Vec::new();
        ciborium::into_writer(&tracer_set, &mut tracer_set_bytes)?;
        let chunk_position_encoding = position.to_database_key();

        storage
            .spatial_hash_storage
            .insert(chunk_position_encoding, tracer_set_bytes)?;
        storage.spatial_hash_storage.flush()?;

        Ok(())
    }

    if let Some(storage) = storage {
        // An empty set we can use to write to the database in the case that there are no entities in this space.
        let default_entity_set = HashSet::new();

        for (chunk_entity, chunk_position) in chunks.iter() {
            commands.entity(chunk_entity).remove::<ToSaveSpatial>();

            let entity_set = if let Some(entity_set) =
                spatial_entity_tracker.get_cell_entities(&chunk_position.into())
            {
                entity_set
            } else {
                // We still need to save something, otherwise we'll think this chunk hadn't been generated when we go to load the world.
                &default_entity_set
            };

            if let Err(error) = save_spatial_hash(
                &mut commands,
                entity_set,
                &spatial_entities,
                chunk_position,
                &storage,
            ) {
                log::error!(
                    "Failed to save spatial hash for position {}: {:?}",
                    chunk_position.index,
                    error
                );
            }
        }
    }
}

pub struct DataLoader<'a> {
    type_id: EntityTypeId,
    tracer_id: TracerId,
    database: &'a sled::Db,
    bytes: &'a [u8],
}

impl<'a> DataLoader<'a> {
    pub fn load<D: DeserializeOwned>(self) -> Result<(Storable, D)> {
        Ok((
            Storable { id: self.tracer_id },
            ciborium::from_reader(self.bytes).with_context(|| {
                format!(
                    "Failed to deserialize entity {} of type {}",
                    self.tracer_id, self.type_id,
                )
            })?,
        ))
    }

    pub fn load_with_tree<D: DeserializeOwned>(self) -> Result<(Storable, D, sled::Tree)> {
        let storable = Storable { id: self.tracer_id };
        let deserialized = ciborium::from_reader(self.bytes).with_context(|| {
            format!(
                "Failed to deserialize entity {} of type {}",
                self.tracer_id, self.type_id,
            )
        })?;

        let tree_name = tree_name_for_entity(storable.id);
        let tree = self.database.open_tree(tree_name)?;

        Ok((storable, deserialized, tree))
    }
}

pub struct DataSaver<'a> {
    result: &'a mut Result<()>,
    storage: &'a mut Vec<u8>,
    tracer_id: &'a mut TracerId,
    entity: &'a mut Entity,
}

impl<'a> DataSaver<'a> {
    pub fn save<S: Serialize>(self, entity: Entity, storable: &Storable, to_serialize: &S) {
        *self.result =
            ciborium::into_writer(to_serialize, self.storage).context("Failed to serialize entity");
        *self.tracer_id = storable.id;
        *self.entity = entity;
    }
}

pub type EntityTypeId = u16;

pub trait SpatialEntity<Q: WorldQuery> {
    const TYPE_ID: EntityTypeId;
    const BOOTSTRAP: BootstrapEntityInfo = BootstrapEntityInfo::NonBootstrap;

    fn load(data_loader: DataLoader, parent: Entity, commands: &mut Commands) -> Result<()>;
    fn save(query: <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'_>, data_saver: DataSaver);
}

#[derive(Resource)]
pub struct EntitySerializationManager {
    #[allow(clippy::type_complexity)]
    entity_loaders: HashMap<
        EntityTypeId,
        &'static (dyn Fn(TracerId, &[u8], &sled::Db, Entity, &mut Commands) -> Result<()> + Sync),
    >,
}

impl EntitySerializationManager {
    pub fn register<E, Q>(app: &mut App)
    where
        E: SpatialEntity<Q> + Component + 'static,
        Q: WorldQuery + 'static,
    {
        fn save_system<E, Q>(
            mut commands: Commands,
            storage: Option<Res<EntityStorage>>,
            query: Query<Q, (With<E>, With<ToSaveSpatial>)>,
        ) where
            E: SpatialEntity<Q> + Component,
            Q: WorldQuery,
        {
            if let Some(storage) = storage {
                for to_save in query.iter() {
                    let mut bytes: Vec<u8> = Vec::new();
                    bytes.extend(E::TYPE_ID.to_le_bytes().iter());

                    let mut save_result = Err(anyhow::anyhow!("Entity was never saved."));
                    let mut tracer_id = 0;
                    let mut entity = Entity::PLACEHOLDER;

                    let data_saver = DataSaver {
                        result: &mut save_result,
                        storage: &mut bytes,
                        tracer_id: &mut tracer_id,
                        entity: &mut entity,
                    };

                    E::save(to_save, data_saver);
                    commands.entity(entity).remove::<ToSaveSpatial>();

                    if let Err(error) = save_result {
                        log::error!(
                            "Failed to serialize entity {} of type {}: {:?}",
                            tracer_id,
                            E::TYPE_ID,
                            error
                        );
                    }

                    if let Err(error) = storage
                        .spatial_entities_storage
                        .insert(tracer_id.to_be_bytes(), bytes)
                    {
                        log::error!(
                            "Failed to write entity {} of type {} to database: {:?}",
                            tracer_id,
                            E::TYPE_ID,
                            error
                        );
                    }
                }
            }
        }

        fn load_setup_system<E, Q>(mut serialization_manager: ResMut<EntitySerializationManager>)
        where
            E: SpatialEntity<Q>,
            Q: WorldQuery,
        {
            let is_not_duplicate = serialization_manager
                .entity_loaders
                .insert(
                    E::TYPE_ID,
                    &|tracer_id, bytes, database, parent, commands| {
                        let data_loader = DataLoader {
                            type_id: E::TYPE_ID,
                            tracer_id,
                            database,
                            bytes,
                        };

                        E::load(data_loader, parent, commands)?;

                        Ok(())
                    },
                )
                .is_none();
            assert!(is_not_duplicate, "Duplicate entity type ID detected.");
        }

        app.add_systems(Update, save_system::<E, Q>.in_set(SaveSystemSet));
        app.add_systems(
            Startup,
            load_setup_system::<E, Q>.in_set(SerializationSetup),
        );
    }

    fn load_entity(
        &self,
        tracer_id: TracerId,
        storage: &EntityStorage,
        parent: Entity,
        commands: &mut Commands,
    ) -> Result<()> {
        // Don't load if already loaded.
        if !storage.tracers_to_entities.contains_key(&tracer_id) {
            let bytes = storage
                .spatial_entities_storage
                .get(tracer_id.to_be_bytes())
                .context("Failed to read from database.")?
                .context("Failed to find entity in database.")?;

            const TYPE_LENGTH: usize = std::mem::size_of::<EntityTypeId>();
            let mut type_id = [0u8; TYPE_LENGTH];
            type_id.copy_from_slice(
                bytes
                    .get(..TYPE_LENGTH)
                    .context("Unexpected end of data.")?,
            );
            let type_id = EntityTypeId::from_le_bytes(type_id);
            let payload = &bytes[TYPE_LENGTH..];

            let entity_loader = self
                .entity_loaders
                .get(&type_id)
                .with_context(|| format!("No entity registered for type {}.", type_id))?;

            entity_loader(tracer_id, payload, &storage.database, parent, commands)?;

            // The table `storage.tracers_to_entities` and its reciprocal will automatically be updated by an external system later,
            // so we don't have to do it here.
        }

        Ok(())
    }
}

fn load_entities(
    mut commands: Commands,
    world_entity: Option<Res<WorldEntity>>,
    terrain_chunks: Query<(Entity, &ChunkPosition), With<ToLoadSpatial>>,
    storage: Option<Res<EntityStorage>>,
    serialization_manager: Res<EntitySerializationManager>,
) {
    fn load_spatial_hash(
        position: &ChunkPosition,
        storage: &EntityStorage,
    ) -> Result<Option<Vec<TracerId>>> {
        let chunk_position_encoding = position.to_database_key();

        let entity_set_bytes = storage
            .spatial_hash_storage
            .get(chunk_position_encoding)
            .context("Failed to read chunk entity list from database.")?;

        if let Some(entity_set_bytes) = entity_set_bytes {
            let entity_set = ciborium::from_reader(&*entity_set_bytes)
                .context("Failed to deserialize chunk entity list from database.")?;

            Ok(Some(entity_set))
        } else {
            Ok(None)
        }
    }

    if let (Some(storage), Some(world_entity)) = (storage, world_entity) {
        for (chunk_entity, chunk_position) in terrain_chunks.iter() {
            match load_spatial_hash(chunk_position, &storage) {
                Ok(spatial_hash) => {
                    if let Some(entities) = spatial_hash {
                        // We now have a list of tracer IDs for entities that belong in this chunk.

                        for tracer_id in entities {
                            if let Err(error) = serialization_manager.load_entity(
                                tracer_id,
                                &storage,
                                world_entity.entity,
                                &mut commands,
                            ) {
                                log::error!("Failed to load entity {}: {:?}", tracer_id, error);
                            }
                        }
                        // Make sure we don't try to load it a second time.
                        commands.entity(chunk_entity).remove::<ToLoadSpatial>();
                    } else {
                        // Looks like we have to generate it.
                        commands
                            .entity(chunk_entity)
                            .insert(ToGenerateSpatial)
                            .remove::<ToLoadSpatial>();
                    }
                }
                Err(error) => log::error!(
                    "Failed to load spatial hash for chunk {}: {:?}",
                    chunk_position.index,
                    error
                ),
            }
        }
    }
}

#[derive(Resource)]
struct EntitySaveTimer {
    time: usize,
}

fn save_timer(
    mut commands: Commands,
    mut save_timer: ResMut<EntitySaveTimer>,
    chunks: Query<Entity, With<ChunkPosition>>,
) {
    save_timer.time += 1;

    if save_timer.time >= CHUNK_TIME_TO_SAVE {
        save_timer.time -= CHUNK_TIME_TO_SAVE;

        for chunk in chunks.iter() {
            commands.entity(chunk).insert(ToSaveSpatial);
        }
    }
}

fn save_on_shutdown(
    mut commands: Commands,
    mut events: EventReader<AppExit>,
    chunks: Query<Entity, With<ChunkPosition>>,
) {
    if events.iter().next().is_some() {
        // Save! Save everything!
        for chunk in chunks.iter() {
            commands.entity(chunk).insert(ToSaveSpatial);
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, SystemSet)]
struct SerializationSetup;

#[derive(Debug, PartialEq, Eq, Hash, Clone, SystemSet)]
struct SaveSystemSet;

fn setup(mut commands: Commands) {
    commands.insert_resource(EntitySerializationManager {
        entity_loaders: HashMap::new(),
    });

    commands.insert_resource(EntitySaveTimer { time: 0 });
}

pub(super) fn register_storage(app: &mut App) {
    app.configure_set(PostUpdate, SaveSystemSet);
    app.configure_set(Startup, SerializationSetup);

    app.add_systems(
        Startup,
        (
            setup.before(SerializationSetup),
            apply_deferred.after(setup).before(SerializationSetup),
        ),
    );

    app.add_systems(
        Update,
        (
            new_tracings,
            clean_up_tracings,
            load_entities,
            save_spatial_hashes,
            save_bootstrap_entity_list,
            bootstrap_entities,
        ),
    );

    app.add_systems(FixedUpdate, save_timer.before(SaveSystemSet));

    app.add_systems(PostUpdate, save_on_shutdown.before(SaveSystemSet));
}
