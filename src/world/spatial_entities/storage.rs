use anyhow::{Context, Result};
use bevy::{
    ecs::{query::WorldQuery, system::EntityCommands},
    prelude::*,
};
use bytes::Bytes;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    mem::size_of,
    sync::{
        atomic::{AtomicBool, Ordering},
        Mutex,
    },
};

use crate::{
    multiplayer::{ClientContext, EntityUpdate, HostContext, ServerChannels, ToTransmitEntity},
    world::{
        generation::ToGenerateSpatial,
        terrain::{storage::CHUNK_TIME_TO_SAVE, ChunkPosition},
        WorldEntity,
    },
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

impl Storable {
    pub fn id(&self) -> TracerId {
        self.id
    }
}

#[derive(Serialize, Deserialize, Reflect, Clone, Copy, Debug)]
pub enum BootstrapEntityInfo {
    NonBootstrap,
    LocalPlayer,
    GlobalTerrain,
    RemotePlayer,
    PlayerSpawner,
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

#[derive(Resource, Default)]
pub struct EntityTracer {
    tracers_to_entities: HashMap<TracerId, Entity>,
    entities_to_tracers: HashMap<Entity, TracerId>,
}

impl EntityTracer {
    /// Gets the Entity ID for a traceable object.
    /// Returning None doesn't mean the entity doesn't exist anymore. It's just not loaded.
    pub fn get_entity(&self, trace_id: Storable) -> Option<Entity> {
        self.tracers_to_entities.get(&trace_id.id).copied()
    }

    pub fn insert_entity(&mut self, tracer_id: TracerId, entity: Entity) {
        self.tracers_to_entities.insert(tracer_id, entity);
        self.entities_to_tracers.insert(entity, tracer_id);
    }
}

#[derive(Resource)]
pub struct EntityStorage {
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

    pub fn bootstrap_list(&self) -> Vec<TracerId> {
        let bootstrap_list = self
            .bootstrap_list
            .lock()
            .expect("Bootstrap list has been poisoned!");

        bootstrap_list.bootstrap_entities.keys().copied().collect()
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

    pub fn new_storable_component<E, Q, U, P>(&self) -> Result<Storable>
    where
        E: SpatialEntity<Q, U, P>,
        Q: WorldQuery,
        U: WorldQuery,
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

    pub fn new_storable_component_with_tree<E, Q, U, P>(&self) -> Result<(Storable, sled::Tree)>
    where
        E: SpatialEntity<Q, U, P>,
        Q: WorldQuery,
        U: WorldQuery,
    {
        let storable = self.new_storable_component::<E, Q, U, P>()?;
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

    /// An entity may need to store complicated data and manipulate it on the fly. You can use this to gain
    /// access to a tree to do that with. You can't create namespaces within the tree, so you'll need to use
    /// a prefix to keep your component's entries from stomping over the entries of other components.
    pub fn get_entity_tree(&self, storable: &Storable) -> Result<sled::Tree> {
        let tree = self.database.open_tree(format!("entity_{}", storable.id))?;
        Ok(tree)
    }
}

pub trait EntityConstruction<P> {
    const REQUEST_TREE: bool = false;

    fn construct_entity(parameters: P, commands: &mut EntityCommands);
    fn construct_entity_with_tree(parameters: P, commands: &mut EntityCommands, _tree: sled::Tree) {
        Self::construct_entity(parameters, commands);
    }
}

fn new_tracings(
    new_tracings: Query<(Entity, &Storable), Changed<Storable>>,
    mut tracer: ResMut<EntityTracer>,
) {
    for (entity_id, traceable) in new_tracings.iter() {
        let tracer_id = traceable.id;

        tracer.insert_entity(tracer_id, entity_id);
    }
}

fn clean_up_tracings(mut removed: RemovedComponents<Storable>, mut tracer: ResMut<EntityTracer>) {
    for entity_id in removed.iter() {
        if let Some(tracker_id) = tracer.entities_to_tracers.remove(&entity_id) {
            tracer.tracers_to_entities.remove(&tracker_id);
        } else {
            log::warn!("Removed traceable without an associated entity.");
        }
    }
}

fn save_bootstrap_entity_list(storage: Res<EntityStorage>) {
    if let Err(error) = storage.save_bootstrap_entities_list() {
        log::error!("Failed to save bootstrap entities list: {:?}", error);
    }
}

fn bootstrap_entities(
    mut commands: Commands,
    storage: Res<EntityStorage>,
    tracer: Res<EntityTracer>,
    serialization_manager: Res<EntitySerializationManager>,
    world_entity: Res<WorldEntity>,
) {
    if !storage.bootstrap_complete.swap(true, Ordering::SeqCst) {
        let bootstrap_list = storage
            .bootstrap_list
            .lock()
            .expect("Bootstrap list has been poisoned!");
        for tracer_id in bootstrap_list.bootstrap_entities.keys() {
            if let Err(error) = serialization_manager.load_entity(
                *tracer_id,
                &tracer,
                &storage,
                world_entity.entity,
                &mut commands,
            ) {
                log::error!("Failed to load entity {}: {:?}", tracer_id, error);
            }
        }
    }
}

fn save_spatial_hashes(
    mut commands: Commands,
    spatial_entity_tracker: Res<SpatialEntityTracker>,
    spatial_entities: Query<(Entity, &Storable)>,
    chunks: Query<(Entity, &ChunkPosition), With<ToSaveSpatial>>,
    storage: Res<EntityStorage>,
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

pub trait DataLoader {
    fn load<D: DeserializeOwned>(self) -> Result<D>;
    fn load_with_tree<D: DeserializeOwned>(self) -> Result<(D, Option<sled::Tree>)>;
}

pub struct LocalDataLoader<'a> {
    type_id: EntityTypeId,
    tracer_id: TracerId,
    database: &'a sled::Db,
    bytes: &'a [u8],
}

impl<'a> DataLoader for LocalDataLoader<'a> {
    fn load<D: DeserializeOwned>(self) -> Result<D> {
        ciborium::from_reader(self.bytes).with_context(|| {
            format!(
                "Failed to deserialize entity {} of type {}",
                self.tracer_id, self.type_id,
            )
        })
    }

    fn load_with_tree<D: DeserializeOwned>(self) -> Result<(D, Option<sled::Tree>)> {
        let storable = Storable { id: self.tracer_id };
        let deserialized = ciborium::from_reader(self.bytes).with_context(|| {
            format!(
                "Failed to deserialize entity {} of type {}",
                self.tracer_id, self.type_id,
            )
        })?;

        let tree_name = tree_name_for_entity(storable.id);
        let tree = self.database.open_tree(tree_name)?;

        Ok((deserialized, Some(tree)))
    }
}

struct RemoteDataLoader {
    type_id: EntityTypeId,
    tracer_id: TracerId,
    bytes: Bytes,
}

impl DataLoader for RemoteDataLoader {
    fn load<D: DeserializeOwned>(self) -> Result<D> {
        bincode::deserialize(&self.bytes).with_context(|| {
            format!(
                "Failed to deserialize entity {} of type {}",
                self.tracer_id, self.type_id,
            )
        })
    }

    fn load_with_tree<D: DeserializeOwned>(self) -> Result<(D, Option<sled::Tree>)> {
        let deserialized = self.load()?;

        Ok((deserialized, None))
    }
}

pub trait DataSaver {
    fn save<S: Serialize>(self, entity: Entity, storable: &Storable, to_serialize: S);
}

pub struct LocalDataSaver<'a> {
    result: &'a mut Result<()>,
    storage: &'a mut Vec<u8>,
    tracer_id: &'a mut TracerId,
    entity: &'a mut Entity,
}

impl<'a> DataSaver for LocalDataSaver<'a> {
    fn save<S: Serialize>(self, entity: Entity, storable: &Storable, to_serialize: S) {
        *self.result = ciborium::into_writer(&to_serialize, self.storage)
            .context("Failed to serialize entity");
        *self.tracer_id = storable.id;
        *self.entity = entity;
    }
}

pub struct RemoteDataSaver<'a> {
    result: &'a mut Result<()>,
    storage: &'a mut Vec<u8>,
    tracer_id: &'a mut TracerId,
    entity: &'a mut Entity,
}

impl<'a> DataSaver for RemoteDataSaver<'a> {
    fn save<S: Serialize>(self, entity: Entity, storable: &Storable, to_serialize: S) {
        *self.result = bincode::serialize_into(self.storage, &to_serialize)
            .context("Failed to serialize entity");
        *self.tracer_id = storable.id;
        *self.entity = entity;
    }
}

pub type EntityTypeId = u32;

pub trait SpatialEntity<Q: WorldQuery, U: WorldQuery, P>: EntityConstruction<P> {
    const TYPE_ID: EntityTypeId;
    const BOOTSTRAP: BootstrapEntityInfo = BootstrapEntityInfo::NonBootstrap;

    fn load(data_loader: impl DataLoader, commands: &mut EntityCommands) -> Result<()>;
    fn update(data_loader: impl DataLoader, query: <U as WorldQuery>::Item<'_>) -> Result<()>;
    fn save(
        query: <<Q as WorldQuery>::ReadOnly as WorldQuery>::Item<'_>,
        data_saver: impl DataSaver,
    );
}

#[derive(Resource)]
pub struct EntitySerializationManager {
    #[allow(clippy::type_complexity)]
    entity_loaders: HashMap<
        EntityTypeId,
        &'static (dyn Fn(TracerId, &[u8], &sled::Db, Entity, &mut Commands) -> Result<()> + Sync),
    >,
    #[allow(clippy::type_complexity)]
    entity_receivers: HashMap<
        EntityTypeId,
        &'static (dyn Fn(TracerId, Bytes, EntityCommands) -> Result<()> + Sync),
    >,
}

impl EntitySerializationManager {
    pub fn register<E, Q, U, P: 'static>(app: &mut App)
    where
        E: SpatialEntity<Q, U, P> + Component + 'static,
        Q: WorldQuery + 'static,
        U: WorldQuery + 'static,
    {
        fn save_system<E, Q, U, P>(
            mut commands: Commands,
            storage: Res<EntityStorage>,
            query: Query<Q, (With<E>, With<ToSaveSpatial>)>,
        ) where
            E: SpatialEntity<Q, U, P> + Component,
            Q: WorldQuery,
            U: WorldQuery,
        {
            for to_save in query.iter() {
                let mut bytes: Vec<u8> = Vec::new();
                bytes.extend(E::TYPE_ID.to_le_bytes().iter());

                let mut save_result = Err(anyhow::anyhow!("Entity was never saved."));
                let mut tracer_id = 0;
                let mut entity = Entity::PLACEHOLDER;

                let data_saver = LocalDataSaver {
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

        fn transmit_system<E, Q, U, P>(
            mut commands: Commands,
            mut host: ResMut<HostContext>,
            to_transmit: Query<(Entity, &ToTransmitEntity), With<E>>,
            entities: Query<Q, With<ToTransmitEntity>>,
        ) where
            E: SpatialEntity<Q, U, P> + Component,
            Q: WorldQuery,
            U: WorldQuery,
        {
            // TODO this could be made more efficient if we generated a "transmit" function that automatically
            // includes the ToTransmitEntity component.
            for (entity_to_transmit, target_clients) in to_transmit.iter() {
                // First we need to serialize the entity.
                let to_transmit = entities
                    .get(entity_to_transmit)
                    .expect("Transmittable entity was missing necessary component(s).");

                let message = EntityUpdate::serialize(|bytes| {
                    let mut save_result = Err(anyhow::anyhow!("Entity was never saved."));
                    let mut tracer_id = 0;

                    let mut entity = Entity::PLACEHOLDER;
                    let data_saver = RemoteDataSaver {
                        result: &mut save_result,
                        storage: bytes,
                        tracer_id: &mut tracer_id,
                        entity: &mut entity,
                    };

                    E::save(to_transmit, data_saver);

                    Ok((tracer_id, E::TYPE_ID))
                });

                match message {
                    Ok(message) => {
                        for client_id in target_clients.clients.iter() {
                            host.server.send_message(
                                *client_id,
                                ServerChannels::UpdateEntity,
                                message.clone(),
                            );
                        }

                        commands
                            .entity(entity_to_transmit)
                            .remove::<ToTransmitEntity>();
                    }
                    Err(error) => {
                        log::error!("Failed to serialize entity for transmission: {:?}", error)
                    }
                }
            }
        }

        fn setup_deserialization<E, Q, U, P>(
            mut serialization_manager: ResMut<EntitySerializationManager>,
        ) where
            E: SpatialEntity<Q, U, P>,
            Q: WorldQuery,
            U: WorldQuery,
        {
            let is_not_duplicate = serialization_manager
                .entity_loaders
                .insert(
                    E::TYPE_ID,
                    &|tracer_id, bytes, database, parent, commands| {
                        let data_loader = LocalDataLoader {
                            type_id: E::TYPE_ID,
                            tracer_id,
                            database,
                            bytes,
                        };

                        let mut entity_commands = commands.spawn(Storable { id: tracer_id });
                        entity_commands.set_parent(parent);

                        E::load(data_loader, &mut entity_commands)?;

                        Ok(())
                    },
                )
                .is_none();
            assert!(is_not_duplicate, "Duplicate entity type ID detected.");

            serialization_manager.entity_receivers.insert(
                E::TYPE_ID,
                &|tracer_id, bytes, mut commands| {
                    let data_loader = RemoteDataLoader {
                        type_id: E::TYPE_ID,
                        tracer_id,
                        bytes,
                    };

                    E::load(data_loader, &mut commands)?;

                    Ok(())
                },
            );
        }

        app.add_systems(
            Update,
            (
                save_system::<E, Q, U, P>
                    .run_if(resource_exists::<EntityStorage>())
                    .in_set(SaveSystemSet),
                transmit_system::<E, Q, U, P>
                    .run_if(resource_exists::<HostContext>())
                    .in_set(SaveSystemSet),
            ),
        );
        app.add_systems(
            Startup,
            setup_deserialization::<E, Q, U, P>.in_set(SerializationSetup),
        );
    }

    fn load_entity(
        &self,
        tracer_id: TracerId,
        tracer: &EntityTracer,
        storage: &EntityStorage,
        parent: Entity,
        commands: &mut Commands,
    ) -> Result<()> {
        // Don't load if already loaded.
        if !tracer.tracers_to_entities.contains_key(&tracer_id) {
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

    fn update_entity(
        &self,
        tracer_id: TracerId,
        update: &EntityUpdate,
        entity_tracer: &mut ResMut<EntityTracer>,
        parent: Entity,
        commands: &mut Commands,
    ) -> Result<()> {
        let reciver = self
            .entity_receivers
            .get(&update.type_id)
            .with_context(|| format!("Unknown entity type ID: {}", update.type_id))?;

        let entity_commands =
            if let Some(entity) = entity_tracer.tracers_to_entities.get(&tracer_id) {
                commands.entity(*entity)
            } else {
                let mut entity_commands = commands.spawn(Storable { id: tracer_id });
                entity_commands.set_parent(parent);

                // We insert this into the entity tracer ourselves.
                // We can't afford to wait until the system to add those does
                // its job, because we can recive multiple updates in a single frame.
                let entity_id = entity_commands.id();
                entity_tracer.insert_entity(tracer_id, entity_id);

                entity_commands
            };

        // Note that the payload clone is not a deep copy.
        reciver(tracer_id, update.payload.clone(), entity_commands)
    }
}

fn receive_entities(
    mut commands: Commands,
    world_entity: Res<WorldEntity>,
    mut entity_tracer: ResMut<EntityTracer>,
    serialization_manager: Res<EntitySerializationManager>,
    client_context: Res<ClientContext>,
) {
    for (tracer_id, entity_update) in client_context.entity_updates() {
        if let Err(error) = serialization_manager.update_entity(
            *tracer_id,
            entity_update,
            &mut entity_tracer,
            world_entity.entity,
            &mut commands,
        ) {
            log::error!("Failed to update entity {}: {:?}", tracer_id, error);
        }
    }
}

fn load_entities(
    mut commands: Commands,
    world_entity: Res<WorldEntity>,
    terrain_chunks: Query<(Entity, &ChunkPosition), With<ToLoadSpatial>>,
    storage: Res<EntityStorage>,
    entity_tracer: Res<EntityTracer>,
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

    for (chunk_entity, chunk_position) in terrain_chunks.iter() {
        match load_spatial_hash(chunk_position, &storage) {
            Ok(spatial_hash) => {
                if let Some(entities) = spatial_hash {
                    // We now have a list of tracer IDs for entities that belong in this chunk.

                    for tracer_id in entities {
                        if let Err(error) = serialization_manager.load_entity(
                            tracer_id,
                            &entity_tracer,
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, SystemSet)]
struct SerializationSetup;

#[derive(Debug, PartialEq, Eq, Hash, Clone, SystemSet)]
pub struct SaveSystemSet;

fn setup(mut commands: Commands) {
    commands.insert_resource(EntitySerializationManager {
        entity_loaders: HashMap::new(),
        entity_receivers: HashMap::new(),
    });
    commands.insert_resource(EntityTracer::default());

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
            new_tracings.run_if(resource_exists::<EntityStorage>()),
            clean_up_tracings.run_if(resource_exists::<EntityStorage>()),
            load_entities
                .run_if(resource_exists::<EntityStorage>())
                .run_if(resource_exists::<WorldEntity>()),
            save_spatial_hashes.run_if(resource_exists::<EntityStorage>()),
            save_bootstrap_entity_list.run_if(resource_exists::<EntityStorage>()),
            bootstrap_entities
                .run_if(resource_exists::<EntityStorage>())
                .run_if(resource_exists::<WorldEntity>()),
            receive_entities
                .run_if(resource_exists::<ClientContext>())
                .run_if(resource_exists::<WorldEntity>()),
        ),
    );

    app.add_systems(FixedUpdate, save_timer.before(SaveSystemSet));
}
