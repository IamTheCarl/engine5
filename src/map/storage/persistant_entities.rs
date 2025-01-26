use anyhow::{Context, Result};
use bincode::DefaultOptions;
use serde::{de::DeserializeSeed, Serialize};
use std::{
    collections::{HashMap, HashSet},
    num::NonZeroU32,
    path::Path,
};

use bevy::{
    prelude::*,
    reflect::TypeRegistry,
    scene::{
        serde::{EntitySerializer, SceneEntityDeserializer},
        DynamicEntity,
    },
};

/// Marks an entity as a persistant entity and attaches an ID to it.
/// This ID is to be consistent between play sessions and multiplayer instances.
#[derive(Component, Debug, Eq, PartialEq)]
pub struct Persistant(PersistantId);

/// Marks an entity as needing to be saved to the database.
#[derive(Component)]
pub struct ToSave;

/// Marks an entity as needing to be deleted from the database.
#[derive(Component)]
pub struct ToDelete;

/// An ID for an entity that persists between sessions (saved/loaded from saves or over
/// multi-player).
pub type PersistantId = NonZeroU32;

/// Requests that a persistant entity to be loaded.
#[derive(Event, Debug)]
pub struct RequestLoad(pub PersistantId);

#[derive(Resource, Default)]
pub struct PersistantEntityTracker {
    persistant_to_entities: HashMap<PersistantId, Entity>,
    entities_to_persistant: HashMap<Entity, PersistantId>,
}

impl PersistantEntityTracker {
    pub(super) fn insert(
        new_entities: Query<(Entity, &Persistant), Added<Persistant>>,
        mut tracker: ResMut<Self>,
    ) {
        for (entity, persistant) in new_entities.iter() {
            tracker.insert_association(persistant.0, entity);
        }
    }

    pub(super) fn remove(mut removals: RemovedComponents<Persistant>, mut tracker: ResMut<Self>) {
        for removed_entity in removals.read() {
            let persistant_id = tracker
                .entities_to_persistant
                .remove(&removed_entity)
                .expect("Wanderer was not being tracked.");

            let entity = tracker
                .persistant_to_entities
                .remove(&persistant_id)
                .expect("Wanderer was not being tracked.");

            assert_eq!(
                entity, removed_entity,
                "Entity did not match its persistant id."
            );
        }
    }

    fn insert_association(&mut self, persistant_id: PersistantId, entity_id: Entity) {
        self.persistant_to_entities.insert(persistant_id, entity_id);
        self.entities_to_persistant.insert(entity_id, persistant_id);
    }

    pub fn get_entity(&self, persistant_id: PersistantId) -> Option<Entity> {
        self.persistant_to_entities.get(&persistant_id).copied()
    }
}

/// Indicates the result of loading a persistant entity.
#[derive(Event, Debug)]
pub struct LoadResult {
    /// The persistant ID
    pub id: PersistantId,

    /// The result of loading the persistant.
    pub status: LoadStatus,
}

#[derive(Debug)]
pub enum LoadStatus {
    /// The entity was loaded. Here's its Entity ID.
    Success(Entity),

    /// Entity is not available in the database.
    NotPresent,

    /// Something out of our control failed, like an IO error or corrupted data.
    // TODO I'm not sure we should be presenting this to other systems. It can be complex
    // to know how to properly handle this situation.
    OtherError,
}

/// Wrapper for the database used to store the map.
#[derive(Resource)]
pub struct MapStorage {
    /// Metadata, such as "which persistant entities should be loaded when bootstrapping the world".
    meta_storage: sled::Tree,

    /// Storage for persistant entities.
    persistant_entity_storage: sled::Tree,
}

impl MapStorage {
    pub fn open(path: impl AsRef<Path>) -> Result<Self> {
        let database = sled::open(path).context("Failed to open database")?;

        let meta_storage = database
            .open_tree("metadata")
            .context("Failed to open metadata tree")?;
        let persistant_entity_storage = database
            .open_tree("persistant_entities")
            .context("Failed to open persistant entity tree")?;

        Ok(Self {
            meta_storage,
            persistant_entity_storage,
        })
    }
}

/// Serializes and saves persistant entities.
pub fn save_persistant_entities(
    world: &World,
    type_registry: Res<AppTypeRegistry>,
    persistant_entities: Query<(Entity, &Persistant), With<ToSave>>,
    map_storage: Res<MapStorage>,
    mut commands: Commands,
) {
    let type_registry = type_registry.read();
    let tst = &map_storage.persistant_entity_storage;

    // TODO we should probably make a "serializable whitelist".
    for (entity, persistant) in persistant_entities.iter() {
        // TODO is it more efficent to extract multiple entities with this scene builder at once?
        let mut dynamic_scene = DynamicSceneBuilder::from_world(world)
            .allow_all_components()
            .deny_all_resources()
            .extract_entity(entity)
            .build();

        let dynamic_entity = dynamic_scene.entities.remove(0);
        let entity_serializer = EntitySerializer {
            entity: &dynamic_entity,
            registry: &type_registry,
        };

        let mut buffer = Vec::new();
        let mut serializer = bincode::Serializer::new(&mut buffer, DefaultOptions::default());
        match entity_serializer.serialize(&mut serializer) {
            Ok(()) => {
                let key = persistant.0.get().to_le_bytes();

                if let Err(error) = tst.insert(key, buffer) {
                    error!("Failed to save persitant entity to database: {error}");
                }
            }
            Err(error) => error!("Failed to serialize persistant entity: {error}"),
        }

        commands.entity(entity).remove::<ToSave>();
    }
}

/// Loads and spawns persistant entities into the map.
pub fn load_persistant_entities(
    mut load_requests: EventReader<RequestLoad>,
    type_registry: Res<AppTypeRegistry>,
    map_storage: Res<MapStorage>,
    mut commands: Commands,
) {
    let mut dynamic_entities = Vec::new();
    {
        let type_registry = type_registry.read();
        let tst = &map_storage.persistant_entity_storage;
        let mut already_loading = HashSet::new();

        for to_load in load_requests.read() {
            let id = to_load.0;

            // Don't load it if it's already loaded.
            if already_loading.insert(id) {
                let key = id.get().to_le_bytes();
                match tst.get(key) {
                    Ok(Some(payload)) => {
                        let entity_deserializer = SceneEntityDeserializer {
                            entity: Entity::PLACEHOLDER,
                            type_registry: &type_registry,
                        };

                        let mut deserializer =
                            bincode::Deserializer::from_slice(&payload, DefaultOptions::new());
                        match entity_deserializer.deserialize(&mut deserializer) {
                            Ok(dynamic_entity) => {
                                dynamic_entities.push((id, dynamic_entity));
                            }
                            Err(error) => {
                                error!("Failed to deserialize persistant entity: {error}");
                                commands.send_event(LoadResult {
                                    id,
                                    status: LoadStatus::OtherError,
                                });
                            }
                        }
                    }
                    Ok(None) => {
                        commands.send_event(LoadResult {
                            id,
                            status: LoadStatus::NotPresent,
                        });
                    }
                    Err(error) => {
                        error!("Failed to load persistant entity: {error}");
                        commands.send_event(LoadResult {
                            id,
                            status: LoadStatus::OtherError,
                        });
                    }
                }
            }
        }
    }

    let type_registry = type_registry.clone();
    commands.queue(move |world: &mut World| {
        let type_registry = type_registry.read();

        for (id, dynamic_entity) in dynamic_entities {
            match write_dynamic_entity_to_world(dynamic_entity, id, &type_registry, world) {
                Ok(entity) => {
                    world
                        .resource_mut::<PersistantEntityTracker>()
                        .insert_association(id, entity);
                    world.send_event(LoadResult {
                        id,
                        status: LoadStatus::Success(entity),
                    });
                }
                Err(error) => {
                    error!("Failed to write dynamic entity to world: {error:?}");
                    world.send_event(LoadResult {
                        id,
                        status: LoadStatus::OtherError,
                    });
                }
            }
        }
    });
}

/// Writing an entity into the world is kinda complex and has a lot of ways to fail, so this
/// function just cleanly wraps that up.
fn write_dynamic_entity_to_world(
    dynamic_entity: DynamicEntity,
    persistant_id: PersistantId,
    type_registry: &TypeRegistry,
    world: &mut World,
) -> Result<Entity> {
    let mut entity_world_mut = world.spawn(Persistant(persistant_id));

    // Oh yeah, this is totally just a re-implementation of how Bevy's scene deserialization works.
    // https://docs.rs/bevy_scene/0.15.1/src/bevy_scene/dynamic_scene.rs.html#87
    for component in dynamic_entity.components {
        let type_info = component
            .get_represented_type_info()
            .context("Could not get represented type info")?;

        let registration = type_registry
            .get(type_info.type_id())
            .context("Type is not registered")?;

        let reflect_component = registration
            .data::<ReflectComponent>()
            .context("Could not get reflection for component")?;

        reflect_component.apply_or_insert(
            &mut entity_world_mut,
            component.as_partial_reflect(),
            type_registry,
        );
    }

    Ok(entity_world_mut.id())
}

/// Deletes persistant entities from the database and then despawns the entity.
pub fn delete_persistant_entities(
    to_delete: Query<(Entity, &Persistant), With<ToDelete>>,
    map_storage: Res<MapStorage>,
    mut commands: Commands,
) {
    let tst = &map_storage.persistant_entity_storage;
    for (entity, to_delete) in to_delete.iter() {
        let id = to_delete.0;
        let key = id.get().to_le_bytes();

        if let Err(error) = tst.remove(key) {
            // This indicates something like an IO error, not that it wasn't present.
            error!("Failed to remove persistant entity from database: {error}");
        }

        // We assume all child entities have already been despawned.
        commands.entity(entity).despawn();
    }
}

pub trait DeletePersistantEntities {
    fn delete_persistant(&mut self);
}

impl DeletePersistantEntities for EntityCommands<'_> {
    fn delete_persistant(&mut self) {
        self.retain::<Persistant>().insert(ToDelete);
    }
}

pub trait GetPersistant {
    fn persistant(
        &mut self,
        tracker: &PersistantEntityTracker,
        persistant_id: PersistantId,
    ) -> EntityCommands<'_> {
        self.get_persistant(tracker, persistant_id)
            .expect("Wanderer did not exist")
    }
    fn get_persistant(
        &mut self,
        tracker: &PersistantEntityTracker,
        persistant_id: PersistantId,
    ) -> Option<EntityCommands<'_>>;
}

impl GetPersistant for Commands<'_, '_> {
    fn get_persistant(
        &mut self,
        tracker: &PersistantEntityTracker,
        persistant_id: PersistantId,
    ) -> Option<EntityCommands<'_>> {
        let entity = tracker.get_entity(persistant_id)?;
        self.get_entity(entity)
    }
}

#[cfg(test)]
mod test {
    use outdir_tempdir::TempDir;

    use super::*;
    use crate::map::MapPlugin;

    /// Spawns an entity, saves it to the database, deletes the entity, then restores it from the
    /// database.
    #[test]
    fn save_persistant_entities() {
        let mut app = App::new();
        app.add_plugins((MapPlugin,));

        let tempdir = TempDir::new();

        let persistant_id = PersistantId::new(1).unwrap();
        let transform = Transform::from_translation(Vec3::new(1.0, 2.0, 3.0));

        {
            let world = app.world_mut();
            world.insert_resource(
                MapStorage::open(tempdir.path().join("save_persistant_entities")).unwrap(),
            );

            world.spawn((Persistant(persistant_id), ToSave, transform));
        };

        app.update();

        {
            let world = app.world_mut();
            world.clear_entities();
            world.send_event(RequestLoad(persistant_id));
        }

        app.update();

        // The GlobalTransform component is required by Transform and should have been added
        // automatically.
        let mut entities = app
            .world_mut()
            .query_filtered::<(&Persistant, &Transform), With<GlobalTransform>>();

        let entities = entities.iter(app.world_mut()).collect::<Vec<_>>();

        assert_eq!(entities, vec![(&Persistant(persistant_id), &transform)]);
    }

    /// Tracks the persistant_id<->entity association.
    #[test]
    fn persistant_entity_tracking() {
        let mut app = App::new();
        app.add_plugins((MapPlugin,));

        let persistant_id = PersistantId::new(1).unwrap();

        let entity_id = {
            let world = app.world_mut();
            world.spawn(Persistant(persistant_id)).id()
        };

        app.update();

        {
            let world = app.world();
            let tracker: &PersistantEntityTracker = world.resource();

            assert_eq!(
                tracker.persistant_to_entities,
                HashMap::from([(persistant_id, entity_id)])
            );
            assert_eq!(
                tracker.entities_to_persistant,
                HashMap::from([(entity_id, persistant_id)])
            );
        }

        {
            let world = app.world_mut();
            world.despawn(entity_id);
        }

        app.update();
        {
            let world = app.world();
            let tracker: &PersistantEntityTracker = world.resource();

            assert_eq!(tracker.persistant_to_entities, HashMap::from([]));
            assert_eq!(tracker.entities_to_persistant, HashMap::from([]));
        }
    }

    /// Delete a persistant entity from the database and then prove it can't be loaded anymore.
    #[test]
    fn delete_persistant_entities() {
        let mut app = App::new();
        app.add_plugins((MapPlugin,));

        let tempdir = TempDir::new();

        let persistant_id = PersistantId::new(1).unwrap();
        let transform = Transform::from_translation(Vec3::new(1.0, 2.0, 3.0));

        {
            let world = app.world_mut();
            world.insert_resource(
                MapStorage::open(tempdir.path().join("delete_persistant_entity")).unwrap(),
            );

            world.spawn((Persistant(persistant_id), ToSave, transform));
        };

        app.update();

        {
            let world = app.world_mut();
            world.clear_entities();
            world.send_event(RequestLoad(persistant_id));
        }

        app.update();

        // The GlobalTransform component is required by Transform and should have been added
        // automatically.
        let mut persistants = app
            .world_mut()
            .query_filtered::<(&Persistant, &Transform), With<GlobalTransform>>();

        let persistant_entity_list = persistants.iter(app.world_mut()).collect::<Vec<_>>();
        assert_eq!(
            persistant_entity_list,
            vec![(&Persistant(persistant_id), &transform)]
        );

        {
            let world = app.world_mut();
            let delete_system = world.register_system(
                move |tracker: Res<PersistantEntityTracker>, mut commands: Commands| {
                    commands
                        .persistant(&tracker, persistant_id)
                        .delete_persistant();
                },
            );

            world.run_system(delete_system).unwrap();
        }

        app.update();

        let mut persistants = app.world_mut().query_filtered::<(), With<Persistant>>();
        let persistant_entity_list = persistants.iter(app.world_mut()).collect::<Vec<_>>();
        assert_eq!(persistant_entity_list, vec![]);

        let does_not_exist_in_database = app
            .world()
            .resource::<MapStorage>()
            .persistant_entity_storage
            .get(persistant_id.get().to_le_bytes())
            .unwrap()
            .is_none();

        assert!(does_not_exist_in_database);
    }

    /// Try to load a persistant entity twice. Only one instance should be loaded.
    #[test]
    fn double_load_persistant_entity() {
        let mut app = App::new();
        app.add_plugins((MapPlugin,));

        let tempdir = TempDir::new();

        let persistant_id = PersistantId::new(1).unwrap();
        let transform = Transform::from_translation(Vec3::new(1.0, 2.0, 3.0));

        {
            let world = app.world_mut();
            world.insert_resource(
                MapStorage::open(tempdir.path().join("double_load_persistant_entity")).unwrap(),
            );

            world.spawn((Persistant(persistant_id), ToSave, transform));
        };

        app.update();

        {
            let world = app.world_mut();
            world.clear_entities();

            // Look, we tried to load the entity twice!
            world.send_event(RequestLoad(persistant_id));
            world.send_event(RequestLoad(persistant_id));
        }

        app.update();

        // The GlobalTransform component is required by Transform and should have been added
        // automatically.
        let mut entities = app
            .world_mut()
            .query_filtered::<(&Persistant, &Transform), With<GlobalTransform>>();

        let entities = entities.iter(app.world_mut()).collect::<Vec<_>>();

        assert_eq!(entities, vec![(&Persistant(persistant_id), &transform)]);
    }
}
