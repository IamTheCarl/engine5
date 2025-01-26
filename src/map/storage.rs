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

/// Plugin for handling storage of maps.
#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct StoragePlugin;

impl Plugin for StoragePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                WandererTracker::insert.before(load_wanderers),
                WandererTracker::remove.after(delete_wanderers),
            ),
        );
        app.add_systems(
            Update,
            (save_wanderers, load_wanderers, delete_wanderers)
                .run_if(resource_exists::<MapStorage>),
        );
        app.add_event::<RequestLoadWanderer>();
        app.add_event::<LoadResult>();
        app.register_type::<Transform>();
        app.insert_resource(WandererTracker::default());
    }
}

/// Marks an entity as needing to be saved to the database.
#[derive(Component)]
pub struct ToSave;

/// Marks an entity as needing to be deleted from the database.
#[derive(Component)]
struct ToDelete;

/// An ID for an entity that can move freely through the world.
pub type WandererId = NonZeroU32;

/// Requests that a wanderer be loaded.
#[derive(Event, Debug)]
pub struct RequestLoadWanderer(pub WandererId);

#[derive(Resource, Default)]
pub struct WandererTracker {
    wanderers_to_entities: HashMap<WandererId, Entity>,
    entities_to_wanderers: HashMap<Entity, WandererId>,
}

impl WandererTracker {
    fn insert(
        new_wanderers: Query<(Entity, &Wanderer), Added<Wanderer>>,
        mut tracker: ResMut<Self>,
    ) {
        for (entity, wanderer) in new_wanderers.iter() {
            tracker.insert_association(wanderer.0, entity);
        }
    }

    fn remove(mut removals: RemovedComponents<Wanderer>, mut tracker: ResMut<Self>) {
        for removed_entity in removals.read() {
            let wanderer_id = tracker
                .entities_to_wanderers
                .remove(&removed_entity)
                .expect("Wanderer was not being tracked.");

            let entity = tracker
                .wanderers_to_entities
                .remove(&wanderer_id)
                .expect("Wanderer was not being tracked.");

            assert_eq!(entity, removed_entity, "Entity did not match its wanderer.");
        }
    }

    fn insert_association(&mut self, wanderer_id: WandererId, entity_id: Entity) {
        self.wanderers_to_entities.insert(wanderer_id, entity_id);
        self.entities_to_wanderers.insert(entity_id, wanderer_id);
    }

    pub fn get_entity(&self, wanderer_id: WandererId) -> Option<Entity> {
        self.wanderers_to_entities.get(&wanderer_id).copied()
    }
}

/// Indicates the result of loading a wanderer
#[derive(Event, Debug)]
pub struct LoadResult {
    /// The wanderer ID
    pub id: WandererId,

    /// The result of loading the wanderer.
    pub status: LoadStatus,
}

#[derive(Debug)]
pub enum LoadStatus {
    /// The object was loaded. Here's its Entity ID.
    Success(Entity),

    /// Object is not available in the database.
    NotPresent,

    /// Something out of our control failed, like an IO error or corrupted data.
    // TODO I'm not sure we should be presenting this to entities. It can be complex
    // to know how to properly handle this situation.
    OtherError,
}

/// Marks an entity as a wandering entity and attaches an ID to it.
/// This ID is to be consistent between play sessions and multiplayer instances.
#[derive(Component, Debug, Eq, PartialEq)]
pub struct Wanderer(WandererId);

/// Wrapper for the database used to store the map.
#[derive(Resource)]
pub struct MapStorage {
    /// Metadata, such as "which wanderers should be loaded when bootstrapping the world".
    meta_storage: sled::Tree,

    /// Storage for wandering entities.
    wanderer_storage: sled::Tree,
}

impl MapStorage {
    pub fn open(path: impl AsRef<Path>) -> Result<Self> {
        let database = sled::open(path).context("Failed to open database")?;

        let meta_storage = database
            .open_tree("metadata")
            .context("Failed to open metadata tree")?;
        let wanderer_storage = database
            .open_tree("wanderers")
            .context("Failed to open wanderer tree")?;

        Ok(Self {
            meta_storage,
            wanderer_storage,
        })
    }
}

/// Serializes and saves wanderers.
fn save_wanderers(
    world: &World,
    type_registry: Res<AppTypeRegistry>,
    wanderers: Query<(Entity, &Wanderer), With<ToSave>>,
    map_storage: Res<MapStorage>,
    mut commands: Commands,
) {
    let type_registry = type_registry.read();
    let tst = &map_storage.wanderer_storage;

    // TODO we should probably make a "serializable whitelist".
    for (entity, wanderer) in wanderers.iter() {
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
                let key = wanderer.0.get().to_le_bytes();

                if let Err(error) = tst.insert(key, buffer) {
                    error!("Failed to save wanderer to database: {error}");
                }
            }
            Err(error) => error!("Failed to serialize wanderer: {error}"),
        }

        commands.entity(entity).remove::<ToSave>();
    }
}

/// Loads and spawns wandering entities into the map.
fn load_wanderers(
    mut load_wanderer: EventReader<RequestLoadWanderer>,
    type_registry: Res<AppTypeRegistry>,
    map_storage: Res<MapStorage>,
    mut commands: Commands,
) {
    let mut dynamic_entities = Vec::new();
    {
        let type_registry = type_registry.read();
        let tst = &map_storage.wanderer_storage;
        let mut already_loading = HashSet::new();

        for to_load in load_wanderer.read() {
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
                                error!("Failed to deserialize wanderer: {error}");
                                commands.send_event(LoadResult {
                                    id,
                                    status: LoadStatus::OtherError,
                                });
                            }
                        }

                        // We leave pushing the result to the next system, which inserts the wanderer.
                    }
                    Ok(None) => {
                        commands.send_event(LoadResult {
                            id,
                            status: LoadStatus::NotPresent,
                        });
                    }
                    Err(error) => {
                        error!("Failed to load wanderer: {error}");
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
                        .resource_mut::<WandererTracker>()
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
    wanderer_id: WandererId,
    type_registry: &TypeRegistry,
    world: &mut World,
) -> Result<Entity> {
    let mut entity_world_mut = world.spawn(Wanderer(wanderer_id));

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

/// Deletes wanderers from the database and then despawns the entity.
fn delete_wanderers(
    to_delete: Query<(Entity, &Wanderer), With<ToDelete>>,
    map_storage: Res<MapStorage>,
    mut commands: Commands,
) {
    let tst = &map_storage.wanderer_storage;
    for (entity, to_delete) in to_delete.iter() {
        let id = to_delete.0;
        let key = id.get().to_le_bytes();

        if let Err(error) = tst.remove(key) {
            error!("Failed to save wanderer to database: {error}");
        }

        // We assume all child entities have already been despawned.
        commands.entity(entity).despawn();
    }
}

pub trait DeleteWanderer {
    fn delete_wanderer(&mut self);
}

impl DeleteWanderer for EntityCommands<'_> {
    fn delete_wanderer(&mut self) {
        self.retain::<Wanderer>().insert(ToDelete);
    }
}

pub trait GetWanderer {
    fn wanderer(
        &mut self,
        tracker: &WandererTracker,
        wanderer_id: WandererId,
    ) -> EntityCommands<'_> {
        self.get_wanderer(tracker, wanderer_id)
            .expect("Wanderer did not exist")
    }
    fn get_wanderer(
        &mut self,
        tracker: &WandererTracker,
        wanderer_id: WandererId,
    ) -> Option<EntityCommands<'_>>;
}

impl GetWanderer for Commands<'_, '_> {
    fn get_wanderer(
        &mut self,
        tracker: &WandererTracker,
        wanderer_id: WandererId,
    ) -> Option<EntityCommands<'_>> {
        let entity = tracker.get_entity(wanderer_id)?;
        self.get_entity(entity)
    }
}

#[cfg(test)]
mod test {
    use outdir_tempdir::TempDir;

    use super::*;

    /// Spawns an entity, saves it to the database, deletes the entity, then restores it from the
    /// database.
    #[test]
    fn save_wanderers() {
        let mut app = App::new();
        app.add_plugins((super::super::MapPlugin,));

        let tempdir = TempDir::new();

        let wanderer_id = WandererId::new(1).unwrap();
        let transform = Transform::from_translation(Vec3::new(1.0, 2.0, 3.0));

        {
            let world = app.world_mut();
            world.insert_resource(MapStorage::open(tempdir.path().join("save_wanderers")).unwrap());

            world.spawn((Wanderer(wanderer_id), ToSave, transform));
        };

        app.update();

        {
            let world = app.world_mut();
            world.clear_entities();
            world.send_event(RequestLoadWanderer(wanderer_id));
        }

        app.update();

        // The GlobalTransform component is required by Transform and should have been added
        // automatically.
        let mut wanderers = app
            .world_mut()
            .query_filtered::<(&Wanderer, &Transform), With<GlobalTransform>>();

        let wanderers = wanderers.iter(app.world_mut()).collect::<Vec<_>>();

        assert_eq!(wanderers, vec![(&Wanderer(wanderer_id), &transform)]);
    }

    /// Tracks the wanderer<->entity association.
    #[test]
    fn wanderer_tracking() {
        let mut app = App::new();
        app.add_plugins((super::super::MapPlugin,));

        let wanderer_id = WandererId::new(1).unwrap();

        let entity_id = {
            let world = app.world_mut();
            world.spawn(Wanderer(wanderer_id)).id()
        };

        app.update();

        {
            let world = app.world();
            let tracker: &WandererTracker = world.resource();

            assert_eq!(
                tracker.wanderers_to_entities,
                HashMap::from([(wanderer_id, entity_id)])
            );
            assert_eq!(
                tracker.entities_to_wanderers,
                HashMap::from([(entity_id, wanderer_id)])
            );
        }

        {
            let world = app.world_mut();
            world.despawn(entity_id);
        }

        app.update();
        {
            let world = app.world();
            let tracker: &WandererTracker = world.resource();

            assert_eq!(tracker.wanderers_to_entities, HashMap::from([]));
            assert_eq!(tracker.entities_to_wanderers, HashMap::from([]));
        }
    }

    /// Delete a wanderer from the database and then prove it can't be loaded anymore.
    #[test]
    fn delete_wanderers() {
        let mut app = App::new();
        app.add_plugins((super::super::MapPlugin,));

        let tempdir = TempDir::new();

        let wanderer_id = WandererId::new(1).unwrap();
        let transform = Transform::from_translation(Vec3::new(1.0, 2.0, 3.0));

        {
            let world = app.world_mut();
            world.insert_resource(
                MapStorage::open(tempdir.path().join("delete_wanderers")).unwrap(),
            );

            world.spawn((Wanderer(wanderer_id), ToSave, transform));
        };

        app.update();

        {
            let world = app.world_mut();
            world.clear_entities();
            world.send_event(RequestLoadWanderer(wanderer_id));
        }

        app.update();

        // The GlobalTransform component is required by Transform and should have been added
        // automatically.
        let mut wanderers = app
            .world_mut()
            .query_filtered::<(&Wanderer, &Transform), With<GlobalTransform>>();

        let wanderers_list = wanderers.iter(app.world_mut()).collect::<Vec<_>>();
        assert_eq!(wanderers_list, vec![(&Wanderer(wanderer_id), &transform)]);

        {
            let world = app.world_mut();
            let delete_system = world.register_system(
                move |tracker: Res<WandererTracker>, mut commands: Commands| {
                    commands.wanderer(&tracker, wanderer_id).delete_wanderer();
                },
            );

            world.run_system(delete_system).unwrap();
        }

        app.update();

        let mut wanderers = app.world_mut().query_filtered::<(), With<Wanderer>>();
        let wanderers_list = wanderers.iter(app.world_mut()).collect::<Vec<_>>();
        assert_eq!(wanderers_list, vec![]);

        let does_not_exist_in_database = app
            .world()
            .resource::<MapStorage>()
            .wanderer_storage
            .get(wanderer_id.get().to_le_bytes())
            .unwrap()
            .is_none();

        assert!(does_not_exist_in_database);
    }

    /// Try to load a wanderer twice. Only one instance should be loaded.
    #[test]
    fn double_load_wanderers() {
        let mut app = App::new();
        app.add_plugins((super::super::MapPlugin,));

        let tempdir = TempDir::new();

        let wanderer_id = WandererId::new(1).unwrap();
        let transform = Transform::from_translation(Vec3::new(1.0, 2.0, 3.0));

        {
            let world = app.world_mut();
            world.insert_resource(
                MapStorage::open(tempdir.path().join("double_load_wanderers")).unwrap(),
            );

            world.spawn((Wanderer(wanderer_id), ToSave, transform));
        };

        app.update();

        {
            let world = app.world_mut();
            world.clear_entities();

            // Look, we tried to load the wanderer twice!
            world.send_event(RequestLoadWanderer(wanderer_id));
            world.send_event(RequestLoadWanderer(wanderer_id));
        }

        app.update();

        // The GlobalTransform component is required by Transform and should have been added
        // automatically.
        let mut wanderers = app
            .world_mut()
            .query_filtered::<(&Wanderer, &Transform), With<GlobalTransform>>();

        let wanderers = wanderers.iter(app.world_mut()).collect::<Vec<_>>();

        assert_eq!(wanderers, vec![(&Wanderer(wanderer_id), &transform)]);
    }
}
