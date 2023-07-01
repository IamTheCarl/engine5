use anyhow::{Context, Result};
use bevy::{ecs::system::EntityCommands, prelude::*};
use std::{collections::HashMap, mem::size_of};

use crate::world::{generation::GenerateSpatial, terrain::ChunkPosition};

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToLoadSpatial;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToSaveSpatial;

type TracerId = u64;

#[derive(Component, Reflect, Clone, Copy)]
pub struct Storable {
    id: TracerId,
}

#[derive(Component)]
pub struct SpatialEntityStorage {
    tracers_to_entities: HashMap<TracerId, Entity>,
    entities_to_tracers: HashMap<Entity, TracerId>,
    database: sled::Db,
    spatial_entities: sled::Tree,
}

impl SpatialEntityStorage {
    pub fn new(database: &sled::Db) -> Result<Self> {
        Ok(SpatialEntityStorage {
            tracers_to_entities: HashMap::new(),
            entities_to_tracers: HashMap::new(),
            database: database.clone(),
            spatial_entities: database
                .open_tree("spatial_entities")
                .context("Failed to open tree for spatial entities.")?,
        })
    }

    pub fn insert_tracer_into_entity(&mut self, entity: &mut EntityCommands) -> Result<()> {
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
                    None => 0,
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

        entity.insert(Storable { id: tracer_id });

        Ok(())
    }

    /// Gets the Entity ID for a traceable object.
    /// Returning None doesn't mean the entity doesn't exist anymore. It's just not loaded.
    pub fn get_entity(&self, trace_id: Storable) -> Option<Entity> {
        self.tracers_to_entities.get(&trace_id.id).copied()
    }
}

fn adopt_children(
    mut commands: Commands,
    untracked_children: Query<(Entity, With<Parent>, Without<Storable>)>,
    mut entity_tracers: Query<(&Children, &mut SpatialEntityStorage)>,
) -> Result<()> {
    for (children, mut tracer) in entity_tracers.iter_mut() {
        for child in children.iter() {
            if let Ok((child_entity_id, _with_parent, _without_traceable)) =
                untracked_children.get(*child)
            {
                // It appears to be one of our children.
                tracer.insert_tracer_into_entity(&mut commands.entity(child_entity_id))?;
            }
        }
    }

    Ok(())
}

fn new_tracings(
    new_tracings: Query<(Entity, &Parent, &Storable), Changed<Storable>>,
    mut storage: Query<&mut SpatialEntityStorage>,
) {
    for (entity_id, world, traceable) in new_tracings.iter() {
        let mut entity_tracker = storage
            .get_mut(world.get())
            .expect("Failed to get world for entity.");

        let tracer_id = traceable.id;

        entity_tracker
            .tracers_to_entities
            .insert(tracer_id, entity_id);
        entity_tracker
            .entities_to_tracers
            .insert(entity_id, tracer_id);
    }
}

fn clean_up_tracings(
    children: Query<&Parent>,
    mut removed: RemovedComponents<Storable>,
    mut storage: Query<&mut SpatialEntityStorage>,
) {
    for entity_id in removed.iter() {
        let world = children
            .get(entity_id)
            .expect("Failed to get world for entity.");

        let mut entity_tracker = storage
            .get_mut(world.get())
            .expect("Failed to get world for entity.");

        if let Some(tracker_id) = entity_tracker.entities_to_tracers.remove(&entity_id) {
            entity_tracker.tracers_to_entities.remove(&tracker_id);
        } else {
            log::warn!("Removed traceable without an associated entity.");
        }
    }
}

fn load_chunk(
    mut commands: Commands,
    to_load: Query<(Entity, &ChunkPosition, With<ToLoadSpatial>)>,
    mut _storage: Query<&mut SpatialEntityStorage>,
) {
    for (entity, _position, _with_to_load_spatial) in to_load.iter() {
        // TODO implement loading.
        commands
            .entity(entity)
            .remove::<ToLoadSpatial>()
            .insert(GenerateSpatial);
    }
}

fn save_chunk(
    mut commands: Commands,
    to_save: Query<(Entity, &ChunkPosition, With<ToSaveSpatial>)>,
    mut _storage: Query<&mut SpatialEntityStorage>,
) {
    for (entity, _position, _with_to_load_spatial) in to_save.iter() {
        // TODO implement saving.
        commands.entity(entity).remove::<ToSaveSpatial>();
    }
}

pub(super) fn register_storage(app: &mut App) {
    app.add_system(new_tracings);
    app.add_system(clean_up_tracings);
    app.add_system(adopt_children.pipe(crate::error_handler));
    app.add_system(load_chunk);
    app.add_system(save_chunk);
}
