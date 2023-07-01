use anyhow::{Context, Result};
use bevy::prelude::*;
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

#[derive(Resource)]
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

    pub fn new_storable_component(&mut self) -> Result<Storable> {
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

        Ok(Storable { id: tracer_id })
    }

    /// Gets the Entity ID for a traceable object.
    /// Returning None doesn't mean the entity doesn't exist anymore. It's just not loaded.
    pub fn get_entity(&self, trace_id: Storable) -> Option<Entity> {
        self.tracers_to_entities.get(&trace_id.id).copied()
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

fn load_chunk(
    mut commands: Commands,
    to_load: Query<(Entity, &ChunkPosition, With<ToLoadSpatial>)>,
    _storage: Option<ResMut<SpatialEntityStorage>>,
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
    _storage: Option<ResMut<SpatialEntityStorage>>,
) {
    for (entity, _position, _with_to_load_spatial) in to_save.iter() {
        // TODO implement saving.
        commands.entity(entity).remove::<ToSaveSpatial>();
    }
}

pub(super) fn register_storage(app: &mut App) {
    app.add_system(new_tracings);
    app.add_system(clean_up_tracings);
    app.add_system(load_chunk);
    app.add_system(save_chunk);
}
