//! Features relating to entities that exist in 3D space. They can be fixed to the terrain they sit on or move freely outside of
//! of terrain space.

use std::collections::{HashMap, HashSet};

use bevy::prelude::*;
use bevy_prototype_debug_lines::DebugLines;

use crate::DebugRenderSettings;

use super::{
    physics::Position,
    terrain::{Chunk, ChunkIndex, ChunkPosition},
};

pub mod storage;

// TODO experiment with storing entities by archtype.
#[derive(Resource)]
pub struct SpatialEntityTracker {
    cell_entity_sets: HashMap<SpatialHash, HashSet<Entity>>,
}

impl SpatialEntityTracker {
    pub fn iter_cells(&self) -> impl Iterator<Item = (&SpatialHash, &HashSet<Entity>)> {
        self.cell_entity_sets.iter()
    }

    fn add_entity(&mut self, spatial_hash: SpatialHash, entity: Entity) {
        self.cell_entity_sets
            .entry(spatial_hash)
            .or_insert_with(HashSet::new)
            .insert(entity);
    }

    fn remove_entity(&mut self, spatial_hash: SpatialHash, entity: Entity) {
        if let Some(cell) = self.cell_entity_sets.get_mut(&spatial_hash) {
            cell.remove(&entity);

            // If the cell is empty, remove it. Save some memory.
            if cell.is_empty() {
                self.cell_entity_sets.remove(&spatial_hash);
            }
        }
    }

    pub fn get_cell_entities(&self, spatial_hash: &SpatialHash) -> Option<&HashSet<Entity>> {
        self.cell_entity_sets.get(spatial_hash)
    }

    #[rustfmt::skip]
    fn calculate_ballpark(spatial_hash: &SpatialHash) -> [SpatialHash; 27] {
        let x = spatial_hash.x;
        let y = spatial_hash.y;
        let z = spatial_hash.z;

        [
            SpatialHash { x,        y,        z        },
            SpatialHash { x,        y,        z: z + 1 },
            SpatialHash { x,        y,        z: z - 1 },
            SpatialHash { x,        y: y + 1, z        },
            SpatialHash { x,        y: y + 1, z: z + 1 },
            SpatialHash { x,        y: y + 1, z: z - 1 },
            SpatialHash { x,        y: y - 1, z        },
            SpatialHash { x,        y: y - 1, z: z + 1 },
            SpatialHash { x,        y: y - 1, z: z - 1 },
            SpatialHash { x: x + 1, y,        z        },
            SpatialHash { x: x + 1, y,        z: z + 1 },
            SpatialHash { x: x + 1, y,        z: z - 1 },
            SpatialHash { x: x + 1, y: y + 1, z        },
            SpatialHash { x: x + 1, y: y + 1, z: z + 1 },
            SpatialHash { x: x + 1, y: y + 1, z: z - 1 },
            SpatialHash { x: x + 1, y: y - 1, z        },
            SpatialHash { x: x + 1, y: y - 1, z: z + 1 },
            SpatialHash { x: x + 1, y: y - 1, z: z - 1 },
            SpatialHash { x: x - 1, y,        z        },
            SpatialHash { x: x - 1, y,        z: z + 1 },
            SpatialHash { x: x - 1, y,        z: z - 1 },
            SpatialHash { x: x - 1, y: y + 1, z        },
            SpatialHash { x: x - 1, y: y + 1, z: z + 1 },
            SpatialHash { x: x - 1, y: y + 1, z: z - 1 },
            SpatialHash { x: x - 1, y: y - 1, z        },
            SpatialHash { x: x - 1, y: y - 1, z: z + 1 },
            SpatialHash { x: x - 1, y: y - 1, z: z - 1 },
        ]
    }

    pub fn get_ballpark(&self, spatial_hash: &SpatialHash, mut processor: impl FnMut(&Entity)) {
        for spatial_hash in Self::calculate_ballpark(spatial_hash) {
            if let Some(cell) = self.get_cell_entities(&spatial_hash) {
                for entity in cell.iter() {
                    processor(entity);
                }
            }
        }
    }
}

#[derive(Component, Debug, PartialEq, Eq, Hash, Default, Clone, Copy)]
pub struct SpatialHash {
    x: i16,
    y: i16,
    z: i16,
}

impl From<ChunkIndex> for SpatialHash {
    fn from(value: ChunkIndex) -> Self {
        Self {
            x: value.x as i16,
            y: value.y as i16,
            z: value.z as i16,
        }
    }
}

impl From<SpatialHash> for ChunkIndex {
    fn from(val: SpatialHash) -> Self {
        ChunkIndex::new(val.x as i32, val.y as i32, val.z as i32)
    }
}

impl From<SpatialHash> for ChunkPosition {
    fn from(val: SpatialHash) -> Self {
        ChunkPosition { index: val.into() }
    }
}

impl From<&ChunkPosition> for SpatialHash {
    fn from(val: &ChunkPosition) -> Self {
        Self {
            x: val.index.x as i16,
            y: val.index.y as i16,
            z: val.index.z as i16,
        }
    }
}

#[derive(Component, Debug)]
pub struct SpatialHashOffset {
    pub translation: Vec3,
}

fn insert_spatial_hash(
    mut commands: Commands,
    entities: Query<(Entity, With<Position>, Without<SpatialHash>)>,
) {
    for (entity, _, _) in entities.iter() {
        commands.entity(entity).insert(SpatialHash::default());
    }
}

fn handle_removed_spatial_hash_entities(
    mut removals: RemovedComponents<SpatialHash>,
    spatial_objects: Query<&SpatialHash>,
    mut spatial_object_tracker: ResMut<SpatialEntityTracker>,
) {
    for entity in removals.iter() {
        if let Ok(spatial_hash) = spatial_objects.get(entity) {
            spatial_object_tracker.remove_entity(*spatial_hash, entity);
        }
    }
}

fn update_spatial_hash_entities(
    mut spatial_objects: Query<(Entity, &Position, &mut SpatialHash), Without<SpatialHashOffset>>,
    mut spatial_object_tracker: ResMut<SpatialEntityTracker>,
    debug_render_settings: Res<DebugRenderSettings>,
    mut lines: ResMut<DebugLines>,
) {
    // TODO only update the hash for entities that have a velocity?

    for (entity, position, mut spatial_hash) in spatial_objects.iter_mut() {
        let old_hash = *spatial_hash;

        let translation = (position.translation / Chunk::CHUNK_DIAMETER as f32).floor();

        spatial_hash.x = translation.x as i16;
        spatial_hash.y = translation.y as i16;
        spatial_hash.z = translation.z as i16;

        if debug_render_settings.hashing_center_point {
            lines.line_colored(
                position.translation,
                position.translation + Vec3::Y,
                0.0,
                Color::GREEN,
            );

            lines.line_colored(
                position.translation,
                translation * Chunk::CHUNK_DIAMETER as f32,
                0.0,
                Color::BLUE,
            );
        }

        // An update is actually needed.
        if old_hash != *spatial_hash
            || spatial_object_tracker
                .get_cell_entities(&spatial_hash)
                .map_or(true, |entity_set| !entity_set.contains(&entity))
        {
            spatial_object_tracker.remove_entity(old_hash, entity);
            spatial_object_tracker.add_entity(*spatial_hash, entity);
        }
    }
}

fn update_spatial_hash_entities_with_offset(
    mut spatial_objects: Query<(Entity, &Position, &SpatialHashOffset, &mut SpatialHash)>,
    mut spatial_object_tracker: ResMut<SpatialEntityTracker>,
    debug_render_settings: Res<DebugRenderSettings>,
    mut lines: ResMut<DebugLines>,
) {
    // TODO only update the hash for entities that have a velocity?

    for (entity, position, spatial_offset, mut spatial_hash) in spatial_objects.iter_mut() {
        let old_hash = *spatial_hash;

        let offset_translation =
            position.translation + position.inverse_quat() * spatial_offset.translation;

        let translation = (offset_translation / Chunk::CHUNK_DIAMETER as f32).floor();

        spatial_hash.x = translation.x as i16;
        spatial_hash.y = translation.y as i16;
        spatial_hash.z = translation.z as i16;

        if debug_render_settings.hashing_center_point {
            lines.line_colored(
                offset_translation,
                offset_translation + Vec3::Y,
                0.0,
                Color::GREEN,
            );

            lines.line_colored(
                offset_translation,
                translation * Chunk::CHUNK_DIAMETER as f32,
                0.0,
                Color::BLUE,
            );
        }

        // An update is actually needed.
        if old_hash != *spatial_hash
            || spatial_object_tracker
                .get_cell_entities(&spatial_hash)
                .map_or(true, |entity_set| !entity_set.contains(&entity))
        {
            spatial_object_tracker.remove_entity(old_hash, entity);
            spatial_object_tracker.add_entity(*spatial_hash, entity);
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct SpatialEntityPlugin;

impl Plugin for SpatialEntityPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, |mut commands: Commands| {
            // TODO make this accessible from a menu or terminal.
            commands.insert_resource(DebugRenderSettings {
                cylinders: true,
                cylinder_terrain_checks: false,
                hashing_center_point: false,
                cylinder_cylinder_checks: false,
                terrain_terrain_checks: false,
                terrain_ray_casts: false,
            });

            commands.insert_resource(SpatialEntityTracker {
                cell_entity_sets: HashMap::new(),
            });
        });

        app.add_systems(
            Update,
            (
                insert_spatial_hash.in_set(SpatialEntityPlugin),
                update_spatial_hash_entities.in_set(SpatialEntityPlugin),
                update_spatial_hash_entities_with_offset.in_set(SpatialEntityPlugin),
            ),
        );
        app.add_systems(
            Update,
            handle_removed_spatial_hash_entities
                .in_set(SpatialEntityPlugin)
                .after(update_spatial_hash_entities)
                .after(update_spatial_hash_entities_with_offset),
        );

        storage::register_storage(app);
    }
}
