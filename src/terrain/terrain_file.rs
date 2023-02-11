use bevy::prelude::*;

use super::{terrain_generation::ToGenerate, Chunk, ChunkPosition};

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToLoad;

#[derive(Component, Default)]
pub struct TerrainFile {}

impl TerrainFile {
    pub fn new() -> Self {
        Self {}
    }

    pub fn iter_chunk_indexes(&self) -> impl Iterator<Item = ChunkPosition> {
        // TODO implement.
        std::iter::empty()
    }
}

/// Load terrain from a file.
fn load_terrain(
    mut commands: Commands,
    mut to_load: Query<(Entity, &ChunkPosition, With<ToLoad>, Without<Chunk>)>,
) {
    for (entity, _position, _to_load, _without_chunk) in to_load.iter_mut() {
        // This chunk no longer needs to be loaded, so remove the marker.
        let mut entity = commands.entity(entity);
        entity.remove::<ToLoad>();

        // TODO complete.

        // If we fail to load a chunk (probably because it doesn't exist in the file) then we mark it as needing to be generated.
        entity.insert(ToGenerate);
    }
}

pub fn register_terrain_files(app: &mut App) {
    app.add_system(load_terrain);
}
