use bevy::prelude::*;

pub mod physics;
pub mod player;
pub mod spatial_entities;
pub mod terrain;

use anyhow::{Context, Result};
use std::path::PathBuf;

use terrain::{BlockRegistry, BlockTag};

pub mod generation;
mod global_terrain;

use self::{
    generation::OscillatingHills, global_terrain::GlobalTerrainEntity,
    spatial_entities::storage::EntityStorage,
};

pub fn open_world(
    commands: &mut Commands,
    block_registry: &BlockRegistry,
    path: impl Into<PathBuf>,
) -> Result<()> {
    let path = path.into();
    let is_new_world = !path.exists();

    let world_database =
        sled::open(path).context("Failed to open or create database for world.")?;
    let storage = EntityStorage::new(&world_database)?;

    if is_new_world {
        // New world. Nothing to bootstrap, insert our initial state.
        let default_tag = BlockTag::try_from("core:default").unwrap();
        let default_data = block_registry
            .get_by_tag(&default_tag)
            .context("Could not find default block in block registry.")?;

        let default_block = default_data.spawn();

        GlobalTerrainEntity::create_new(
            commands,
            &storage,
            OscillatingHills {
                block: default_block,
                rate: 512,
                depth: 16,
            },
        )
        .context("Failed to create global terrain.")?;
    } else {
        // Old world. We just need to let it bootstrap.
        storage.request_bootstrap();
    }

    commands.insert_resource(storage);

    Ok(())
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct WorldPlugin;

impl Plugin for WorldPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugin(terrain::TerrainPlugin);
        app.add_plugin(physics::PhysicsPlugin);
        app.add_plugin(player::PlayerPlugin);
        app.add_plugin(spatial_entities::SpatialEntityPlugin);

        generation::setup_terrain_generation(app);
        global_terrain::setup(app);
    }
}
