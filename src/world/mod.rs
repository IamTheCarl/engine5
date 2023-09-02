use bevy::prelude::*;

mod commands;
pub mod physics;
pub mod player;
pub mod spatial_entities;
pub mod terrain;

use anyhow::{bail, Context, Result};
use std::path::PathBuf;

use terrain::{BlockRegistry, BlockTag};

mod dynamic_terrain;
pub mod generation;
mod global_terrain;

use crate::GameState;

use self::{
    generation::OscillatingHills, global_terrain::GlobalTerrainEntity,
    spatial_entities::storage::EntityStorage,
};

#[derive(Component)]
struct World;

#[derive(Resource)]
struct WorldEntity {
    pub entity: Entity,
}

pub fn open_world(
    commands: &mut Commands,
    block_registry: &BlockRegistry,
    path: impl Into<PathBuf>,
) -> Result<()> {
    let path = path.into();
    if path.exists() {
        raw_open_world(commands, block_registry, path)
    } else {
        bail!("World does not exist.");
    }
}

pub fn create_world(
    commands: &mut Commands,
    block_registry: &BlockRegistry,
    path: impl Into<PathBuf>,
) -> Result<()> {
    let path = path.into();
    if !path.exists() {
        raw_open_world(commands, block_registry, path)
    } else {
        bail!("World already exists.");
    }
}

pub fn raw_open_world(
    commands: &mut Commands,
    block_registry: &BlockRegistry,
    path: impl Into<PathBuf>,
) -> Result<()> {
    let path = path.into();
    let is_new_world = !path.exists();

    let world_database =
        sled::open(path).context("Failed to open or create database for world.")?;
    let storage = EntityStorage::new(&world_database)?;

    let entity = commands
        .spawn((
            World,
            TransformBundle::default(),
            VisibilityBundle::default(),
        ))
        .id();
    commands.insert_resource(WorldEntity { entity });

    if is_new_world {
        // New world. Nothing to bootstrap, insert our initial state.
        let default_tag = BlockTag::try_from("core:default").unwrap();
        let default_data = block_registry
            .get_by_tag(&default_tag)
            .context("Could not find default block in block registry.")?;

        let default_block = default_data.spawn();

        GlobalTerrainEntity::create_new(
            entity,
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

#[derive(Debug, Clone, Copy, Default, Eq, PartialEq, Hash, States)]
pub enum WorldState {
    #[default]
    Unloaded,
    Running,
    Paused,
}

fn unload_world(mut commands: Commands, world_entities: Query<Entity, With<World>>) {
    for entity in world_entities.iter() {
        if let Some(entity) = commands.get_entity(entity) {
            entity.despawn_recursive();
        }
    }

    commands.remove_resource::<EntityStorage>();
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct WorldPlugin;

impl Plugin for WorldPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((
            terrain::TerrainPlugin,
            physics::PhysicsPlugin,
            player::PlayerPlugin,
            spatial_entities::SpatialEntityPlugin,
        ));

        app.add_state::<WorldState>();
        app.add_systems(OnExit(GameState::InGame), unload_world);

        generation::setup_terrain_generation(app);
        global_terrain::setup(app);
        dynamic_terrain::setup(app);
        commands::setup(app);
    }
}
