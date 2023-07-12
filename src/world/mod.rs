use bevy::prelude::*;

pub mod physics;
pub mod player;
pub mod spatial_entities;
pub mod terrain;

use anyhow::{Context, Result};
use std::path::PathBuf;

use physics::{Position, Velocity};
use terrain::{BlockRegistry, BlockTag, TerrainSpace, TerrainSpaceBundle, TerrainStorage};

pub mod generation;

use self::spatial_entities::storage::SpatialEntityStorage;

pub fn spawn_world(
    commands: &mut Commands,
    block_registry: &BlockRegistry,
    path: impl Into<PathBuf>,
) -> Result<()> {
    let default_tag = BlockTag::try_from("core:default").unwrap();
    let default_data = block_registry
        .get_by_tag(&default_tag)
        .context("Could not find default block in block registry.")?;
    let default_block = default_data.spawn();

    let world_database =
        sled::open(path.into()).context("Failed to open or create database for world.")?;

    let overworld_storage = TerrainStorage::open_local(&world_database, "overworld")
        .context("Failed to open namespace for overworld terrain.")?;

    let storage = SpatialEntityStorage::new(&world_database)?;

    commands.spawn((
        TerrainSpaceBundle {
            terrain_space: TerrainSpace::global(),
            position: Position {
                translation: Vec3::ZERO,
                rotation: 0.0,
            },
            file: overworld_storage,
            // storable: storage.new_bootstrapped_storable_component(
            //     BootstrapEntityInfo::GlobalTerrain,
            //     "global_terrain",
            // )?,
            transform: Transform::default(),
            global_transform: GlobalTransform::default(),
            visibility: Visibility::Inherited,
            computed_visibility: ComputedVisibility::default(),
        },
        generation::OscillatingHills {
            block: default_block,
            rate: 512,
            depth: 16,
            database: world_database,
        },
        // generation::FlatWorld {
        //     block: default_block,
        // },
        // generation::CheckerBoard {
        //     even_block: default_block,
        //     odd_block: default_block,
        //     even_height: 1,
        //     odd_height: 3,
        // },
        Velocity {
            translation: Vec3::ZERO,
            rotational: 0.0,
        },
    ));

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

        generation::register_terrain_generators(app);
    }
}
