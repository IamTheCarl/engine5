use avian3d::prelude::*;
use bevy::{ecs::system::EntityCommands, prelude::*};

mod commands;
pub mod spatial_entities;
pub mod terrain;

use anyhow::{bail, Context, Result};
use std::path::PathBuf;

use terrain::{BlockRegistry, BlockTag};
pub mod generation;

use crate::{config::player_info::PlayerInfo, error_handler, GameState};

use self::{
    generation::OscillatingHills,
    spatial_entities::{
        storage::EntityStorage,
        types::{
            global_terrain::GlobalTerrainEntity,
            player::{spawner::PlayerSpawner, ActivePlayer, LocalPlayer, PlayerEntity},
        },
    },
};

#[derive(Component)]
struct World;

#[derive(Resource)]
pub struct WorldEntity {
    pub entity: Entity,
}

pub fn open_world(
    commands: &mut Commands,
    local_player: Option<&PlayerInfo>,
    block_registry: &BlockRegistry,
    path: impl Into<PathBuf>,
) -> Result<()> {
    let path = path.into();
    if path.exists() {
        raw_open_file_backed_world(commands, local_player, block_registry, path)
    } else {
        bail!("World does not exist.");
    }
}

pub fn create_world(
    commands: &mut Commands,
    local_player: Option<&PlayerInfo>,
    block_registry: &BlockRegistry,
    path: impl Into<PathBuf>,
) -> Result<()> {
    let path = path.into();
    if !path.exists() {
        raw_open_file_backed_world(commands, local_player, block_registry, path)
    } else {
        bail!("World already exists.");
    }
}

pub fn raw_open_file_backed_world(
    commands: &mut Commands,
    local_player: Option<&PlayerInfo>,
    block_registry: &BlockRegistry,
    path: impl Into<PathBuf>,
) -> Result<()> {
    let path = path.into();
    let is_new_world = !path.exists();

    let world_database =
        sled::open(path).context("Failed to open or create database for world.")?;
    let storage = EntityStorage::new(&world_database)?;

    // Request that the local player be spawned as soon as possible.
    if let Some(local_player) = local_player {
        commands.insert_resource(SpawnLocalPlayerRequest {
            name: local_player.name.clone(),
        });
    }

    let entity = client_world(commands).id();

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

pub fn client_world<'a>(commands: &'a mut Commands) -> EntityCommands<'a> {
    let world_commands = commands.spawn((
        World,
        TransformBundle::default(),
        VisibilityBundle::default(),
    ));

    let entity = world_commands.id();
    commands.insert_resource(WorldEntity { entity });

    commands.entity(entity)
}

/// This resource is used as a marker, telling the ECS that we need to spawn the local player.
#[derive(Resource)]
struct SpawnLocalPlayerRequest {
    name: String,
}

#[allow(clippy::complexity)]
fn spawn_local_player(
    mut commands: Commands,
    local_player_request: Res<SpawnLocalPlayerRequest>,
    world_entity: Res<WorldEntity>,
    storage: Option<ResMut<EntityStorage>>,
    offline_players: Query<(Entity, &PlayerEntity), Without<ActivePlayer>>,
    player_spawn: Query<&Transform, With<PlayerSpawner>>,
) -> Result<()> {
    // Check if the player already exists in the world.
    for (player_entity, offline_player) in offline_players.iter() {
        if offline_player.name == local_player_request.name {
            // This is our player! Activate it!
            commands
                .entity(player_entity)
                .insert(LocalPlayer)
                .insert(ActivePlayer);

            commands.remove_resource::<SpawnLocalPlayerRequest>();
            return Ok(());
        }
    }

    // Looks like they don't.
    if let Some(storage) = storage {
        // Spawn them in, if it's a single player world.
        if let Ok(spawn_position) = player_spawn.get_single() {
            PlayerEntity::spawn_deactivated(
                world_entity.entity,
                &mut commands,
                &storage,
                spawn_position.clone(),
                local_player_request.name.clone(),
                0.0,
            )?
            .insert(LocalPlayer)
            .insert(ActivePlayer);

            commands.remove_resource::<SpawnLocalPlayerRequest>();
        }
    }

    // If it's not a single-player world, we'll just wait for the server to send us a player.

    Ok(())
}

#[derive(Debug, Clone, Copy, Default, Eq, PartialEq, Hash, States)]
pub enum WorldState {
    #[default]
    Unloaded,
    Running,
    Paused,
}

#[derive(Component)]
pub struct ViewRadius {
    pub chunks: i32,
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
            PhysicsPlugins::default(),
            PhysicsDebugPlugin::default(),
            spatial_entities::SpatialEntityPlugin,
        ));

        //         app.insert_resource(RapierConfiguration {
        //             gravity: Vec3::new(0.0, -9.8, 0.0),
        //             physics_pipeline_active: true,
        //             query_pipeline_active: true,
        //             timestep_mode: TimestepMode::Interpolated {
        //                 dt: 0.016,
        //                 time_scale: 1.0,
        //                 substeps: 1,
        //             },
        //             scaled_shape_subdivision: 10,
        //             force_update_from_transform_changes: true,
        //         });

        app.init_state::<WorldState>();
        app.add_systems(OnExit(GameState::InGame), unload_world);
        app.add_systems(
            Update,
            spawn_local_player
                .pipe(error_handler)
                .run_if(resource_exists::<WorldEntity>)
                .run_if(resource_exists::<SpawnLocalPlayerRequest>)
                .run_if(in_state(WorldState::Running)),
        );

        generation::setup_terrain_generation(app);
        commands::setup(app);
    }
}
