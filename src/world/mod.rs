use bevy::{ecs::system::EntityCommands, prelude::*};

mod commands;
pub mod physics;
pub mod spatial_entities;
pub mod terrain;

use anyhow::{bail, Context, Result};
use std::path::PathBuf;

use terrain::{BlockRegistry, BlockTag};
pub mod generation;

use crate::{
    config::player_info::PlayerInfo, error_handler, multiplayer::RemoteClientPlayer, GameState,
};

use self::{
    generation::OscillatingHills,
    physics::Position,
    spatial_entities::{
        storage::EntityStorage,
        types::{
            global_terrain::GlobalTerrainEntity,
            player::{spawner::PlayerSpawner, LocalPlayer, PlayerEntity},
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
    block_registry: &BlockRegistry,
    path: impl Into<PathBuf>,
) -> Result<()> {
    let path = path.into();
    if path.exists() {
        raw_open_file_backed_world(commands, block_registry, path)
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
        raw_open_file_backed_world(commands, block_registry, path)
    } else {
        bail!("World already exists.");
    }
}

pub fn raw_open_file_backed_world(
    commands: &mut Commands,
    block_registry: &BlockRegistry,
    path: impl Into<PathBuf>,
) -> Result<()> {
    let path = path.into();
    let is_new_world = !path.exists();

    let world_database =
        sled::open(path).context("Failed to open or create database for world.")?;
    let storage = EntityStorage::new(&world_database)?;

    // TODO we shouldn't make this request if we're running a headless server.
    // Request that the local player be spawned as soon as possible.
    commands.insert_resource(SpawnLocalPlayerRequest);

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

pub fn client_world<'w, 's, 'a>(commands: &'a mut Commands<'w, 's>) -> EntityCommands<'w, 's, 'a> {
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
struct SpawnLocalPlayerRequest;

#[allow(clippy::complexity)]
fn spawn_local_player(
    mut commands: Commands,
    player_info: Res<PlayerInfo>,
    world_entity: Res<WorldEntity>,
    storage: ResMut<EntityStorage>,
    offline_players: Query<(Entity, &PlayerEntity), Without<RemoteClientPlayer>>,
    player_spawn: Query<&Position, With<PlayerSpawner>>,
) -> Result<()> {
    // Check if the player already exists in the world.
    for (player_entity, offline_player) in offline_players.iter() {
        if offline_player.name == player_info.name {
            // This is our player! Activate it!
            commands.entity(player_entity).insert(LocalPlayer);

            return Ok(());
        }
    }

    // Looks like they don't.
    // Spawn them in.
    if let Ok(spawn_position) = player_spawn.get_single() {
        PlayerEntity::spawn_local(
            world_entity.entity,
            &mut commands,
            &storage,
            spawn_position.clone(),
            player_info.name.clone(),
            0.0,
        )?;

        commands.remove_resource::<SpawnLocalPlayerRequest>();
    }

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
            physics::PhysicsPlugin,
            spatial_entities::SpatialEntityPlugin,
        ));

        app.add_state::<WorldState>();
        app.add_systems(OnExit(GameState::InGame), unload_world);
        app.add_systems(
            Update,
            spawn_local_player
                .pipe(error_handler)
                .run_if(resource_exists::<WorldEntity>())
                .run_if(resource_exists::<SpawnLocalPlayerRequest>())
                .run_if(in_state(WorldState::Running)),
        );

        generation::setup_terrain_generation(app);
        commands::setup(app);
    }
}
