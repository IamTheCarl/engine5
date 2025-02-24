use std::{
    net::{IpAddr, SocketAddr},
    path::Path,
};

use bevy::prelude::*;

use anyhow::{Context, Result};
use bevy_console::{reply, AddConsoleCommand, ConsoleCommand};
use clap::{Parser, Subcommand};
use vec_rand::RandVec;

use crate::{
    config::{file_paths, player_info::PlayerInfo},
    multiplayer::{ClientContext, HostContext},
    world::{client_world, SpawnLocalPlayerRequest},
    GameState,
};

use super::{terrain::BlockRegistry, WorldState};

/// Load and unload the world.
#[derive(Parser, ConsoleCommand)]
#[command(name = "world")]
struct WorldCommand {
    #[command(subcommand)]
    command: WorldSubcommand,
}

#[derive(Subcommand)]
enum WorldSubcommand {
    /// Open a world on your local system.
    Load {
        /// The name of the world.
        name: String,
    },

    /// Unload the current world.
    Unload,

    Create {
        /// The name to give the world.
        name: String,
    },

    /// Delete a world.
    Delete {
        /// The name of the world to delete.
        name: String,
    },

    /// List worlds on your local system.
    List,

    /// Connect to an already running multi-player game.
    Connect {
        /// The host of the game. Can be a raw IP address or hostname.
        ip_address: String,

        /// The port number of the host.
        #[arg(default_value = "5000", short, long)]
        port: u16,

        /// An integer that the server will use to identify your client on the fly.
        /// Defaults to your default network interface's MAC address or a randomly
        /// generated number if that fails.
        #[arg(short, long)]
        client_id: Option<u64>,

        /// The name to give your player. Defaults to your OS username.
        your_name: Option<String>,
    },

    /// Host the current world as a multi-player game.
    /// The world must already be loaded.
    Host {
        /// The port number to host on. Set to zero to select one at random.
        #[arg(default_value = "5000", short, long)]
        port: u16,

        #[arg(default_value = "127.0.0.1", short, long)]
        ip_address: IpAddr,

        /// The maximum number of clients we permit to join the session.
        #[arg(default_value = "20", short, long)]
        max_players: usize,
    },

    /// Close connection to remote server or end session you are currently hosting.
    Close,
}

fn list_worlds(log: &mut ConsoleCommand<WorldCommand>) {
    match std::fs::read_dir(file_paths::SAVE_DIRECTORY) {
        Ok(directory) => {
            for entry in directory.flatten() {
                let path = entry.path();
                if let Some(name) = path.file_name().and_then(|name| name.to_str()) {
                    reply!(log, "\t{}", name);
                }
            }
        }
        Err(error) => reply!(log, "Failed to read save directory: {:?}", error),
    }
}

fn world_command(
    mut log: ConsoleCommand<WorldCommand>,
    commands: Commands,

    game_state: Res<State<GameState>>,
    next_app_state: ResMut<NextState<GameState>>,
    next_world_state: ResMut<NextState<WorldState>>,

    block_registry: Res<BlockRegistry>,
    player_info: Res<PlayerInfo>,
) {
    if let Some(Ok(WorldCommand { command })) = log.take() {
        match game_state.get() {
            GameState::MainMenu | GameState::FatalError => world_command_in_menu(
                command,
                log,
                commands,
                next_app_state,
                next_world_state,
                block_registry,
                player_info,
            ),
            GameState::InGame => {
                world_command_in_game(command, log, commands, next_app_state, next_world_state)
            }
            GameState::ShuttingDown => {
                reply!(log, "Cannot manipulate worlds while shutting down.");
            }
        }
    }
}

fn world_command_in_menu(
    command: WorldSubcommand,
    mut log: ConsoleCommand<WorldCommand>,

    mut commands: Commands,

    mut next_app_state: ResMut<NextState<GameState>>,
    mut next_world_state: ResMut<NextState<WorldState>>,

    block_registry: Res<BlockRegistry>,
    player_info: Res<PlayerInfo>,
) {
    match command {
        WorldSubcommand::Load { name } => {
            if let Err(error) = super::open_world(
                &mut commands,
                Some(&player_info),
                &block_registry,
                Path::new(file_paths::SAVE_DIRECTORY).join(name),
            ) {
                reply!(log, "Failed to load world: {:?}", error);
            } else {
                reply!(log, "World successfully loaded.");
                next_app_state.set(GameState::InGame);
                next_world_state.set(WorldState::Running);
            }
        }
        WorldSubcommand::Unload => {
            reply!(log, "World is already unloaded.");
        }
        WorldSubcommand::Create { name } => {
            if let Err(error) = super::create_world(
                &mut commands,
                Some(&player_info),
                &block_registry,
                Path::new(file_paths::SAVE_DIRECTORY).join(name),
            ) {
                reply!(log, "Failed to create world: {:?}", error);
            } else {
                reply!(log, "World successfully created.");
                next_app_state.set(GameState::InGame);
                next_world_state.set(WorldState::Running);
            }
        }
        WorldSubcommand::Delete { name } => {
            let path = Path::new(file_paths::SAVE_DIRECTORY).join(name);

            if path.exists() {
                if let Err(error) = std::fs::remove_dir_all(path) {
                    reply!(log, "Failed to delete world: {:?}", error);
                }
            } else {
                reply!(log, "World does not exist.");
            }
        }
        WorldSubcommand::List => list_worlds(&mut log),
        WorldSubcommand::Connect {
            ip_address,
            port,
            client_id,
            your_name,
        } => {
            let client_id = client_id.unwrap_or_else(|| {
                // The MAC address is guarenteed to be unique, so prefer that.
                // If for some weird reason we can't get one, generate a random number and hope for the best.
                let client_id_vec = mac_address::get_mac_address()
                    .ok()
                    .flatten()
                    .map(|mac| {
                        let mut bytes: Vec<u8> = mac.bytes().into();
                        bytes.extend([0, 0]);
                        bytes
                    })
                    .unwrap_or(RandVec::generate(8));

                let mut client_id = [0; 8];
                client_id.copy_from_slice(&client_id_vec);

                u64::from_le_bytes(client_id)
            });

            let player_name = your_name.unwrap_or_else(|| player_info.name.clone());

            if let Err(error) = world_connect(
                &mut commands,
                ip_address,
                port,
                client_id,
                player_name,
                &mut next_app_state,
                &mut next_world_state,
            ) {
                reply!(log, "Failed to connect to host: {:?}", error);
            }
        }
        WorldSubcommand::Host {
            port: _,
            ip_address: _,
            max_players: _,
        } => {
            reply!(log, "You must load a world before you can host it.");
        }
        WorldSubcommand::Close => {
            reply!(log, "You are not in a multi-player session.");
        }
    }
}

fn world_connect(
    commands: &mut Commands,
    ip_address: String,
    port: u16,
    client_id: u64,
    client_name: String,
    next_app_state: &mut ResMut<NextState<GameState>>,
    next_world_state: &mut ResMut<NextState<WorldState>>,
) -> Result<()> {
    use std::cmp::Ordering;

    let mut addresses = dns_lookup::lookup_host(&ip_address).context("DNS lookup failed.")?;

    // We do this sort to make IPv4 take higher priority.
    // It'll result in players connecting faster most of the time.
    // TODO: Should this be an option in the settings?
    addresses.sort_by(|a, b| {
        if a.is_ipv4() && b.is_ipv6() {
            Ordering::Less
        } else if a.is_ipv6() && b.is_ipv4() {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    });

    let addresses = addresses
        .into_iter()
        .map(move |ip_addr| SocketAddr::new(ip_addr, port));
    ClientContext::start_session(
        commands,
        Box::new(addresses),
        client_id,
        client_name.clone(),
    )?;
    client_world(commands);
    commands.insert_resource(SpawnLocalPlayerRequest { name: client_name });

    next_app_state.set(GameState::InGame);
    next_world_state.set(WorldState::Running);

    Ok(())
}

fn world_command_in_game(
    command: WorldSubcommand,
    mut log: ConsoleCommand<WorldCommand>,

    mut commands: Commands,

    mut next_app_state: ResMut<NextState<GameState>>,
    mut next_world_state: ResMut<NextState<WorldState>>,
) {
    match command {
        WorldSubcommand::Load { name: _ } => {
            reply!(
                log,
                "You must unload the world before you can load another one."
            );
        }
        WorldSubcommand::Unload => {
            next_app_state.set(GameState::MainMenu);
            next_world_state.set(WorldState::Unloaded);
        }
        WorldSubcommand::Create { name: _ } => {
            reply!(
                log,
                "You must unload the world before you can create a new one."
            );
        }
        WorldSubcommand::Delete { name: _ } => {
            reply!(
                log,
                "You must unload the world before you can delete a world."
            )
        }
        WorldSubcommand::List => list_worlds(&mut log),
        WorldSubcommand::Connect {
            ip_address: _,
            port: _,
            client_id: _,
            your_name: _,
        } => {
            reply!(
                log,
                "You must unload the current world before you can connect to another one."
            )
        }
        WorldSubcommand::Host {
            port,
            ip_address,
            max_players,
        } => {
            let socket_address = SocketAddr::new(ip_address, port);
            if let Err(error) =
                HostContext::start_session(&mut commands, socket_address, max_players)
            {
                reply!(log, "Failed to host world: {:?}", error);
            }
        }
        WorldSubcommand::Close => {
            HostContext::end_session(&mut commands);
            ClientContext::end_session(&mut commands);
            reply!(log, "Session ended.");
        }
    }
}

pub fn setup(app: &mut App) {
    app.add_console_command::<WorldCommand, _>(world_command);
}
