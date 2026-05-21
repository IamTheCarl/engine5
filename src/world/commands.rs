use std::{
    net::{IpAddr, SocketAddr},
    path::Path,
};

use bevy::prelude::*;
use bevy_console::AddConsoleCommand;

use anyhow::{Context, Result};
use bevy_console::ConsoleCommand;
use clap::{Parser, Subcommand};

use crate::{
    config::{file_paths, player_info::PlayerInfo},
    multiplayer::{ClientContext, HostContext},
    world::SpawnLocalPlayerRequest,
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

        /// The port number of the host. Set to zero to select one at random.
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

fn list_worlds() {
    match std::fs::read_dir(file_paths::SAVE_DIRECTORY) {
        Ok(directory) => {
            for entry in directory.flatten() {
                let path = entry.path();
                if let Some(name) = path.file_name().and_then(|name| name.to_str()) {
                    log::info!("\t{}", name);
                }
            }
        }
        Err(error) => log::info!("Failed to read save directory: {:?}", error),
    }
}

fn world_command(
    mut command: ConsoleCommand<WorldCommand>,
    mut next_app_state: ResMut<NextState<GameState>>,
    mut next_world_state: ResMut<NextState<WorldState>>,
    block_registry: Res<BlockRegistry>,
    player_info: Res<PlayerInfo>,
) {
    let _ = (block_registry, player_info);
    if let Some(Ok(cmd)) = command.take() {
        handle_world_subcommand(cmd.command, &mut next_app_state, &mut next_world_state);
    }
}

fn handle_world_subcommand(
    subcommand: WorldSubcommand,
    next_app_state: &mut NextState<GameState>,
    next_world_state: &mut NextState<WorldState>,
) {
    match subcommand {
        WorldSubcommand::Load { name } => {
            log::info!("Loading world: {}", name);
        }
        WorldSubcommand::Unload => {
            log::info!("Unloading world");
        }
        WorldSubcommand::Create { name } => {
            log::info!("Creating world: {}", name);
        }
        WorldSubcommand::Delete { name } => {
            log::info!("Deleting world: {}", name);
        }
        WorldSubcommand::List => {
            list_worlds();
        }
        WorldSubcommand::Connect {
            ip_address,
            port,
            client_id,
            your_name,
        } => {
            let client_id = client_id.unwrap_or_else(|| {
                mac_address::get_mac_address()
                    .ok()
                    .flatten()
                    .map(|mac| {
                        let bytes = mac.bytes();
                        let mut padded = [0u8; 8];
                        padded[0..6].copy_from_slice(&bytes);
                        u64::from_le_bytes(padded)
                    })
                    .unwrap_or_else(|| rand::random())
            });
            let client_name = your_name.unwrap_or_else(|| whoami::username());

            if let Err(e) = world_connect(
                &ip_address,
                port,
                client_id,
                &client_name,
                next_app_state,
                next_world_state,
            ) {
                log::error!("Failed to connect: {:?}", e);
            }
        }
        WorldSubcommand::Host {
            port,
            ip_address,
            max_players,
        } => {
            log::info!(
                "Hosting on {}:{} with max {} players",
                ip_address,
                port,
                max_players
            );
        }
        WorldSubcommand::Close => {
            log::info!("Closing connection");
        }
    }
}

fn world_connect(
    ip_address: &str,
    port: u16,
    client_id: u64,
    client_name: &str,
    next_app_state: &mut NextState<GameState>,
    next_world_state: &mut NextState<WorldState>,
) -> Result<()> {
    use std::cmp::Ordering;

    let mut addresses = dns_lookup::lookup_host(ip_address).context("DNS lookup failed.")?;

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

    next_app_state.set(GameState::InGame);
    next_world_state.set(WorldState::Running);

    Ok(())
}

pub fn setup(app: &mut App) {
    app.add_console_command::<WorldCommand, _>(world_command);
}
