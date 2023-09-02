use std::path::Path;

use bevy::prelude::*;

use bevy_console::{reply, AddConsoleCommand, ConsoleCommand};
use clap::{Parser, Subcommand};

use crate::{config::file_paths, GameState};

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
        host: String,

        /// The port number of the host.
        #[arg(default_value = "5000")]
        port: u16,
    },

    /// Host the current world as a multi-player game.
    /// The world must already be loaded.
    Host {
        /// The port number to host on.
        #[arg(default_value = "5000")]
        port: u16,
    },

    /// Close connection to remote server or end session you are currently hosting.
    Close,
}

fn list_worlds(log: &mut ConsoleCommand<WorldCommand>) {
    match std::fs::read_dir(file_paths::SAVE_DIRECTORY) {
        Ok(directory) => {
            for entry in directory {
                if let Ok(entry) = entry {
                    let path = entry.path();
                    if let Some(name) = path.file_name().and_then(|name| name.to_str()) {
                        reply!(log, "\t{}", name);
                    }
                }
            }
        }
        Err(error) => reply!(log, "Failed to read save directory: {:?}", error),
    }
}

fn world_command(
    mut log: ConsoleCommand<WorldCommand>,
    mut commands: Commands,

    game_state: Res<State<GameState>>,
    mut next_app_state: ResMut<NextState<GameState>>,
    mut next_world_state: ResMut<NextState<WorldState>>,

    block_registry: Res<BlockRegistry>,
) {
    if let Some(Ok(WorldCommand { command })) = log.take() {
        match game_state.get() {
            GameState::MainMenu | GameState::FatalError => match command {
                WorldSubcommand::Load { name } => {
                    if let Err(error) = super::open_world(
                        &mut commands,
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
                WorldSubcommand::Connect { host, port } => reply!(log, "Unimplemented."),
                WorldSubcommand::Host { port: _ } => {
                    reply!(log, "You must load a world before you can host it.");
                }
                WorldSubcommand::Close => reply!(log, "No world is being hosted."),
            },
            GameState::InGame => {
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
                    WorldSubcommand::Connect { host: _, port: _ } => {
                        reply!(log, "You muat unload the current world before you can connect to another one.")
                    }
                    WorldSubcommand::Host { port } => reply!(log, "Unimplemented."),
                    WorldSubcommand::Close => reply!(log, "Unimplemented."),
                }
            }
            GameState::ShuttingDown => {
                reply!(log, "Cannot manipulate worlds while shutting down.");
            }
        }
    }
}

pub fn setup(app: &mut App) {
    app.add_console_command::<WorldCommand, _>(world_command);
}
