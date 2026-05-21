use bevy::prelude::*;
use bevy::window::ExitCondition;
use bevy_console::{ConsoleConfiguration, ConsolePlugin};

use crate::ui::ErrorContext;

pub mod config;
pub mod console_scripts;
mod logging;
pub mod multiplayer;
pub mod ui;
pub mod world;

pub use console_scripts::ConsoleScripts;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins
            .set(AssetPlugin {
                watch_for_changes_override: Some(true),
                ..default()
            })
            .set(WindowPlugin {
                primary_window: Some(Window {
                    title: "Engine 5".to_string(),
                    ..Default::default()
                }),
                exit_condition: ExitCondition::DontExit,
                ..default()
            }),))
        .add_plugins((ConsolePlugin, ConsoleScripts, Engine5::new()))
        .insert_resource(ConsoleConfiguration::default())
        .run();
}

#[derive(Debug, Clone, Copy, Default, Eq, PartialEq, Hash, States)]
pub enum GameState {
    #[default]
    MainMenu,
    InGame,
    FatalError,
    ShuttingDown,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
struct Engine5;

impl Engine5 {
    fn new() -> Self {
        Self
    }
}

impl Plugin for Engine5 {
    fn build(&self, app: &mut App) {
        logging::setup(app);

        config::file_paths::create_folders();

        app.init_state::<GameState>();

        app.add_plugins((
            world::WorldPlugin,
            // TODO: restore bevy_ui_navigation (no Bevy 0.18 version available)
            // bevy_ui_navigation::DefaultNavigationPlugins,
            ui::UserInterface,
            config::ConfigPlugin,
            multiplayer::MultiplayerPlugin,
        ));

        app.add_systems(PostUpdate, exit_on_all_windows_closed);

        app.configure_sets(Update, Engine5);

        app.add_systems(
            OnEnter(GameState::ShuttingDown),
            |mut exit: MessageWriter<AppExit>| {
                exit.write(AppExit::Success);
            },
        );
    }
}

pub fn exit_on_all_windows_closed(
    mut next_state: ResMut<NextState<GameState>>,
    windows: Query<&Window>,
) {
    if windows.is_empty() {
        log::info!("No windows are open, exiting");
        next_state.set(GameState::ShuttingDown);
    }
}

pub fn error_handler(
    In(result): In<anyhow::Result<()>>,
    mut commands: Commands,
    mut next_state: ResMut<NextState<GameState>>,
) -> anyhow::Result<()> {
    if let Err(ref error) = result {
        commands.insert_resource(ErrorContext {
            error: error.to_string(),
        });
        next_state.set(GameState::FatalError);
    }
    result
}
