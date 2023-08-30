use bevy::{app::AppExit, asset::ChangeWatcher, prelude::*, window::ExitCondition};
use bevy_console::{ConsoleConfiguration, ConsolePlugin};
use bevy_prototype_debug_lines::DebugLinesPlugin;
use bevy_ui_navigation::DefaultNavigationPlugins;
use std::time::Duration;

use crate::ui::ErrorContext;

pub mod config;
mod ui;
pub mod world;

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins
                .set(AssetPlugin {
                    // Tell the asset server to watch for asset changes on disk:
                    watch_for_changes: ChangeWatcher::with_delay(Duration::from_millis(200)),
                    ..default()
                })
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        title: "Engine 5".to_string(),
                        ..Default::default()
                    }),
                    exit_condition: ExitCondition::DontExit, // We handle the exit ourselves.
                    ..default()
                }),
            ConsolePlugin,
            Engine5::new(),
        ))
        .insert_resource(ConsoleConfiguration::default())
        .run();
}

#[derive(Debug, Clone, Copy, Default, Eq, PartialEq, Hash, States)]
pub enum AppState {
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
        // I wait until here to do this so that at least the log should be working.
        config::file_paths::create_folders();

        app.add_state::<AppState>();

        app.add_plugins((
            world::WorldPlugin,
            DebugLinesPlugin::default(),
            DefaultNavigationPlugins,
            ui::UserInterface,
            config::ConfigPlugin,
        ));

        app.add_systems(PostUpdate, exit_on_all_windows_closed);

        app.configure_set(Update, Engine5);

        app.add_systems(
            Startup,
            apply_deferred
                .after(world::terrain::TerrainPlugin)
                .before(Engine5),
        );

        // TODO this is a temporary way to shutdown. I want this to verify all worlds have been saved and closed before application exits.
        app.add_systems(
            OnEnter(AppState::ShuttingDown),
            |mut exit_event: EventWriter<AppExit>| exit_event.send(AppExit),
        );
    }
}

// Reworked from the original in Bevy to
pub fn exit_on_all_windows_closed(
    mut next_state: ResMut<NextState<AppState>>,
    windows: Query<&Window>,
) {
    if windows.is_empty() {
        log::info!("No windows are open, exiting");
        next_state.set(AppState::ShuttingDown);
    }
}

pub fn error_handler(
    In(result): In<anyhow::Result<()>>,
    mut commands: Commands,
    mut next_state: ResMut<NextState<AppState>>,
) {
    // Only switch to error state if there was actually an error.
    if let Err(error) = result {
        commands.insert_resource(ErrorContext { error });
        next_state.set(AppState::FatalError);
    }
}
