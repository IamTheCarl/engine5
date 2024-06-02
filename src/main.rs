use bevy::{app::AppExit, log::LogPlugin, prelude::*, window::ExitCondition};
use bevy_console::{ConsoleConfiguration, ConsolePlugin};
use console_scripts::ConsoleScripts;

pub mod config;
pub mod console_scripts;
mod logging;
pub mod multiplayer;
// pub mod ui;
pub mod world;

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins
                .set(AssetPlugin::default())
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        title: "Engine 5".to_string(),
                        ..Default::default()
                    }),
                    exit_condition: ExitCondition::DontExit, // We handle the exit ourselves.
                    ..default()
                })
                .disable::<LogPlugin>(),
            ConsolePlugin,
            ConsoleScripts,
            Engine5::new(),
        ))
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

        // I wait until here to do this so that at least the log should be working.
        config::file_paths::create_folders();

        app.init_state::<GameState>();

        app.add_plugins((
            world::WorldPlugin,
            // ui::UserInterface,
            config::ConfigPlugin,
            multiplayer::MultiplayerPlugin,
        ));

        app.add_systems(PostUpdate, exit_on_all_windows_closed);

        app.configure_sets(Update, Engine5);

        app.add_systems(
            Startup,
            apply_deferred
                .after(world::terrain::TerrainPlugin)
                .before(Engine5),
        );

        // TODO this is a temporary way to shutdown. I want this to verify all worlds have been saved and closed before application exits.
        app.add_systems(
            OnEnter(GameState::ShuttingDown),
            |mut exit_event: EventWriter<AppExit>| {
                exit_event.send(AppExit::Success);
            },
        );
    }
}

// Reworked from the original in Bevy to
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
    mut next_state: ResMut<NextState<GameState>>,
) {
    // Only switch to error state if there was actually an error.
    if let Err(error) = result {
        log::error!("Switching to error state. Caused by: {error}");
        // commands.insert_resource(ErrorContext { error });
        next_state.set(GameState::FatalError);
    }
}
