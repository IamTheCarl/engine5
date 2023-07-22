use anyhow::Result;
use bevy::prelude::*;
use bevy_prototype_debug_lines::DebugLinesPlugin;
use std::path::Path;
use world::terrain::BlockRegistry;

pub mod controls;
pub mod file_paths;
pub mod world;

fn main() {
    App::new()
        .add_plugins((DefaultPlugins, Engine5::new()))
        .run();
}

#[derive(Debug, Clone, Copy, Default, Eq, PartialEq, Hash, States)]
pub enum AppState {
    #[default]
    MainMenu,
    PauseMenu,
    InGame,
}

/// A temporary system to test states with.
fn state_switch(keys: Res<Input<KeyCode>>, mut next_state: ResMut<NextState<AppState>>) {
    if keys.just_pressed(KeyCode::Key1) {
        // Main menu state.
        log::info!("Enter Main Menu State");
        next_state.set(AppState::MainMenu);
    }

    if keys.just_pressed(KeyCode::Key2) {
        // Pause state.
        log::info!("Enter Pause State");
        next_state.set(AppState::PauseMenu);
    }

    if keys.just_pressed(KeyCode::Key3) {
        // Running state.
        log::info!("Enter Running State");
        next_state.set(AppState::InGame);
    }
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
        file_paths::create_folders();

        app.add_state::<AppState>();

        app.add_plugins((
            world::WorldPlugin,
            DebugLinesPlugin::default(),
            controls::PlayerControls,
        ));

        app.configure_set(Update, Engine5);
        app.add_systems(Startup, setup.pipe(error_handler).in_set(Engine5));

        app.add_systems(
            Startup,
            apply_deferred
                .after(world::terrain::TerrainPlugin)
                .before(Engine5),
        );

        app.add_systems(Update, state_switch);
    }
}

fn setup(mut commands: Commands, block_registry: Res<BlockRegistry>) -> Result<()> {
    // TODO make this accessible from a menu or terminal.
    commands.insert_resource(DebugRenderSettings {
        cylinders: true,
        cylinder_terrain_checks: false,
        hashing_center_point: false,
        cylinder_cylinder_checks: false,
        terrain_terrain_checks: false,
        terrain_ray_casts: false,
    });

    world::open_world(
        &mut commands,
        &block_registry,
        Path::new(file_paths::SAVE_DIRECTORY).join("test"),
    )?;

    Ok(())
}

pub fn error_handler(In(result): In<anyhow::Result<()>>) {
    if let Err(error) = result {
        log::error!("Fatal Error: {:?}", error);
        std::process::exit(1); // TODO I'd like a more graceful shutdown. One that saves the world and displays an error message in the window.
    }
}

#[derive(Resource, Reflect)]
pub struct DebugRenderSettings {
    cylinders: bool,
    cylinder_terrain_checks: bool,
    hashing_center_point: bool,
    cylinder_cylinder_checks: bool,
    terrain_terrain_checks: bool,
    terrain_ray_casts: bool,
}
