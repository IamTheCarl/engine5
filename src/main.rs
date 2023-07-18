use anyhow::Result;
use bevy::prelude::*;
use bevy_prototype_debug_lines::DebugLinesPlugin;
use std::path::Path;
use world::terrain::BlockRegistry;

pub mod file_paths;
pub mod world;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(Engine5::new())
        .run();
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
        app.add_plugin(world::WorldPlugin);
        app.add_plugin(DebugLinesPlugin::default());
        // TODO enable DBand dithering once you have control of the camera.

        app.configure_set(Engine5.after(world::terrain::TerrainPlugin));
        app.add_startup_system(
            apply_system_buffers
                .after(world::terrain::TerrainPlugin)
                .before(Engine5),
        );
        app.add_startup_system(setup.pipe(error_handler).in_set(Engine5));
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
