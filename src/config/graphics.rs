use bevy::prelude::*;
use serde::{Deserialize, Serialize};

use super::Config;

#[derive(Resource, Serialize, Deserialize)]
pub struct GraphicsConfig {
    start_in_fullscreen_mode: bool,
    chunk_view_radius: u16,
}

impl Default for GraphicsConfig {
    fn default() -> Self {
        Self {
            start_in_fullscreen_mode: true,
            chunk_view_radius: 8,
        }
    }
}

impl Config for GraphicsConfig {
    const CONFIG_FILE: &'static str = "graphics.yaml";
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct GraphicsConfigPlugin;

// TODO make this accessible from a terminal.
#[derive(Resource, Reflect)]
pub struct DebugRenderSettings {
    pub cylinders: bool,
    pub cylinder_terrain_checks: bool,
    pub hashing_center_point: bool,
    pub cylinder_cylinder_checks: bool,
    pub terrain_terrain_checks: bool,
    pub terrain_ray_casts: bool,
}

impl Default for DebugRenderSettings {
    fn default() -> Self {
        Self {
            cylinders: true,
            cylinder_terrain_checks: false,
            hashing_center_point: false,
            cylinder_cylinder_checks: false,
            terrain_terrain_checks: false,
            terrain_ray_casts: false,
        }
    }
}

impl Plugin for GraphicsConfigPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, |mut commands: Commands| {
            commands.insert_resource(DebugRenderSettings::default());
            commands.insert_resource(GraphicsConfig::load_or_default());
        });
    }
}
