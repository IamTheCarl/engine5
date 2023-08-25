use bevy::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Resource, Serialize, Deserialize)]
pub struct GraphicsConfig {}

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

impl Plugin for GraphicsConfigPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, |mut commands: Commands| {
            commands.insert_resource(DebugRenderSettings {
                cylinders: true,
                cylinder_terrain_checks: false,
                hashing_center_point: false,
                cylinder_cylinder_checks: false,
                terrain_terrain_checks: false,
                terrain_ray_casts: false,
            });
        });
    }
}
