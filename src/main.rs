use bevy::prelude::*;
use bevy_prototype_debug_lines::DebugLinesPlugin;
// use bevy_flycam::PlayerPlugin;

mod physics;
mod player;
mod terrain;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(Engine5::new())
        .run();
}

struct Engine5 {}

impl Engine5 {
    fn new() -> Self {
        Self {}
    }
}

impl Plugin for Engine5 {
    fn build(&self, app: &mut App) {
        // TODO should probably make these proper plugins.
        terrain::setup(app);
        physics::setup(app);

        app.add_plugin(player::PlayerPlugin);
        app.add_plugin(DebugLinesPlugin::default());
        // TODO enable DBand dithering once you have control of the camera.

        // app.add_startup_system(setup);
    }
}

// fn setup(
//     mut commands: Commands,
//     mut meshes: ResMut<Assets<Mesh>>,
//     mut materials: ResMut<Assets<StandardMaterial>>,
// ) {
//     // // plane
//     // commands.spawn(PbrBundle {
//     //     mesh: meshes.add(Mesh::from(shape::Plane { size: 5.0 })),
//     //     material: materials.add(Color::rgb(0.3, 0.5, 0.3).into()),
//     //     ..Default::default()
//     // });
//     // // cube
//     // commands.spawn(PbrBundle {
//     //     mesh: meshes.add(Mesh::from(shape::Cube { size: 1.0 })),
//     //     material: materials.add(Color::rgb(0.8, 0.7, 0.6).into()),
//     //     transform: Transform::from_xyz(0.0, 0.5, 0.0),
//     //     ..Default::default()
//     // });
//     // // light
//     // commands.spawn(PointLightBundle {
//     //     transform: Transform::from_xyz(4.0, 8.0, 4.0),
//     //     ..Default::default()
//     // });
// }
