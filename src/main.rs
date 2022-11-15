use bevy::prelude::*;
use bevy_flycam::PlayerPlugin;
use terrain::{BlockRegistry, BlockTag};

mod terrain;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_plugin(Engine5::new())
        .add_plugin(PlayerPlugin) // TODO remove when you implement your own player.
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
        app.add_startup_system(setup);
    }
}

fn setup(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
) {
    // plane
    commands.spawn(PbrBundle {
        mesh: meshes.add(Mesh::from(shape::Plane { size: 5.0 })),
        material: materials.add(Color::rgb(0.3, 0.5, 0.3).into()),
        ..Default::default()
    });
    // cube
    commands.spawn(PbrBundle {
        mesh: meshes.add(Mesh::from(shape::Cube { size: 1.0 })),
        material: materials.add(Color::rgb(0.8, 0.7, 0.6).into()),
        transform: Transform::from_xyz(0.0, 0.5, 0.0),
        ..Default::default()
    });
    // light
    commands.spawn(PointLightBundle {
        transform: Transform::from_xyz(4.0, 8.0, 4.0),
        ..Default::default()
    });

    // chunk

    let block_registry = BlockRegistry::load().unwrap();
    let stone_tag = BlockTag::try_from("core:stone").unwrap();
    let stone_data = block_registry.get_by_tag(&stone_tag).unwrap();
    let stone_block = stone_data.spawn();

    let chunk = terrain::Chunk::new(Some(stone_block));
    let chunk_mesh = chunk.build_mesh();

    commands.spawn(chunk);
    commands.spawn(PbrBundle {
        mesh: meshes.add(chunk_mesh),
        material: materials.add(Color::rgb(0.8, 0.2, 0.2).into()),
        transform: Transform::from_xyz(8.0, 0.5, 8.0),
        ..Default::default()
    });
}
