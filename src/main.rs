use bevy::prelude::*;
use bevy_flycam::PlayerPlugin;
use terrain::{BlockRegistry, BlockTag};

use crate::terrain::{BlockLocalCoordinate, TerrainMaterial};

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
        app.add_plugin(MaterialPlugin::<terrain::TerrainMaterial>::default());
        app.add_startup_system(setup);
    }
}

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut terrain_material: ResMut<Assets<terrain::TerrainMaterial>>,
    mut shaders: ResMut<Assets<Shader>>,
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

    use terrain::Chunk;

    let mut chunk = Chunk::new(Some(stone_block));
    for x in 0..Chunk::CHUNK_DIAMETER {
        for y in 0..Chunk::CHUNK_DIAMETER {
            for z in 0..Chunk::CHUNK_DIAMETER {
                let x = x as f32;
                let y = y as f32;
                let z = z as f32;

                let height = ((x / 16.0) * std::f64::consts::PI as f32).sin()
                    * ((z / 16.0) * std::f64::consts::PI as f32).sin()
                    * 16.0;
                if y > height {
                    chunk
                        .set_block_local(BlockLocalCoordinate::new(x as i8, y as i8, z as i8), None)
                        .ok();
                }
            }
        }
    }

    let chunk_mesh = chunk.build_mesh();

    let terrain_image: Handle<Image> = asset_server.load("terrain/default-color.png");

    commands.spawn(chunk);
    commands.spawn(MaterialMeshBundle {
        mesh: meshes.add(chunk_mesh),
        material: terrain_material.add(TerrainMaterial {}), // TODO recycle this material for ALL terrain.
        transform: Transform::from_xyz(8.0, 0.5, 8.0),
        ..Default::default()
    });
}
