use bevy::prelude::*;
use bevy_prototype_debug_lines::DebugLinesPlugin;
use physics::{Position, Velocity};
use terrain::{
    BlockRegistry, BlockTag, CheckerBoard, FlatWorld, OscillatingHills, TerrainFile, TerrainSpace,
    TerrainSpaceBundle,
};
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

#[derive(StageLabel)]
struct Engine5;

impl Engine5 {
    fn new() -> Self {
        Self
    }
}

impl Plugin for Engine5 {
    fn build(&self, app: &mut App) {
        app.add_plugin(terrain::TerrainPlugin);
        app.add_plugin(physics::PhysicsPlugin);
        app.add_plugin(player::PlayerPlugin);
        app.add_plugin(DebugLinesPlugin::default());
        // TODO enable DBand dithering once you have control of the camera.

        app.add_startup_stage_after(terrain::TerrainPlugin, Engine5, SystemStage::parallel());
        app.add_startup_system_to_stage(Engine5, setup);
    }
}

fn setup(mut commands: Commands, block_registry: Res<BlockRegistry>) {
    let default_tag = BlockTag::try_from("core:default").unwrap();
    let default_data = block_registry.get_by_tag(&default_tag).unwrap();
    let default_block = default_data.spawn();

    commands.spawn((
        TerrainSpaceBundle {
            terrain_space: TerrainSpace::default(),
            position: Position {
                translation: Vec3::ZERO,
                rotation: 0.0,
            },
            file: TerrainFile::new(),
            transform: Transform::default(),
            global_transform: GlobalTransform::default(),
            visibility: Visibility { is_visible: true },
            computed_visibility: ComputedVisibility::default(),
        },
        OscillatingHills {
            block: default_block,
            rate: 512,
            depth: 16,
        },
        // FlatWorld {
        //     block: default_block,
        // },
        // CheckerBoard {
        //     even_block: default_block,
        //     odd_block: default_block,
        //     even_height: 1,
        //     odd_height: 3,
        // },
        Velocity {
            translation: Vec3::ZERO,
            rotational: 0.1,
        },
    ));
}
