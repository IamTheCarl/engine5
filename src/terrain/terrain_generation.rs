use bevy::{math::Vec3Swizzles, prelude::*};
use ordered_float::NotNan;

use crate::{
    physics::{Cylinder, Position, SpatialHashOffset, Velocity},
    player::create_player,
};

use super::{Block, Chunk, ChunkPosition, LocalBlockCoordinate, UpdateMesh};

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToGenerate;

#[derive(Component)]
pub struct EmptyWorld;

fn empty_world_generator(
    chunk_position: &ChunkPosition,
    _chunk: &mut Chunk,
    _context: &EmptyWorld,
    commands: &mut Commands,
) {
    // Nothing, absolutely nothing!
    if chunk_position.index == IVec3::ZERO {
        let middle = Chunk::CHUNK_DIAMETER / 2;
        create_player(
            commands,
            Position {
                translation: Vec3::new(middle as f32, middle as f32, middle as f32),
                rotation: 0.0,
            },
        );
    }
}

#[derive(Component)]
pub struct FlatWorld {
    pub block: Block,
}

fn flat_world_generator(
    chunk_position: &ChunkPosition,
    chunk: &mut Chunk,
    context: &FlatWorld,
    commands: &mut Commands,
) {
    if chunk_position.index.y == 0 {
        for (_position, block) in chunk.iter_range_mut(
            LocalBlockCoordinate::new(0, 0, 0),
            LocalBlockCoordinate::new(Chunk::CHUNK_DIAMETER, 1, Chunk::CHUNK_DIAMETER),
        ) {
            *block = Some(context.block);
        }

        if chunk_position.index == IVec3::ZERO {
            let middle = Chunk::CHUNK_DIAMETER / 2;
            create_player(
                commands,
                Position {
                    translation: Vec3::new(middle as f32, 1.0, middle as f32),
                    rotation: 0.0,
                },
            );
        }
    }
}

#[derive(Component)]
pub struct OscillatingHills {
    pub block: Block,
    pub rate: i32,
    pub depth: i32,
}

fn oscillating_hills_generator(
    chunk_position: &ChunkPosition,
    chunk: &mut Chunk,
    context: &OscillatingHills,
    commands: &mut Commands,
) {
    if chunk_position.index.y == 0 {
        let base_offset = chunk_position.as_block_coordinate().xz();

        let rate = Vec2::splat(context.rate as f32);
        let depth = context.depth as f32;
        let half_depth = depth * 0.5;

        let calculate_height_for_index = |position: IVec2| {
            let phase = (((base_offset + position) % context.rate).as_vec2() / rate)
                * std::f64::consts::PI as f32
                * 2.0;

            (((phase.x as f64).sin() as f32 * (phase.y as f64).sin() as f32) * half_depth
                + half_depth)
                .ceil()
        };

        for (position, column) in chunk.iter_columns_mut() {
            let height = calculate_height_for_index(position) as usize;

            column[0..height].fill(Some(context.block));
        }

        if chunk_position.index == IVec3::ZERO {
            let middle = Chunk::CHUNK_DIAMETER / 2;

            let height = calculate_height_for_index(IVec2::splat(middle));
            create_player(
                commands,
                Position {
                    translation: Vec3::new(middle as f32, height, middle as f32),
                    rotation: 0.0,
                },
            );
        }

        if chunk_position.index == IVec3::new(0, 0, 1) {
            let middle = Chunk::CHUNK_DIAMETER / 2;
            let height = calculate_height_for_index(IVec2::splat(middle));

            commands.spawn((
                Cylinder {
                    height: NotNan::new(1.0).unwrap(),
                    radius: NotNan::new(2.0).unwrap(),
                },
                Velocity::default(),
                Transform::default(),
                Position {
                    translation: Vec3::new(0.0, height as f32, 24.0),
                    rotation: 0.0,
                },
            ));
        }
    }
}

#[derive(Component)]
pub struct CheckerBoard {
    pub even_block: Block,
    pub odd_block: Block,
    pub even_height: i32,
    pub odd_height: i32,
}

fn checker_board_generator(
    chunk_position: &ChunkPosition,
    chunk: &mut Chunk,
    context: &CheckerBoard,
    commands: &mut Commands,
) {
    if chunk_position.index.y == 0 {
        let (new_block, height) =
            if (chunk_position.index.x % 2 == 0) ^ (chunk_position.index.z % 2 == 0) {
                (context.even_block, context.even_height)
            } else {
                (context.odd_block, context.odd_height)
            };

        for (_position, block) in chunk.iter_range_mut(
            LocalBlockCoordinate::new(0, 0, 0),
            LocalBlockCoordinate::new(Chunk::CHUNK_DIAMETER, height, Chunk::CHUNK_DIAMETER),
        ) {
            *block = Some(new_block);
        }

        if chunk_position.index == IVec3::ZERO {
            let middle = Chunk::CHUNK_DIAMETER / 2;
            create_player(
                commands,
                Position {
                    translation: Vec3::new(middle as f32, height as f32, middle as f32),
                    rotation: 0.0,
                },
            );
        }

        if chunk_position.index == IVec3::new(0, 0, 1) {
            commands.spawn((
                Cylinder {
                    height: NotNan::new(1.0).unwrap(),
                    radius: NotNan::new(2.0).unwrap(),
                },
                Velocity::default(),
                Transform::default(),
                Position {
                    translation: Vec3::new(0.0, height as f32, 24.0),
                    rotation: 0.0,
                },
            ));
        }
    }
}

fn new_terrain_generator<C, F>(app: &mut App, generator: F)
where
    F: Fn(&ChunkPosition, &mut Chunk, &C, &mut Commands) + Send + Sync + 'static,
    C: Component,
{
    app.add_system(
        move |mut commands: Commands,
              terrain_spaces: Query<&C>,
              mut to_generate: Query<(
            Entity,
            &ChunkPosition,
            &mut Chunk,
            &Parent,
            With<ToGenerate>,
        )>| {
            // First, generate all the terrain.
            // TODO the generation calls should be done outside of the ECS so that this system becomes non-blocking.
            to_generate.for_each_mut(|(entity, position, mut chunk, parent, _to_generate)| {
                if let Ok(context) = terrain_spaces.get(parent.get()) {
                    generator(position, &mut chunk, context, &mut commands);

                    commands.entity(entity).remove::<ToGenerate>().insert((
                        UpdateMesh,
                        position.as_transform(),
                        SpatialHashOffset {
                            translation: Vec3::new(8.0, 0.0, 8.0),
                        },
                    ));
                }
            });
        },
    );
}

pub fn register_terrain_generators(app: &mut App) {
    new_terrain_generator(app, flat_world_generator);
    new_terrain_generator(app, empty_world_generator);
    new_terrain_generator(app, oscillating_hills_generator);
    new_terrain_generator(app, checker_board_generator);
}
