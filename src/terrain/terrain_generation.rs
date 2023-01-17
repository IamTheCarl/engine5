use bevy::{math::Vec3Swizzles, prelude::*};

use crate::physics::SpatialHashOffset;

use super::{Block, Chunk, ChunkPosition, LocalBlockCoordinate, UpdateMesh};

const GENERATION_BATCH_SIZE: usize = 1;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToGenerate;

#[derive(Component)]
pub struct EmptyWorld;

fn empty_world_generator(
    _chunk_position: &ChunkPosition,
    _chunk: &mut Chunk,
    _context: &EmptyWorld,
) {
    // Nothing, absolutely nothing!
    // TODO spawn the player.
}

#[derive(Component)]
pub struct FlatWorld {
    pub block: Block,
}

fn flat_world_generator(chunk_position: &ChunkPosition, chunk: &mut Chunk, context: &FlatWorld) {
    // TODO spawn the player.

    if chunk_position.index.y == 0 {
        for (_position, block) in chunk.iter_range_mut(
            LocalBlockCoordinate::new(0, 0, 0),
            LocalBlockCoordinate::new(Chunk::CHUNK_DIAMETER, 1, Chunk::CHUNK_DIAMETER),
        ) {
            *block = Some(context.block);
        }

        // FIXME a single block like this can't be sat on by the player. They just fall right through.
        // if let Some(block) = chunk.get_block_local_mut(BlockLocalCoordinate::new(0, 1, 0)) {
        //     *block = Some(context.block);
        // }
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
) {
    // TODO spawn the player.

    if chunk_position.index.y == 0 {
        let base_offset = chunk_position.as_block_coordinate().xz();

        let rate = Vec2::splat(context.rate as f32);
        let depth = context.depth as f32;
        let half_depth = depth * 0.5;

        for (position, column) in chunk.iter_columns_mut() {
            let phase = (((base_offset + position) % context.rate).as_vec2() / rate)
                * std::f64::consts::PI as f32
                * 2.0;

            let height = (((phase.x as f64).sin() as f32 * (phase.y as f64).sin() as f32)
                * half_depth
                + half_depth)
                .ceil() as usize;

            column[0..height].fill(Some(context.block));
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
) {
    // TODO spawn the player.

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
    }
}

fn new_terrain_generator<C, F>(
    generator: F,
) -> impl Fn(
    Commands,
    Query<&C>,
    Query<(
        Entity,
        &ChunkPosition,
        &mut Chunk,
        &Parent,
        With<ToGenerate>,
    )>,
)
where
    F: Fn(&ChunkPosition, &mut Chunk, &C) + Send + Sync,
    C: Component,
{
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
        to_generate.par_for_each_mut(
            GENERATION_BATCH_SIZE,
            |(_entity, position, mut chunk, parent, _to_generate)| {
                // TODO I'd like to filter these out ahead of time if I can.
                if let Ok(context) = terrain_spaces.get(parent.get()) {
                    generator(position, &mut chunk, context);
                }
            },
        );

        // Now that they're all generated, we can remove the tags to generate the chunks.
        // We have to do this now rather than with the generation because the command queue can't be shared between
        // threads in an efficient way.
        for (entity, position, _chunk, _parent, _to_generate) in to_generate.iter_mut() {
            commands.entity(entity).remove::<ToGenerate>().insert((
                UpdateMesh,
                position.as_transform(),
                SpatialHashOffset {
                    translation: Vec3::new(8.0, 0.0, 8.0),
                },
            ));
        }
    }
}

pub fn register_terrain_generators(app: &mut App) {
    app.add_system(new_terrain_generator(flat_world_generator));
    app.add_system(new_terrain_generator(empty_world_generator));
    app.add_system(new_terrain_generator(oscillating_hills_generator));
    app.add_system(new_terrain_generator(checker_board_generator));
}
