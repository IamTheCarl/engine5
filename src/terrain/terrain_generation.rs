use bevy::prelude::*;

use super::{
    terrain_space::ParentTerrainSpace, Block, BlockLocalCoordinate, Chunk, ChunkIndex,
    ChunkPosition, UpdateMesh,
};

// TODO should this be configurable? Maybe something to auto benchmark on startup?
const GENERATION_BATCH_SIZE: usize = 1;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToGenerate;

#[derive(Component)]
pub struct EmptyWorld;

fn empty_world_generator(_chunk_index: ChunkIndex, _chunk: &mut Chunk, _context: &EmptyWorld) {
    // Nothing, absolutely nothing!
}

#[derive(Component)]
pub struct FlatWorld {
    pub block: Block,
}

fn flat_world_generator(chunk_index: ChunkIndex, chunk: &mut Chunk, context: &FlatWorld) {
    // TODO spawn the player.

    if chunk_index.y == 0 {
        for (_position, block) in chunk.iter_range_mut(
            IVec3::new(0, 0, 0),
            IVec3::new(
                Chunk::CHUNK_DIAMETER as i32,
                1,
                Chunk::CHUNK_DIAMETER as i32,
            ),
        ) {
            *block = Some(context.block);
        }

        // FIXME a single block like this can't be sat on by the player. They just fall right through.
        // if let Some(block) = chunk.get_block_local_mut(BlockLocalCoordinate::new(0, 1, 0)) {
        //     *block = Some(context.block);
        // }
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
        &ParentTerrainSpace,
        With<ToGenerate>,
    )>,
)
where
    F: Fn(ChunkIndex, &mut Chunk, &C) + Send + Sync,
    C: Component,
{
    move |mut commands: Commands,
          terrain_spaces: Query<&C>,
          mut to_generate: Query<(
        Entity,
        &ChunkPosition,
        &mut Chunk,
        &ParentTerrainSpace,
        With<ToGenerate>,
    )>| {
        // First, generate all the terrain.
        to_generate.par_for_each_mut(
            GENERATION_BATCH_SIZE,
            |(_entity, position, mut chunk, parent, _to_generate)| {
                // TODO I'd like to filter these out ahead of time if I can.
                if let Ok(context) = terrain_spaces.get(parent.get()) {
                    generator(position.index, &mut chunk, context);
                }
            },
        );

        // Now that they're all generated, we can remove the tags to generate the chunks.
        // We have to do this now rather than with the generation because the command queue can't be shared between
        // threads in an efficient way.
        for (entity, _position, _chunk, _parent, _to_generate) in to_generate.iter_mut() {
            commands
                .entity(entity)
                .remove::<ToGenerate>()
                .insert(UpdateMesh);
        }
    }
}

pub fn register_terrain_generators(app: &mut App) {
    app.add_system(new_terrain_generator(flat_world_generator));
    app.add_system(new_terrain_generator(empty_world_generator));
}
