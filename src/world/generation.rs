use anyhow::Result;
use bevy::{math::Vec3Swizzles, prelude::*};
use serde::{Deserialize, Serialize};

use crate::{
    world::physics::{Cylinder, Position, Velocity},
    world::terrain::storage::ToSaveTerrain,
    world::{player::PlayerEntity, spatial_entities::SpatialHashOffset},
};

use super::{
    spatial_entities::storage::{EntityStorage, ToSaveSpatial},
    terrain::{
        Block, Chunk, ChunkIndex, ChunkPosition, LocalBlockCoordinate, TerrainSpace, UpdateMesh,
    },
};

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToGenerateTerrain;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToGenerateSpatial;

#[derive(Serialize, Deserialize)]
pub struct EmptyWorld;

impl WorldGenerator for EmptyWorld {
    fn generate_terrain(&self, _chunk_position: &ChunkPosition) -> Result<Option<Chunk>> {
        // Nothing, absolutely nothing!
        Ok(None)
    }

    fn generate_spatial(
        &self,
        chunk_position: &ChunkPosition,
        storage: &mut EntityStorage,
        commands: &mut Commands,
    ) -> Result<()> {
        // Spawn a player.
        if chunk_position.index == ChunkIndex::ZERO {
            let middle = Chunk::CHUNK_DIAMETER / 2;
            PlayerEntity::spawn(
                commands,
                storage,
                Position {
                    translation: Vec3::new(middle as f32, middle as f32, middle as f32),
                    rotation: 0.0,
                },
            )?;
        }

        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
pub struct FlatWorld {
    pub block: Block,
}

impl WorldGenerator for FlatWorld {
    fn generate_terrain(&self, chunk_position: &ChunkPosition) -> Result<Option<Chunk>> {
        if chunk_position.index.y == 0 {
            let mut chunk = Chunk::new(None);

            for (_position, block) in chunk.iter_range_mut(
                LocalBlockCoordinate::new(0, 0, 0),
                LocalBlockCoordinate::new(Chunk::CHUNK_DIAMETER, 1, Chunk::CHUNK_DIAMETER),
            ) {
                *block = Some(self.block);
            }

            Ok(Some(chunk))
        } else {
            Ok(None)
        }
    }

    fn generate_spatial(
        &self,
        chunk_position: &ChunkPosition,
        storage: &mut EntityStorage,
        commands: &mut Commands,
    ) -> Result<()> {
        if chunk_position.index == ChunkIndex::ZERO {
            let middle = Chunk::CHUNK_DIAMETER / 2;
            PlayerEntity::spawn(
                commands,
                storage,
                Position {
                    translation: Vec3::new(middle as f32, 1.0, middle as f32),
                    rotation: 0.0,
                },
            )?;
        }

        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
pub struct OscillatingHills {
    pub block: Block,
    pub rate: i32,
    pub depth: i32,
}

impl OscillatingHills {
    fn calculate_height_for_index(&self, base_offset: IVec2, position: IVec2) -> f32 {
        let rate = Vec2::splat(self.rate as f32);
        let depth = self.depth as f32;
        let half_depth = depth * 0.5;

        let phase = (((base_offset + position) % self.rate).as_vec2() / rate)
            * std::f64::consts::PI as f32
            * 2.0;

        (((phase.x as f64).sin() as f32 * (phase.y as f64).sin() as f32) * half_depth + half_depth)
            .ceil()
    }
}

impl WorldGenerator for OscillatingHills {
    fn generate_terrain(&self, chunk_position: &ChunkPosition) -> Result<Option<Chunk>> {
        if chunk_position.index.y == 0 {
            let mut chunk = Chunk::new(None);

            let base_offset = chunk_position.as_block_coordinate().xz();

            for (position, column) in chunk.iter_columns_mut() {
                let height = self.calculate_height_for_index(base_offset, position) as usize;

                column[0..height].fill(Some(self.block));
            }

            Ok(Some(chunk))
        } else {
            Ok(None)
        }
    }

    fn generate_spatial(
        &self,
        chunk_position: &ChunkPosition,
        storage: &mut EntityStorage,
        commands: &mut Commands,
    ) -> Result<()> {
        let base_offset = chunk_position.as_block_coordinate().xz();

        // TODO we need to create a "DynamicTerrain" struct to manage this type of entity.
        // if chunk_position.index == ChunkIndex::new(-1, 1, 0) {
        //     let (storable, tree) = storage.new_storable_component_with_tree()?;

        //     commands.spawn((
        //         storable,
        //         TerrainSpaceBundle {
        //             terrain_space: TerrainSpace::local(SingleFilledChunk { block: self.block }),
        //             position: Position {
        //                 translation: Vec3::new(-24.0, 32.0, 0.0),
        //                 rotation: 0.0,
        //             },
        //             storage: TerrainStorage::Local { tree },
        //             transform: Transform::default(),
        //             global_transform: GlobalTransform::default(),
        //             visibility: Visibility::Inherited,
        //             computed_visibility: ComputedVisibility::default(),
        //         },
        //         Velocity {
        //             translation: Vec3::new(0.0, 0.0, 0.0),
        //             rotational: 0.2,
        //         },
        //     ));
        // }

        if chunk_position.index == ChunkIndex::ZERO {
            let middle = Chunk::CHUNK_DIAMETER / 2;

            let height = self.calculate_height_for_index(base_offset, IVec2::splat(middle));
            PlayerEntity::spawn(
                commands,
                storage,
                Position {
                    translation: Vec3::new(middle as f32, height, middle as f32),
                    rotation: 0.0,
                },
            )?;
        }

        if chunk_position.index == ChunkIndex::new(0, 0, 1) {
            let middle = Chunk::CHUNK_DIAMETER / 2;
            let height = self.calculate_height_for_index(base_offset, IVec2::splat(middle));

            commands.spawn((
                Cylinder {
                    height: 1.0,
                    radius: 2.0,
                },
                Velocity::default(),
                Transform::default(),
                Position {
                    translation: Vec3::new(0.0, height, 24.0),
                    rotation: 0.0,
                },
                // storage.new_storable_component()?,
                ToSaveSpatial,
            ));
        }

        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
pub struct CheckerBoard {
    pub even_block: Block,
    pub odd_block: Block,
    pub even_height: i32,
    pub odd_height: i32,
}

impl CheckerBoard {
    fn get_block_and_height(&self, chunk_position: &ChunkPosition) -> (Block, i32) {
        if (chunk_position.index.x % 2 == 0) ^ (chunk_position.index.z % 2 == 0) {
            (self.even_block, self.even_height)
        } else {
            (self.odd_block, self.odd_height)
        }
    }
}

impl WorldGenerator for CheckerBoard {
    fn generate_terrain(&self, chunk_position: &ChunkPosition) -> Result<Option<Chunk>> {
        if chunk_position.index.y == 0 {
            let mut chunk = Chunk::new(None);

            let (new_block, height) = self.get_block_and_height(chunk_position);

            for (_position, block) in chunk.iter_range_mut(
                LocalBlockCoordinate::new(0, 0, 0),
                LocalBlockCoordinate::new(Chunk::CHUNK_DIAMETER, height, Chunk::CHUNK_DIAMETER),
            ) {
                *block = Some(new_block);
            }

            Ok(Some(chunk))
        } else {
            Ok(None)
        }
    }

    fn generate_spatial(
        &self,
        chunk_position: &ChunkPosition,
        storage: &mut EntityStorage,
        commands: &mut Commands,
    ) -> Result<()> {
        let (_new_block, height) = self.get_block_and_height(chunk_position);

        if chunk_position.index == ChunkIndex::ZERO {
            let middle = Chunk::CHUNK_DIAMETER / 2;
            PlayerEntity::spawn(
                commands,
                storage,
                Position {
                    translation: Vec3::new(middle as f32, height as f32, middle as f32),
                    rotation: 0.0,
                },
            )?;
        }

        if chunk_position.index == ChunkIndex::new(0, 0, 1) {
            commands.spawn((
                Cylinder {
                    height: 1.0,
                    radius: 2.0,
                },
                Velocity::default(),
                Transform::default(),
                Position {
                    translation: Vec3::new(0.0, height as f32, 24.0),
                    rotation: 0.0,
                },
                // storage.new_storable_component()?,
                ToSaveSpatial,
            ));
        }

        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
pub struct SingleFilledChunk {
    pub block: Block,
}

impl WorldGenerator for SingleFilledChunk {
    fn generate_terrain(&self, chunk_position: &ChunkPosition) -> Result<Option<Chunk>> {
        if chunk_position.index == ChunkIndex::ZERO
            || chunk_position.index == ChunkIndex::new(1, 0, 0)
        {
            let mut chunk = Chunk::new(None);

            for (_position, block) in chunk.iter_mut() {
                *block = Some(self.block);
            }

            Ok(Some(chunk))
        } else {
            Ok(None)
        }
    }

    fn generate_spatial(
        &self,
        _chunk_position: &ChunkPosition,
        _storage: &mut EntityStorage,
        _commands: &mut Commands,
    ) -> Result<()> {
        Ok(())
    }
}

pub trait WorldGenerator {
    fn generate_terrain(&self, chunk_position: &ChunkPosition) -> Result<Option<Chunk>>;
    fn generate_spatial(
        &self,
        chunk_position: &ChunkPosition,
        storage: &mut EntityStorage,
        commands: &mut Commands,
    ) -> Result<()>;
}

macro_rules! register_terrain_generators {
    ($($x:ident),*) => {
        #[derive(Deserialize, Serialize)]
        pub enum WorldGeneratorEnum {
            None,
            $(
                $x($x),
            )*
        }

        impl Default for WorldGeneratorEnum {
            fn default() -> Self {
                Self::None
            }
        }

        impl std::ops::Deref for WorldGeneratorEnum {
            type Target = dyn WorldGenerator;

            fn deref(&self) -> &Self::Target {
                match self {
                    Self::None => &EmptyWorld,
                    $(
                        Self::$x(value) => value,
                    )*
                }
            }
        }


        $(
            impl From<$x> for WorldGeneratorEnum {
                fn from(value: $x) -> Self {
                    Self::$x(value)
                }
            }
        )*
    };
}

register_terrain_generators!(FlatWorld, OscillatingHills, CheckerBoard, SingleFilledChunk);

type GenerateTerrainQuery<'a, 'b, 'c> = Query<
    'a,
    'b,
    (
        Entity,
        &'c ChunkPosition,
        &'c Parent,
        With<ToGenerateTerrain>,
        Without<Chunk>,
    ),
>;
type GenerateEntitiesQuery<'a, 'b, 'c> =
    Query<'a, 'b, (Entity, &'c ChunkPosition, &'c Parent), With<ToGenerateSpatial>>;

fn generate_spatial_entities(
    mut commands: Commands,
    terrain_spaces: Query<&TerrainSpace>,
    to_generate: GenerateEntitiesQuery,
    storage: Option<ResMut<EntityStorage>>,
) -> Result<()> {
    if let Some(mut storage) = storage {
        // TODO the generation calls should be done outside of the ECS so that this system becomes non-blocking.
        for (entity_id, position, parent) in to_generate.iter() {
            if let Ok(terrain_space) = terrain_spaces.get(parent.get()) {
                terrain_space
                    .generator
                    .generate_spatial(position, &mut storage, &mut commands)?;

                let mut entity = commands.entity(entity_id);

                entity.remove::<ToGenerateSpatial>().insert(ToSaveSpatial);
            }
        }
    }

    Ok(())
}

fn generate_terrain(
    mut commands: Commands,
    mut terrain_spaces: Query<&mut TerrainSpace>,
    to_generate: GenerateTerrainQuery,
) -> Result<()> {
    // TODO the generation calls should be done outside of the ECS so that this system becomes non-blocking.
    for (entity_id, position, parent, _to_generate, _without_chunk) in to_generate.iter() {
        if let Ok(mut terrain_space) = terrain_spaces.get_mut(parent.get()) {
            let chunk = terrain_space.generator.generate_terrain(position)?;

            let mut entity = commands.entity(entity_id);

            entity.remove::<ToGenerateTerrain>().insert((
                UpdateMesh, // TODO this should probably be controlled by some kind of mobile entity that actually loads terrain.
                position.as_transform(),
                SpatialHashOffset {
                    translation: Vec3::new(8.0, 0.0, 8.0),
                },
                ToSaveTerrain, // We just went through the effort to generate it, might as well save it while we're at it.
            ));

            if let Some(chunk) = chunk {
                entity.insert(chunk);
                terrain_space.non_empty_chunks.insert(entity_id);
            }
        }
    }

    Ok(())
}

pub fn setup_terrain_generation(app: &mut App) {
    app.add_system(generate_terrain.pipe(crate::error_handler));
    app.add_system(generate_spatial_entities.pipe(crate::error_handler));
}
