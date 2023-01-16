use super::{
    terrain_file::{TerrainFile, ToLoad},
    Chunk, ChunkBundle, ChunkIndex, ChunkPosition,
};
use crate::physics::{Position, SpatialHashOffset};
use bevy::{prelude::*, time::FixedTimestep};
use std::collections::HashMap;

const CHUNK_TIME_TO_LIVE_SECONDS: usize = 5;

#[derive(Component)]
pub struct LoadsTerrain {
    pub radius: i32,
}

#[derive(Component)]
pub struct LoadAllTerrain;

#[derive(Component)]
struct ActiveTerrainTimer {
    // How many ticks are we to keep it alive for.
    deadline: usize,
}

#[derive(Resource)]
struct TerrainTime {
    time: usize,
}

#[derive(Component, Default)]
pub struct TerrainSpace {
    loaded_terrain: HashMap<ChunkIndex, Entity>,
}

#[derive(Component)]
pub struct ParentTerrainSpace {
    parent: Entity,
}

impl ParentTerrainSpace {
    pub fn get(&self) -> Entity {
        self.parent
    }
}

impl TerrainSpace {
    fn load_chunk(
        &mut self,
        commands: &mut Commands,
        space_entity: Entity,
        chunk_position: ChunkPosition,
        despawn_deadline: Option<usize>,
    ) -> bool {
        match self.loaded_terrain.entry(chunk_position.index) {
            std::collections::hash_map::Entry::Occupied(already_loaded) => {
                // Oh, already loaded? Just update the timer then.
                if let Some(despawn_deadline) = despawn_deadline {
                    commands
                        .entity(*already_loaded.get())
                        .insert(ActiveTerrainTimer {
                            deadline: despawn_deadline,
                        });
                }

                false
            }
            std::collections::hash_map::Entry::Vacant(to_load) => {
                let position = chunk_position.as_position();

                let mut entity = commands.spawn((
                    ChunkBundle {
                        chunk: Chunk::new(None),
                        transform: Transform::default(),
                        position,
                        chunk_position,
                        offset: SpatialHashOffset {
                            translation: Vec3::new(8.0, 0.0, 8.0),
                        },
                    },
                    ToLoad, // Mark that this terrain needs to be loaded.
                    ParentTerrainSpace {
                        parent: space_entity,
                    },
                ));

                if let Some(despawn_deadline) = despawn_deadline {
                    entity.insert(ActiveTerrainTimer {
                        deadline: despawn_deadline,
                    });
                }

                let entity = entity.id();

                // We add as children so we can despawn recursively.
                to_load.insert(entity);

                // Indicate that we're going to load this.
                true
            }
        }
    }
}

#[derive(Bundle)]
pub struct TerrainSpaceBundle {
    pub terrain_space: TerrainSpace,
    pub position: Position,
    pub file: TerrainFile,
    pub global_transform: GlobalTransform,
    pub visibility: Visibility,
    pub computed_visibility: ComputedVisibility,
}

fn load_all_terrain(
    mut commands: Commands,
    mut spaces: Query<(
        Entity,
        &mut TerrainSpace,
        &TerrainFile,
        With<LoadAllTerrain>,
    )>,
) {
    for (space_entity, mut space, terrain_file, _keep_all_terrain_loaded) in spaces.iter_mut() {
        for position in terrain_file.iter_chunk_indexes() {
            // Returns false if the chunk was already loaded.
            // We'll just assume that if anything is loaded, everything is loaded.
            // Also we don't put a despawn deadline on this, because we assume it'll never unload.
            if !space.load_chunk(&mut commands, space_entity, position, None) {
                break;
            }
        }

        commands.entity(space_entity).remove::<LoadAllTerrain>();
    }
}

fn load_terrain(
    mut commands: Commands,
    terrain_loaders: Query<(&Position, &LoadsTerrain)>,
    mut terrain_spaces: Query<(Entity, &Position, &mut TerrainSpace, With<TerrainFile>)>,
    terrain_time: Res<TerrainTime>,
) {
    let despawn_deadline = terrain_time.time + CHUNK_TIME_TO_LIVE_SECONDS;

    for (space_entity, space_position, mut space, _terrain_file) in terrain_spaces.iter_mut() {
        for (loader_position, loads_terrain) in terrain_loaders.iter() {
            let loader_position_in_chunk_space = space_position.inverse_quat()
                * (loader_position.translation - space_position.translation);
            let chunk_index =
                (loader_position_in_chunk_space / Chunk::CHUNK_DIAMETER as f32).as_ivec3();

            for x in -loads_terrain.radius..loads_terrain.radius {
                for z in -loads_terrain.radius..loads_terrain.radius {
                    for y in -loads_terrain.radius..loads_terrain.radius {
                        let chunk_index = chunk_index + ChunkIndex::new(x, y, z);

                        space.load_chunk(
                            &mut commands,
                            space_entity,
                            ChunkPosition { index: chunk_index },
                            Some(despawn_deadline),
                        );
                    }
                }
            }
        }
    }
}

fn clean_up_chunks(
    mut commands: Commands,
    chunks: Query<(
        Entity,
        With<Chunk>,
        &ActiveTerrainTimer,
        &ChunkPosition,
        &ParentTerrainSpace,
    )>,
    mut terrain_spaces: Query<&mut TerrainSpace>,
    mut terrain_time: ResMut<TerrainTime>,
) {
    for (chunk_entity, _chunk, timer, position, terrain_space) in chunks.iter() {
        if timer.deadline <= terrain_time.time {
            // Your time has come. It is time to die.
            commands.entity(chunk_entity).despawn();

            let mut space = terrain_spaces
                .get_mut(terrain_space.get())
                .expect("Chunk without a space found.");
            space.loaded_terrain.remove(&position.index);
        }
    }

    terrain_time.time += 1;
}

fn setup_system(mut commands: Commands) {
    commands.insert_resource(TerrainTime { time: 0 })
}

pub fn register_terrain_space(app: &mut App) {
    app.add_startup_system(setup_system);
    app.add_system(load_all_terrain);

    app.add_system(load_terrain);

    // We check to clean up chunks once a second.
    // We don't run this after `load_all_terrain` because that terrain doesn't dynamically unload.
    app.add_system(
        clean_up_chunks
            .after(load_terrain)
            .with_run_criteria(FixedTimestep::step(1.0)),
    );
}
