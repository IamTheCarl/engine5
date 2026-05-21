use anyhow::{Context, Result};
use bevy::{app::AppExit, prelude::*};

use crate::world::spatial_entities::SpatialHashOffset;

use super::{
    super::generation::ToGenerateTerrain, Chunk, ChunkPosition, TerrainSpace, TerrainTime,
};

pub const CHUNK_TIME_TO_SAVE: usize = 60 * 5;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToLoadTerrain;

#[derive(Component)]
#[component(storage = "SparseSet")]
pub struct ToSaveTerrain;

#[derive(Component)]
pub enum TerrainStorage {
    None,
    Local { tree: sled::Tree },
}

impl Default for TerrainStorage {
    fn default() -> Self {
        Self::None
    }
}

impl TerrainStorage {
    pub fn open_local(database: &sled::Db, namespace: impl Into<String>) -> Result<Self> {
        let tree = database
            .open_tree(namespace.into())
            .context("Could not find or create namespace for terrain within database.")?;

        Ok(Self::Local { tree })
    }

    pub fn read_chunk(&self, position: &ChunkPosition) -> Result<Option<Option<Chunk>>> {
        match self {
            TerrainStorage::None => Ok(None),
            TerrainStorage::Local { tree } => {
                let key = position.to_database_key();

                let value = tree
                    .get(key)
                    .context("Failed to read chunk from database.")?;

                if let Some(value) = value {
                    if value.is_empty() {
                        // An empty array indicates an empty chunk.
                        Ok(Some(None))
                    } else {
                        Ok(Some(Some(Chunk::deserialize(&value))))
                    }
                } else {
                    Ok(None)
                }
            }
        }
    }

    pub fn write_chunk(&self, chunk: Option<&Chunk>, position: &ChunkPosition) -> Result<()> {
        match self {
            TerrainStorage::None => Ok(()), // We don't save it so we just do nothing.
            TerrainStorage::Local { tree } => {
                let key = position.to_database_key();

                if let Some(chunk) = chunk {
                    let mut value = Vec::new();
                    chunk.serialize(&mut value);

                    tree.insert(key, value)
                        .context("Failed to insert terrain chunk into database.")?;
                } else {
                    // An empty array indicates this chunk is empty.
                    tree.insert(key, &[])
                        .context("Failed to insert empty terrain chunk into database.")?;
                }

                tree.flush()
                    .context("Failed to flush terrain chunk to database.")?;
                Ok(())
            }
        }
    }

    pub fn iter_chunk_indexes(&self) -> impl Iterator<Item = ChunkPosition> {
        // TODO implement.
        std::iter::empty()
    }
}

type ToLoadQuery<'a, 'b, 'c, 'd> =
    Query<'a, 'b, (Entity, &'c ChildOf, &'d ChunkPosition), (With<ToLoadTerrain>, Without<Chunk>)>;

/// Load terrain from a file.
fn load_terrain(
    mut commands: Commands,
    mut space: Query<(&TerrainStorage, &mut TerrainSpace)>,
    mut to_load: ToLoadQuery,
) -> () {
    for (entity, parent, position) in to_load.iter_mut() {
        // This chunk will no longer need to be loaded when we're done, so remove the marker.
        let mut entity = commands.entity(entity);
        entity.remove::<ToLoadTerrain>();

        if let Ok((storage, mut terrain_space)) = space.get_mut(parent.0) {
            if let Some(storage_content) = storage.read_chunk(position).unwrap_or(None) {
                if let Some(chunk) = storage_content {
                    // Sweet, add that into the world.
                    entity.insert((
                        chunk,
                        position.as_transform(),
                        SpatialHashOffset {
                            translation: Vec3::new(8.0, 0.0, 8.0),
                        },
                    ));

                    terrain_space.non_empty_chunks.insert(entity.id());
                } else {
                    // The storage indicated that this chunk is empty.
                    entity.insert((
                        position.as_transform(),
                        SpatialHashOffset {
                            translation: Vec3::new(8.0, 0.0, 8.0),
                        },
                    ));
                }
            } else {
                // Looks like it wasn't in the storage.
                entity.insert(ToGenerateTerrain);
            }
        } else {
            // Looks like we couldn't get the storage for it. Generate it.
            log::warn!("Failed to get storage for terrain chunk.");
            entity.insert(ToGenerateTerrain);
        }
    }
}

/// Save terrain to a file.
fn save_terrain(
    mut commands: Commands,
    storage: Query<&TerrainStorage>,
    terrain: Query<(Entity, &ChildOf, &ChunkPosition, Option<&Chunk>), With<ToSaveTerrain>>,
) {
    for (entity, this_storage, position, chunk) in terrain.iter() {
        if let Ok(storage) = storage.get(this_storage.0) {
            if let Err(error) = storage.write_chunk(chunk, position) {
                // Crashing while saving would be very bad, so just log the error.
                // We don't remove the "ToSave" tag so that we can try again the next frame (yes, this will spam the log)
                log::error!("Failed to save chunk: {:?}", error);
            } else {
                // We were successful. Remove that save marker so we don't start saving every frame.
                commands.entity(entity).remove::<ToSaveTerrain>();
            }
        }
    }
}

#[derive(Component)]
struct SaveTimer {
    /// How many ticks are we to wait before saving the changes?
    deadline: usize,
}

type SaveTimerStartTerrainQuery<'a, 'b> = Query<
    'a,
    'b,
    (Entity,),
    (
        With<Chunk>,
        Without<ToLoadTerrain>,
        Without<ToGenerateTerrain>,
        Without<ToSaveTerrain>,
        Changed<Chunk>,
    ),
>;

/// We don't want to stress the hard drive too hard so we start a timer with each modification, waiting for that to expire before we
/// save terrain to disk. This hopefully lets us accumulate some changes.
fn save_timer_start(
    mut commands: Commands,
    terrain: SaveTimerStartTerrainQuery,
    terrain_time: Res<TerrainTime>,
) {
    let save_deadline = terrain_time.time + CHUNK_TIME_TO_SAVE;

    for (entity,) in terrain.iter() {
        // This will replace a save deadline timer, thus extending its time to save.
        commands.entity(entity).insert(SaveTimer {
            deadline: save_deadline,
        });
    }
}

/// The timer went off, we need to mark the chunk as needing to be saved.
fn save_timer_trigger(
    mut commands: Commands,
    terrain: Query<(Entity, &SaveTimer), With<Chunk>>,
    terrain_time: Res<TerrainTime>,
) {
    for (entity, save_timer) in terrain.iter() {
        if save_timer.deadline <= terrain_time.time {
            // Okay good, remove that timer so we don't start saving every frame.
            commands
                .entity(entity)
                .remove::<SaveTimer>()
                .insert(ToSaveTerrain);
        }
    }
}

fn save_on_shutdown(
    mut commands: Commands,
    mut events: bevy::ecs::message::MessageReader<AppExit>,
    chunks: Query<Entity, With<Chunk>>,
) {
    if events.read().next().is_some() {
        // Save! Save everything!
        for chunk in chunks.iter() {
            commands.entity(chunk).insert(ToSaveTerrain);
        }
    }
}

pub fn register_terrain_files(app: &mut App) {
    #[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
    struct SaveTimerStartSet;

    #[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
    struct SaveTimerTriggerSet;

    #[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
    struct SaveTerrainSet;

    #[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
    struct SaveOnShutdownSet;

    app.configure_sets(Update, SaveTimerStartSet.after(super::terrain_time_tick));
    app.configure_sets(
        Update,
        SaveTimerTriggerSet
            .after(SaveTimerStartSet)
            .before(SaveTerrainSet),
    );
    app.configure_sets(PostUpdate, SaveOnShutdownSet.before(SaveTerrainSet));

    app.add_systems(
        Update,
        (
            load_terrain,
            save_timer_start.in_set(SaveTimerStartSet),
            save_timer_trigger.in_set(SaveTimerTriggerSet),
        ),
    );

    app.add_systems(
        PostUpdate,
        (
            save_terrain.in_set(SaveTerrainSet),
            save_on_shutdown.in_set(SaveOnShutdownSet),
        ),
    );
}
