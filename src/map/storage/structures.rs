use std::collections::{HashMap, HashSet};

use bevy::prelude::*;
use sled::IVec;

use super::{persistant_entities::Persistant, MapStorage, PersistantId};
use crate::map::terrain::{ChunkIndex, Structure};

#[derive(Debug, Default, Resource)]
pub struct StructureTracker {
    structures: HashMap<Entity, (PersistantId, HashSet<ChunkIndex>)>,
}

pub fn add_structures(
    new_structures: Query<(Entity, &Persistant), Added<Structure>>,
    mut tracker: ResMut<StructureTracker>,
) {
    for (entity, persistant) in new_structures.iter() {
        // The changed structures method will do most of the work to add this thing.
        // We just need to make sure it's being tracked before then.
        tracker
            .structures
            .insert(entity, (persistant.0, HashSet::new()));
    }
}

#[allow(clippy::type_complexity)]
pub fn update_structures(
    changed_structures: Query<(Entity, &Structure), (Changed<Structure>, With<Persistant>)>,
    mut tracker: ResMut<StructureTracker>,
    storage: Res<MapStorage>,
) {
    // Try and reuse these across entities, to avoid reallocations.
    let mut old_chunks = HashSet::new();
    let mut new_chunks = HashSet::new();

    for (changed_entity, structure) in changed_structures.iter() {
        let (persistant_id, chunks) = tracker
            .structures
            .get_mut(&changed_entity)
            .expect("Changed structure that was never tracked");

        // Collect lists of the old chunks and the new chunks.
        let keys = chunks.iter().map(|index| index.gen_key());
        old_chunks.clear();
        old_chunks.extend(keys);

        // We need to update the tracker to the new list of chunks.
        chunks.clear();
        chunks.extend(structure.aabb.intersected_chunks());

        new_chunks.clear();
        new_chunks.extend(chunks.iter().map(|index| index.gen_key()));

        storage
            .structure_list
            .transaction(
                |tst| -> Result<(), sled::transaction::ConflictableTransactionError<()>> {
                    let to_remove = old_chunks.difference(&new_chunks);
                    let to_add = new_chunks.difference(&old_chunks);

                    // We start by removing this structure from all chunks it used to be in.
                    for key in to_remove {
                        let filtered_list: Option<IVec> = tst.get(key)?.map(|original| {
                            encode_id_list(
                                decode_id_list(original.as_ref())
                                    .filter(|id| *id != *persistant_id),
                            )
                            .into()
                        });

                        if let Some(filtered_list) = filtered_list {
                            if filtered_list.is_empty() {
                                tst.remove(key)?;
                            } else {
                                tst.insert(key, filtered_list)?;
                            }
                        } else {
                            tst.remove(key)?;
                        }
                    }

                    // Now we add the key to all chunks it isn't in yet.
                    for key in to_add {
                        // The entry shouldn't be in there already. If it is, that's not really a
                        // problem since persistent entity loading is deduplicated.

                        let original = tst.get(key)?;
                        let added_id = [*persistant_id];

                        let new_list = if let Some(original) = original {
                            // Add the item to the current list.
                            encode_id_list(decode_id_list(original.as_ref()).chain(added_id))
                        } else {
                            // No prior content, so just make a list from this one item.
                            encode_id_list(added_id)
                        };

                        tst.insert(key, new_list)?;
                    }

                    Ok(())
                },
            )
            .expect("Could not build transaction");
    }
}

pub fn remove_structures(
    mut removals: RemovedComponents<Structure>,
    mut tracker: ResMut<StructureTracker>,
    storage: Res<MapStorage>,
) {
    let tst = &storage.structure_list;
    for removed_entity in removals.read() {
        let (removed_persistant_id, chunks) = tracker
            .structures
            .remove(&removed_entity)
            .expect("Removed a structure that was never tracked");

        let keys = chunks.iter().map(|index| index.gen_key());
        for key in keys {
            if let Err(error) = tst.fetch_and_update(key, |original| -> Option<IVec> {
                // We only need to fiter out the ID if there was a list to begin with.
                original
                    .map(|original| {
                        encode_id_list(
                            decode_id_list(original).filter(|id| *id != removed_persistant_id),
                        )
                        .into()
                    })
                    .filter(|buffer: &IVec| !buffer.is_empty())
            }) {
                error!("Failed to update structure list: {error}");
            }
        }
    }
}

/// Decodes a binary list of persistant IDs.
fn decode_id_list(list: &[u8]) -> impl Iterator<Item = PersistantId> + '_ {
    list.chunks_exact(size_of::<PersistantId>())
        .filter_map(|chunk| {
            let mut id = [0u8; size_of::<PersistantId>()];
            id.copy_from_slice(chunk);
            let id = u32::from_le_bytes(id);

            PersistantId::new(id)
        })
}

/// Encodes a list of persistant IDs to binary.
fn encode_id_list(list: impl IntoIterator<Item = PersistantId>) -> Vec<u8> {
    list.into_iter()
        .flat_map(|id| id.get().to_le_bytes())
        .collect()
}

trait GenKey {
    fn gen_key(&self) -> [u8; 6];
}

impl GenKey for ChunkIndex {
    fn gen_key(&self) -> [u8; 6] {
        // TODO let's extend the key to 8 bytes and use the extra 2 as a "terrain space ID".

        // We are going to truncate all of these down to 2 bytes.
        let x = self.x.to_le_bytes();
        let y = self.y.to_le_bytes();
        let z = self.z.to_le_bytes();

        [x[0], x[1], y[0], y[1], z[0], z[1]]
    }
}

#[cfg(test)]
mod test {
    use outdir_tempdir::TempDir;

    use crate::map::{
        storage::persistant_entities::ToSave,
        terrain::{TerrainAABB, VoxelIndex},
        MapPlugin,
    };

    use super::*;

    #[test]
    fn generate_keys_for_chunk_index() {
        assert_eq!(ChunkIndex::new(0, 0, 0).gen_key(), [0, 0, 0, 0, 0, 0]);
        assert_eq!(
            ChunkIndex::new(0x44223311, 0x55667788, 0x11AABBCC).gen_key(),
            [0x11, 0x33, 0x88, 0x77, 0xCC, 0xBB]
        );
    }

    #[test]
    fn persistant_id_list_serialization() {
        assert_eq!(
            decode_id_list(&encode_id_list([])).collect::<Vec<_>>(),
            vec![]
        );

        assert_eq!(
            decode_id_list(&encode_id_list([PersistantId::new(1).unwrap()])).collect::<Vec<_>>(),
            vec![PersistantId::new(1).unwrap()]
        );

        assert_eq!(
            decode_id_list(&encode_id_list([
                PersistantId::new(1).unwrap(),
                PersistantId::new(2).unwrap()
            ]))
            .collect::<Vec<_>>(),
            vec![PersistantId::new(1).unwrap(), PersistantId::new(2).unwrap()]
        );

        // This cuts the list short and should therefor truncate the last ID.
        assert_eq!(
            decode_id_list(
                &encode_id_list([PersistantId::new(1).unwrap(), PersistantId::new(2).unwrap()])
                    [0..7]
            )
            .collect::<Vec<_>>(),
            vec![PersistantId::new(1).unwrap()]
        );

        // Values of 0 are not possible, so they will be omitted.
        assert_eq!(
            decode_id_list(&[0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00]).collect::<Vec<_>>(),
            vec![PersistantId::new(1).unwrap()]
        );
    }

    #[test]
    fn structure_tracking() {
        let mut app = App::new();
        app.add_plugins((MapPlugin,));

        let tempdir = TempDir::new();

        let persistant_id = PersistantId::new(1).unwrap();

        let entity_id = {
            let world = app.world_mut();
            world.insert_resource(
                MapStorage::open(tempdir.path().join("structure_tracking")).unwrap(),
            );

            world
                .spawn((
                    Persistant(persistant_id),
                    ToSave,
                    Structure {
                        aabb: TerrainAABB {
                            a: VoxelIndex::new(-1, -1, -1),
                            b: VoxelIndex::new(1, 1, 1),
                        },
                    },
                ))
                .id()
        };

        app.update();

        assert_eq!(
            app.world().resource::<StructureTracker>().structures,
            HashMap::from([(
                entity_id,
                (
                    persistant_id,
                    HashSet::from([
                        ChunkIndex::new(-1, -1, -1),
                        ChunkIndex::new(-1, -1, 0),
                        ChunkIndex::new(-1, 0, -1),
                        ChunkIndex::new(-1, 0, 0),
                        ChunkIndex::new(0, -1, -1),
                        ChunkIndex::new(0, -1, 0),
                        ChunkIndex::new(0, 0, -1),
                        ChunkIndex::new(0, 0, 0),
                    ])
                )
            )])
        );

        {
            let world = app.world_mut();
            world.despawn(entity_id);
        }

        app.update();

        assert_eq!(
            app.world().resource::<StructureTracker>().structures,
            HashMap::from([])
        );
    }

    #[test]
    fn structure_storage() {
        let mut app = App::new();
        app.add_plugins((MapPlugin,));

        let tempdir = TempDir::new();

        let persistant_id = PersistantId::new(1).unwrap();

        let entity_id = {
            let world = app.world_mut();
            world.insert_resource(
                MapStorage::open(tempdir.path().join("structure_storage")).unwrap(),
            );

            world
                .spawn((
                    Persistant(persistant_id),
                    ToSave,
                    Structure {
                        aabb: TerrainAABB {
                            a: VoxelIndex::new(-1, -1, -1),
                            b: VoxelIndex::new(1, 1, 1),
                        },
                    },
                ))
                .id()
        };

        app.update();

        {
            let storage = &app.world().resource::<MapStorage>().structure_list;
            assert_eq!(
                storage.get(ChunkIndex::new(-1, -1, -1).gen_key()),
                Ok(Some(persistant_id.get().to_le_bytes().to_vec().into()))
            );
            assert_eq!(
                storage.get(ChunkIndex::new(-1, -1, 0).gen_key()),
                Ok(Some(persistant_id.get().to_le_bytes().to_vec().into()))
            );
            assert_eq!(
                storage.get(ChunkIndex::new(-1, 0, -1).gen_key()),
                Ok(Some(persistant_id.get().to_le_bytes().to_vec().into()))
            );
            assert_eq!(
                storage.get(ChunkIndex::new(-1, 0, 0).gen_key()),
                Ok(Some(persistant_id.get().to_le_bytes().to_vec().into()))
            );
            assert_eq!(
                storage.get(ChunkIndex::new(0, -1, -1).gen_key()),
                Ok(Some(persistant_id.get().to_le_bytes().to_vec().into()))
            );
            assert_eq!(
                storage.get(ChunkIndex::new(0, -1, 0).gen_key()),
                Ok(Some(persistant_id.get().to_le_bytes().to_vec().into()))
            );
            assert_eq!(
                storage.get(ChunkIndex::new(0, 0, -1).gen_key()),
                Ok(Some(persistant_id.get().to_le_bytes().to_vec().into()))
            );
            assert_eq!(
                storage.get(ChunkIndex::new(0, 0, 0).gen_key()),
                Ok(Some(persistant_id.get().to_le_bytes().to_vec().into()))
            );
        }

        {
            let world = app.world_mut();
            world.despawn(entity_id);
        }

        app.update();

        {
            let storage = &app.world().resource::<MapStorage>().structure_list;
            assert_eq!(storage.get(ChunkIndex::new(-1, -1, -1).gen_key()), Ok(None));
            assert_eq!(storage.get(ChunkIndex::new(-1, -1, 0).gen_key()), Ok(None));
            assert_eq!(storage.get(ChunkIndex::new(-1, 0, -1).gen_key()), Ok(None));
            assert_eq!(storage.get(ChunkIndex::new(-1, 0, 0).gen_key()), Ok(None));
            assert_eq!(storage.get(ChunkIndex::new(0, -1, -1).gen_key()), Ok(None));
            assert_eq!(storage.get(ChunkIndex::new(0, -1, 0).gen_key()), Ok(None));
            assert_eq!(storage.get(ChunkIndex::new(0, 0, -1).gen_key()), Ok(None));
            assert_eq!(storage.get(ChunkIndex::new(0, 0, 0).gen_key()), Ok(None));
        }
    }
}
