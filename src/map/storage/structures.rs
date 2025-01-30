use std::collections::{HashMap, HashSet};

use bevy::prelude::*;

use super::{persistant_entities::PersistantEntityTracker, MapStorage, PersistantId};
use crate::map::terrain::{ChunkIndex, Structure};

#[derive(Debug, Resource)]
pub struct StructureTracker {
    structures: HashMap<Entity, HashSet<ChunkIndex>>,
}

pub fn add_structures(
    new_structures: Query<&Structure, Added<Structure>>,
    mut tracker: ResMut<StructureTracker>,
) {
    for structure in new_structures.iter() {
        let chunks: HashSet<_> = structure.aabb.intersected_chunks().collect();
    }
}

pub fn update_structures(
    new_structures: Query<&Structure, Changed<Structure>>,

    mut tracker: ResMut<StructureTracker>,
) {
}

pub fn remove_structures(
    mut removals: RemovedComponents<Structure>,
    mut structure_tracker: ResMut<StructureTracker>,
    storage: Res<MapStorage>,
) {
    let tst = &storage.structure_list;
    for removed in removals.read() {
        let chunks = structure_tracker
            .structures
            .remove(&removed)
            .expect("Removed a structure that was never tracked");

        let keys = chunks.iter().map(|index| index.gen_key());
        for key in keys {
            tst.fetch_and_update(key, |original| -> Option<Vec<u8>> {
                if let Some(original) = original {
                    todo!()
                } else {
                    // Nothing to be removed I guess.
                    None
                }
            });
            todo!()
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
        // We are going to truncate all of these down to 2 bytes.
        let x = self.x.to_le_bytes();
        let y = self.y.to_le_bytes();
        let z = self.z.to_le_bytes();

        [x[0], x[1], y[0], y[1], z[0], z[1]]
    }
}

#[cfg(test)]
mod test {
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
}
