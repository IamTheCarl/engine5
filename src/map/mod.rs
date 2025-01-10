use bevy::prelude::*;

mod storage;
mod terrain;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct MapPlugin;

impl Plugin for MapPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins((terrain::TerrainPlugin, storage::StoragePlugin));
    }
}
