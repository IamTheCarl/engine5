use anyhow::{Context, Result};
use std::path::Path;

use bevy::prelude::*;
use persistant_entities::{
    delete_persistant_entities, load_persistant_entities, save_persistant_entities,
    PersistantEntityTracker,
};

mod persistant_entities;
mod structures;

/// Wrapper for the database used to store the map.
#[derive(Resource)]
pub struct MapStorage {
    /// Metadata, such as "which persistant entities should be loaded when bootstrapping the world".
    meta: sled::Tree,

    /// Storage for persistant entities.
    persistant_entities: sled::Tree,

    /// Stores which entities are stored in which chunks.
    structure_list: sled::Tree,
}

impl MapStorage {
    pub fn open(path: impl AsRef<Path>) -> Result<Self> {
        let database = sled::open(path).context("Failed to open database")?;

        let meta = database
            .open_tree("metadata")
            .context("Failed to open metadata tree")?;
        let persistant_entities = database
            .open_tree("persistant_entities")
            .context("Failed to open persistant entity tree")?;
        let structure_list = database
            .open_tree("persistant_entities")
            .context("Failed to open structure list tree")?;

        Ok(Self {
            meta,
            persistant_entities,
            structure_list,
        })
    }
}

pub use persistant_entities::{
    DeletePersistantEntities, GetPersistant, LoadResult, PersistantId, RequestLoad,
};

/// Plugin for handling storage of maps.
#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct StoragePlugin;

impl Plugin for StoragePlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                PersistantEntityTracker::insert.before(load_persistant_entities),
                PersistantEntityTracker::remove.after(delete_persistant_entities),
            ),
        );
        app.add_systems(
            Update,
            (
                save_persistant_entities,
                load_persistant_entities,
                delete_persistant_entities,
            )
                .run_if(resource_exists::<MapStorage>),
        );
        app.add_event::<RequestLoad>();
        app.add_event::<LoadResult>();
        app.register_type::<Transform>();
        app.insert_resource(PersistantEntityTracker::default());
    }
}
