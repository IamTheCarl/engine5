use bevy::prelude::*;
use persistant_entities::{
    delete_persistant_entities, load_persistant_entities, save_persistant_entities,
    PersistantEntityTracker,
};

mod persistant_entities;

pub use persistant_entities::{
    DeletePersistantEntities, GetPersistant, LoadResult, MapStorage, RequestLoad,
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
