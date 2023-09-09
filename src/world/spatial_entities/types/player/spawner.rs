use anyhow::Result;
use bevy::prelude::*;

use proc_macros::entity_serialization;

use crate::world::{
    physics::Position,
    spatial_entities::storage::{
        EntitySerializationManager, EntityStorage, Storable, ToSaveSpatial,
    },
};

#[derive(Component)]
pub struct PlayerSpawner;

#[entity_serialization(type_id = 0, marker = PlayerSpawner, bootstrap = BootstrapEntityInfo::PlayerSpawner)]
struct PlayerSpawnerStorage {
    position: Position,
}

impl PlayerSpawner {
    pub fn spawn(
        parent: Entity,
        commands: &mut Commands,
        storage: &EntityStorage,
        position: Position,
    ) -> Result<()> {
        let storable = storage.new_storable_component::<PlayerSpawner, _>()?;

        Self::spawn_internal(
            parent,
            commands,
            storable,
            PlayerSpawnerStorage { position },
        );

        Ok(())
    }

    fn spawn_internal(
        parent: Entity,
        commands: &mut Commands,
        storable: Storable,
        parameters: PlayerSpawnerStorage,
    ) {
        commands
            .spawn((
                Self,
                parameters.position,
                Transform::default(),
                GlobalTransform::default(),
                storable,
                ToSaveSpatial, // We want to save this as soon as its spawned.
            ))
            .set_parent(parent);
    }
}

pub fn setup(app: &mut App) {
    EntitySerializationManager::register::<PlayerSpawner, _>(app);
}
