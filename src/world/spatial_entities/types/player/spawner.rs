use anyhow::Result;
use bevy::prelude::*;
use serde::{Deserialize, Serialize};

use crate::world::{
    physics::Position,
    spatial_entities::storage::{
        BootstrapEntityInfo, DataLoader, DataSaver, EntityStorage, EntityTypeId, SpatialEntity,
        Storable, ToSaveSpatial,
    },
};

#[derive(Component)]
pub struct PlayerSpawner;

#[derive(Deserialize)]
struct PlayerSpawnerParameters {
    position: Position,
}

impl SpatialEntity<(Entity, &Storable, &Position)> for PlayerSpawner {
    const TYPE_ID: EntityTypeId = 0;
    const BOOTSTRAP: BootstrapEntityInfo = BootstrapEntityInfo::PlayerSpawner;

    fn load(data_loader: DataLoader, parent: Entity, commands: &mut Commands) -> Result<()> {
        let (storable, parameters) = data_loader.load::<PlayerSpawnerParameters>()?;
        Self::spawn_internal(parent, commands, storable, parameters);
        Ok(())
    }

    fn save((entity, storable, position): (Entity, &Storable, &Position), data_saver: DataSaver) {
        #[derive(Serialize)]
        struct PlayerSpawnSerialization<'a> {
            position: &'a Position,
        }

        data_saver.save(entity, storable, &PlayerSpawnSerialization { position });
    }
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
            PlayerSpawnerParameters { position },
        );

        Ok(())
    }

    fn spawn_internal(
        parent: Entity,
        commands: &mut Commands,
        storable: Storable,
        parameters: PlayerSpawnerParameters,
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
