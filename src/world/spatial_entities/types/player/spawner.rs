use anyhow::Result;
use bevy::{ecs::system::EntityCommands, prelude::*};

use proc_macros::entity_serialization;

use crate::world::{
    physics::Position,
    spatial_entities::storage::{EntityConstruction, EntitySerializationManager, EntityStorage},
};

#[derive(Component)]
pub struct PlayerSpawner;

#[entity_serialization(type_id = 0, marker = PlayerSpawner, bootstrap = BootstrapEntityInfo::PlayerSpawner)]
#[derive(serde::Deserialize)]
pub struct PlayerSpawnerStorage {
    position: Position,
}

impl PlayerSpawner {
    pub fn spawn(
        parent: Entity,
        commands: &mut Commands,
        storage: &EntityStorage,
        position: Position,
    ) -> Result<()> {
        let storable = storage.new_storable_component::<PlayerSpawner>()?;

        let mut commands = commands.spawn(storable);
        commands.insert(ChildOf(parent));

        Self::construct_entity(PlayerSpawnerStorage { position }, &mut commands);

        Ok(())
    }
}

impl EntityConstruction<PlayerSpawnerStorage> for PlayerSpawner {
    fn construct_entity(parameters: PlayerSpawnerStorage, commands: &mut EntityCommands) {
        commands.insert((
            Self,
            parameters.position,
            Transform::default(),
            GlobalTransform::default(),
        ));
    }
}

pub fn setup(app: &mut App) {
    EntitySerializationManager::register::<PlayerSpawner>(app);
}
