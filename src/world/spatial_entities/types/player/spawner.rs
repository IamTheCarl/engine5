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
        let storable = storage.new_storable_component::<PlayerSpawner, _, _, _>()?;

        let mut commands = commands.spawn(storable);
        commands.set_parent(parent);

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
    EntitySerializationManager::register::<PlayerSpawner, _, _, _>(app);
}
