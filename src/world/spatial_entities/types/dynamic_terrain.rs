use anyhow::Result;
use bevy::prelude::*;
use proc_macros::entity_serialization;

use crate::world::{
    generation::WorldGeneratorEnum,
    physics::{Position, Velocity},
    spatial_entities::storage::{
        EntitySerializationManager, EntityStorage, Storable, ToSaveSpatial,
    },
    terrain::{
        terrain_space::SpaceModificationRequestList, TerrainSpace, TerrainSpaceBundle,
        TerrainStorage,
    },
};

#[entity_serialization(type_id = 2, marker = DynamicTerrainEntity, load_tree)]
pub struct DynamicTerrainParameters {
    #[query = TerrainSpace]
    #[get = generator]
    generator: WorldGeneratorEnum,
    position: Position,
    velocity: Velocity,
}

#[derive(Component)]
pub struct DynamicTerrainEntity;

impl DynamicTerrainEntity {
    pub fn spawn(
        parent: Entity,
        commands: &mut Commands,
        storage: &EntityStorage,
        generator: impl Into<WorldGeneratorEnum>,
        position: Position,
        velocity: Velocity,
    ) -> Result<()> {
        let (storable, tree) =
            storage.new_storable_component_with_tree::<DynamicTerrainEntity, _>()?;

        Self::spawn_internal(
            parent,
            commands,
            storable,
            TerrainStorage::Local { tree },
            DynamicTerrainParameters {
                generator: generator.into(),
                position,
                velocity,
            },
        );
        Ok(())
    }

    fn spawn_internal(
        parent: Entity,
        commands: &mut Commands,
        storable: Storable,
        storage: TerrainStorage,
        parameters: DynamicTerrainParameters,
    ) {
        commands
            .spawn((
                Self,
                ToSaveSpatial,
                storable,
                TerrainSpaceBundle {
                    terrain_space: TerrainSpace::local(parameters.generator),
                    position: parameters.position,
                    storage,
                    modification_request_list: SpaceModificationRequestList::default(),
                    transform: Transform::default(),
                    global_transform: GlobalTransform::default(),
                    visibility: Visibility::Inherited,
                    computed_visibility: ComputedVisibility::default(),
                },
                parameters.velocity,
            ))
            .set_parent(parent);
    }
}

pub fn setup(app: &mut App) {
    EntitySerializationManager::register::<DynamicTerrainEntity, _>(app);
}
