use anyhow::Result;
use avian3d::prelude::*;
use bevy::{ecs::system::EntityCommands, prelude::*};
use proc_macros::entity_serialization;

use crate::world::{
    generation::WorldGeneratorEnum,
    spatial_entities::storage::{
        EntityConstruction, EntitySerializationManager, EntityStorage, Storable, ToSaveSpatial,
    },
    terrain::{TerrainSpace, TerrainSpaceBundle, TerrainStorage},
};

#[entity_serialization(type_id = 2, marker = DynamicTerrainEntity)]
pub struct DynamicTerrainParameters {
    #[query = TerrainSpace]
    #[get = generator]
    generator: WorldGeneratorEnum,
    transform: Transform,
    linear_velocity: LinearVelocity,
    angular_velocity: AngularVelocity,
}

#[derive(Component)]
pub struct DynamicTerrainEntity;

impl DynamicTerrainEntity {
    pub fn spawn(
        parent: Entity,
        commands: &mut Commands,
        storage: &EntityStorage,
        generator: impl Into<WorldGeneratorEnum>,
        transform: Transform,
        linear_velocity: LinearVelocity,
        angular_velocity: AngularVelocity,
    ) -> Result<()> {
        let (storable, tree) =
            storage.new_storable_component_with_tree::<DynamicTerrainEntity, _, _, _>()?;

        Self::spawn_internal(
            parent,
            commands,
            storable,
            TerrainStorage::Local { tree },
            DynamicTerrainParameters {
                generator: generator.into(),
                transform,
                linear_velocity,
                angular_velocity,
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
                    storage,
                    transform: TransformBundle::from_transform(parameters.transform),
                    rigid_body: RigidBody::Dynamic,
                    gravity_scale: GravityScale(0.0),
                    ..Default::default()
                },
                parameters.linear_velocity,
                parameters.angular_velocity,
            ))
            .set_parent(parent);
    }

    fn common_construction(
        parameters: DynamicTerrainParameters,
        storage: TerrainStorage,
        commands: &mut EntityCommands,
    ) {
        commands.insert((
            Self,
            TerrainSpaceBundle {
                terrain_space: TerrainSpace::local(parameters.generator),
                transform: TransformBundle::from_transform(parameters.transform),
                storage,
                ..Default::default()
            },
            parameters.linear_velocity,
            parameters.angular_velocity,
        ));
    }
}

impl EntityConstruction<DynamicTerrainParameters> for DynamicTerrainEntity {
    const REQUEST_TREE: bool = true;

    fn construct_entity(parameters: DynamicTerrainParameters, commands: &mut EntityCommands) {
        Self::common_construction(parameters, TerrainStorage::None, commands);
    }

    fn construct_entity_with_tree(
        parameters: DynamicTerrainParameters,
        commands: &mut EntityCommands,
        tree: sled::Tree,
    ) {
        Self::common_construction(parameters, TerrainStorage::Local { tree }, commands);
    }
}

pub fn setup(app: &mut App) {
    EntitySerializationManager::register::<DynamicTerrainEntity, _, _, _>(app);
}
