use anyhow::Result;
use bevy::prelude::*;
use serde::{Deserialize, Serialize};

use super::{
    generation::WorldGeneratorEnum,
    physics::{Position, Velocity},
    spatial_entities::storage::{
        DataLoader, DataSaver, EntitySerializationManager, EntityStorage, EntityTypeId,
        SpatialEntity, Storable, ToSaveSpatial,
    },
    terrain::{
        terrain_space::ModificationRequestList, TerrainSpace, TerrainSpaceBundle, TerrainStorage,
    },
};

#[derive(Deserialize)]
pub struct DynamicTerrainParameters {
    generator: WorldGeneratorEnum,
    position: Position,
    velocity: Velocity,
}

#[derive(Component)]
pub struct DynamicTerrainEntity;

impl SpatialEntity<(Entity, &Storable, &Position, &Velocity, &TerrainSpace)>
    for DynamicTerrainEntity
{
    const TYPE_ID: EntityTypeId = 2;

    fn load(data_loader: DataLoader, parent: Entity, commands: &mut Commands) -> Result<()> {
        let (storable, parameters, tree) =
            data_loader.load_with_tree::<DynamicTerrainParameters>()?;
        Self::spawn_internal(
            parent,
            commands,
            storable,
            TerrainStorage::Local { tree },
            parameters,
        );
        Ok(())
    }

    fn save(
        (entity, storable, position, velocity, terrain_space): (
            Entity,
            &Storable,
            &Position,
            &Velocity,
            &TerrainSpace,
        ),
        data_saver: DataSaver,
    ) {
        #[derive(Serialize)]
        pub struct DynamicTerrainSave<'a> {
            generator: &'a WorldGeneratorEnum,
            position: &'a Position,
            velocity: &'a Velocity,
        }

        data_saver.save(
            entity,
            storable,
            &DynamicTerrainSave {
                generator: &terrain_space.generator,
                position,
                velocity,
            },
        );
    }
}

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
                    modification_request_list: ModificationRequestList::default(),
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
