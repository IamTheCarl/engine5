use anyhow::Result;
use bevy::prelude::*;
use serde::{Deserialize, Serialize};

use super::{
    generation::WorldGeneratorEnum,
    physics::{Position, Velocity},
    spatial_entities::storage::{
        BootstrapEntityInfo, DataLoader, DataSaver, EntitySerializationManager, EntityStorage,
        EntityTypeId, SpatialEntity, Storable, ToSaveSpatial,
    },
    terrain::{
        terrain_space::SpaceModificationRequestList, TerrainSpace, TerrainSpaceBundle,
        TerrainStorage,
    },
};

#[derive(Deserialize)]
pub struct GlobalTerrainParameters {
    generator: WorldGeneratorEnum,
}

#[derive(Component)]
pub struct GlobalTerrainEntity;

impl SpatialEntity<(Entity, &Storable, &TerrainSpace)> for GlobalTerrainEntity {
    const TYPE_ID: EntityTypeId = 1;
    const BOOTSTRAP: BootstrapEntityInfo = BootstrapEntityInfo::GlobalTerrain;

    fn load(data_loader: DataLoader, parent: Entity, commands: &mut Commands) -> Result<()> {
        let (storable, parameters, tree) =
            data_loader.load_with_tree::<GlobalTerrainParameters>()?;
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
        (entity, storable, terrain_space): (Entity, &Storable, &TerrainSpace),
        data_saver: DataSaver,
    ) {
        #[derive(Serialize)]
        pub struct GlobalTerrainSave<'a> {
            generator: &'a WorldGeneratorEnum,
        }

        data_saver.save(
            entity,
            storable,
            &GlobalTerrainSave {
                generator: &terrain_space.generator,
            },
        );
    }
}

impl GlobalTerrainEntity {
    pub fn create_new(
        parent: Entity,
        commands: &mut Commands,
        storage: &EntityStorage,
        generator: impl Into<WorldGeneratorEnum>,
    ) -> Result<()> {
        let (storable, tree) =
            storage.new_storable_component_with_tree::<GlobalTerrainEntity, _>()?;

        Self::spawn_internal(
            parent,
            commands,
            storable,
            TerrainStorage::Local { tree },
            GlobalTerrainParameters {
                generator: generator.into(),
            },
        );

        Ok(())
    }

    fn spawn_internal(
        parent: Entity,
        commands: &mut Commands,
        storable: Storable,
        storage: TerrainStorage,
        parameters: GlobalTerrainParameters,
    ) {
        commands
            .spawn((
                Self,
                ToSaveSpatial,
                storable,
                TerrainSpaceBundle {
                    terrain_space: TerrainSpace::global(parameters.generator),
                    position: Position {
                        translation: Vec3::ZERO,
                        rotation: 0.0,
                    },
                    storage,
                    modification_request_list: SpaceModificationRequestList::default(),
                    transform: Transform::default(),
                    global_transform: GlobalTransform::default(),
                    visibility: Visibility::Inherited,
                    computed_visibility: ComputedVisibility::default(),
                },
                Velocity {
                    translation: Vec3::ZERO,
                    rotational: 0.0,
                },
            ))
            .set_parent(parent);
    }
}

pub fn setup(app: &mut App) {
    EntitySerializationManager::register::<GlobalTerrainEntity, _>(app);
}
