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

#[entity_serialization(type_id = 1, marker = GlobalTerrainEntity, bootstrap = BootstrapEntityInfo::GlobalTerrain, load_tree)]
pub struct GlobalTerrainParameters {
    #[query = TerrainSpace]
    #[get = generator]
    generator: WorldGeneratorEnum,
}

#[derive(Component)]
pub struct GlobalTerrainEntity;

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
