use anyhow::Result;
use bevy::{ecs::system::EntityCommands, prelude::*};
use proc_macros::entity_serialization;

use crate::world::{
    generation::WorldGeneratorEnum,
    physics::{Position, Velocity},
    spatial_entities::storage::{EntityConstruction, EntitySerializationManager, EntityStorage},
    terrain::{
        terrain_space::SpaceModificationRequestList, TerrainSpace, TerrainSpaceBundle,
        TerrainStorage,
    },
};

#[entity_serialization(type_id = 1, marker = GlobalTerrainEntity, bootstrap = BootstrapEntityInfo::GlobalTerrain)]
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
            storage.new_storable_component_with_tree::<GlobalTerrainEntity, _, _>()?;

        let mut commands = commands.spawn(storable);
        commands.set_parent(parent);
        Self::common_construction(
            GlobalTerrainParameters {
                generator: generator.into(),
            },
            TerrainStorage::Local { tree },
            &mut commands,
        );

        Ok(())
    }

    fn common_construction(
        parameters: GlobalTerrainParameters,
        storage: TerrainStorage,
        commands: &mut EntityCommands,
    ) {
        commands.insert((
            Self,
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
        ));
    }
}

impl EntityConstruction<GlobalTerrainParameters> for GlobalTerrainEntity {
    const REQUEST_TREE: bool = true;

    fn construct_entity(parameters: GlobalTerrainParameters, commands: &mut EntityCommands) {
        Self::common_construction(parameters, TerrainStorage::None, commands);
    }

    fn construct_entity_with_tree(
        parameters: GlobalTerrainParameters,
        commands: &mut EntityCommands,
        tree: sled::Tree,
    ) {
        Self::common_construction(parameters, TerrainStorage::Local { tree }, commands);
    }
}

pub fn setup(app: &mut App) {
    EntitySerializationManager::register::<GlobalTerrainEntity, _, _>(app);
}
