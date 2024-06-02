use anyhow::Result;
use avian3d::prelude::*;
use bevy::{ecs::system::EntityCommands, prelude::*};
use proc_macros::entity_serialization;

use crate::world::{
    generation::WorldGeneratorEnum,
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
            storage.new_storable_component_with_tree::<GlobalTerrainEntity, _, _, _>()?;

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
                storage,
                modification_request_list: SpaceModificationRequestList::default(),
                visibility: Visibility::Inherited,
                inherited_visibility: InheritedVisibility::default(),
                view_visibility: ViewVisibility::default(),
                transform: TransformBundle::default(),
                rigid_body: RigidBody::Static,
                gravity_scale: GravityScale(0.0),
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
    EntitySerializationManager::register::<GlobalTerrainEntity, _, _, _>(app);
}
