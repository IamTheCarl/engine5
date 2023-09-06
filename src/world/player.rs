use anyhow::Result;
use bevy::math::Vec3Swizzles;
use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;

use crate::config::controls::{ButtonState, InputState};
use crate::world::physics::{
    Cylinder, Position, RayCast, RayTerrainIntersection, RayTerrainIntersectionList, Velocity,
};
use crate::world::terrain::terrain_space::SpaceModificationRequest;
use crate::world::terrain::{Block, BlockRegistry, BlockTag, Chunk, LoadsTerrain, TerrainSpace};

use super::physics::PhysicsPlugin;
use super::spatial_entities::storage::{
    BootstrapEntityInfo, DataLoader, DataSaver, EntitySerializationManager, EntityStorage,
    EntityTypeId, SpatialEntity, Storable, ToSaveSpatial,
};
use super::terrain::terrain_space::SpaceModificationRequestList;

const PLAYER_SPEED: f32 = 12.0;

#[derive(Deserialize)]
struct PlayerEntityParameters {
    velocity: Velocity,
    position: Position,
    pitch: f32,
}

#[derive(Component)]
struct LocalPlayer;

#[derive(Component)]
pub struct PlayerEntity {
    pitch: f32,
}

impl SpatialEntity<(Entity, &Storable, &Position, &Velocity, &Self)> for PlayerEntity {
    const TYPE_ID: EntityTypeId = 0;
    const BOOTSTRAP: BootstrapEntityInfo = BootstrapEntityInfo::LocalPlayer;

    fn load(data_loader: DataLoader, parent: Entity, commands: &mut Commands) -> Result<()> {
        let (storable, parameters) = data_loader.load::<PlayerEntityParameters>()?;
        Self::spawn_internal(parent, commands, storable, parameters);
        Ok(())
    }

    fn save(
        (entity, storable, position, velocity, player): (
            Entity,
            &Storable,
            &Position,
            &Velocity,
            &Self,
        ),
        data_saver: DataSaver,
    ) {
        #[derive(Serialize)]
        struct PlayerStorageSerialization<'a> {
            velocity: &'a Velocity,
            position: &'a Position,
            pitch: f32,
        }

        data_saver.save(
            entity,
            storable,
            &PlayerStorageSerialization {
                position,
                velocity,
                pitch: player.pitch,
            },
        );
    }
}

impl PlayerEntity {
    /// Spawns the `Camera3dBundle` to be controlled
    pub fn spawn(
        parent: Entity,
        commands: &mut Commands,
        storage: &EntityStorage,
        position: Position,
        pitch: f32,
    ) -> Result<()> {
        let storable = storage.new_storable_component::<PlayerEntity, _>()?;

        Self::spawn_internal(
            parent,
            commands,
            storable,
            PlayerEntityParameters {
                velocity: Velocity::default(),
                position,
                pitch,
            },
        );
        Ok(())
    }

    fn spawn_internal(
        parent: Entity,
        commands: &mut Commands,
        storable: Storable,
        parameters: PlayerEntityParameters,
    ) {
        commands
            .spawn((
                Self {
                    pitch: parameters.pitch,
                },
                Cylinder {
                    height: 2.5,
                    radius: 0.3,
                },
                parameters.position,
                parameters.velocity,
                Transform::default(),
                GlobalTransform::default(),
                LoadsTerrain { radius: 8 },
                storable,
                ToSaveSpatial, // We want to save this as soon as its spawned.
                LocalPlayer, // FIXME this shouldn't just be a local player by default, but it's okay since multiplayer isn't implemented yet.
            ))
            .set_parent(parent)
            .with_children(|parent| {
                parent
                    .spawn((
                        Transform::from_translation(Vec3::new(0.0, 2.0, 0.0)),
                        GlobalTransform::default(),
                    ))
                    .with_children(|parent| {
                        parent.spawn((
                            Camera3dBundle {
                                transform: Transform::from_translation(Vec3::new(0.0, 0.0, 0.0)),
                                ..Default::default()
                            },
                            RayCast {
                                direction: Vec3::NEG_Z,
                                length: 256.0,
                            },
                            RayTerrainIntersectionList {
                                contact_limit: Some(std::num::NonZeroUsize::new(1).unwrap()),
                                contacts: HashMap::new(),
                            },
                            BlockPlacementContext::default(),
                            BlockRemovalContext::default(),
                        ));
                    });
            });
    }

    fn process_inputs(
        input_state: Res<InputState>,
        mut players: Query<
            (Entity, &mut Position, &mut Velocity, &mut PlayerEntity),
            With<LocalPlayer>,
        >,
        children: Query<&Children>,
        mut cameras: Query<&mut Transform, With<Camera>>,
    ) {
        for (player_entity, mut position, mut velocity, mut player) in players.iter_mut() {
            let local_movement =
                position.inverse_quat() * input_state.horizontal_movement.extend(0.0).xzy();
            velocity.translation = local_movement * PLAYER_SPEED;

            if input_state.jumping.is_pressed() {
                velocity.translation += Vec3::Y * PLAYER_SPEED;
            }

            if input_state.crouching.is_pressed() {
                velocity.translation -= Vec3::Y * PLAYER_SPEED;
            }

            position.rotation += input_state.look_movement.x;
            player.pitch += input_state.look_movement.y;
            player.pitch = player.pitch.clamp(-1.54, 1.54);

            // Update the player's camera to have the right pitch.
            let camera_iterator = DescendantIter::new(&children, player_entity);
            for camera in camera_iterator {
                if let Ok(mut transform) = cameras.get_mut(camera) {
                    transform.rotation = Quat::from_axis_angle(Vec3::X, player.pitch);
                }
            }
        }
    }

    fn place_block(
        input_state: Res<InputState>,
        mut players: Query<(&mut BlockPlacementContext, &RayTerrainIntersectionList)>,
        mut terrain_spaces: Query<(&TerrainSpace, &mut SpaceModificationRequestList)>,
        terrain: Query<&Chunk>,
        block_registry: Res<BlockRegistry>,
        time: Res<Time>,
    ) -> Result<()> {
        fn place_block(
            ray: &RayTerrainIntersectionList,
            terrain_spaces: &mut Query<(&TerrainSpace, &mut SpaceModificationRequestList)>,
            terrain: &Query<&Chunk>,
            block: Block,
        ) {
            let mut contact_iter = ray.contacts.iter();

            let mut closest_contact: Option<(Entity, &RayTerrainIntersection)> = contact_iter
                .next()
                .and_then(|(entity, terrain_contact_vec)| {
                    terrain_contact_vec
                        .first()
                        .map(|contact| (*entity, contact))
                });

            for (terrain_space_entity, contacts) in contact_iter {
                if let Some(first) = contacts.first() {
                    if let Some((_old_terrain_space_entity, contact)) = closest_contact {
                        if contact.distance > first.distance {
                            closest_contact = Some((*terrain_space_entity, first));
                        }
                    } else {
                        closest_contact = Some((*terrain_space_entity, first));
                    }
                }
            }

            // None just means there weren't any contacts at all.
            if let Some((terrain_entity, intersection)) = closest_contact {
                if let Ok((terrain_space, mut modification_request_list)) =
                    terrain_spaces.get_mut(terrain_entity)
                {
                    let block_coordinate = intersection.block_coordinate + intersection.normal;

                    let old_block = terrain_space.get_block(
                        terrain,
                        |terrain, entity| terrain.get(entity).ok(),
                        block_coordinate,
                    );

                    if old_block.is_none() {
                        modification_request_list.push(SpaceModificationRequest::ReplaceBlock {
                            coordinate: block_coordinate,
                            new_block: Some(block),
                        })
                    }
                } else {
                    log::warn!("Terrain disappeared after the closest intersection was found.")
                }
            }
        }

        // TODO this should be based on the player's inventory selection.
        let block = block_registry
            .get_by_tag(&BlockTag::try_from("core:default")?)?
            .spawn();

        match input_state.secondary_fire {
            ButtonState::JustPressed => {
                for (mut context, ray) in players.iter_mut() {
                    context.timer.reset();
                    context.button_held = true;

                    place_block(ray, &mut terrain_spaces, &terrain, block)
                }
            }
            ButtonState::Released => {
                for (mut context, _ray) in players.iter_mut() {
                    context.button_held = false;
                }
            }
            ButtonState::Held => {}
        }

        for (mut context, ray) in players.iter_mut() {
            if context.button_held {
                context.timer.tick(time.delta());

                for _ in 0..context.timer.times_finished_this_tick() {
                    place_block(ray, &mut terrain_spaces, &terrain, block);
                }
            }
        }

        Ok(())
    }

    // TODO this can probably be combined with the `place_block` system using some generics.
    #[allow(clippy::too_many_arguments)]
    fn remove_block(
        input_state: Res<InputState>,
        mut players: Query<(&mut BlockRemovalContext, &RayTerrainIntersectionList)>,
        mut terrain_spaces: Query<&mut SpaceModificationRequestList, With<TerrainSpace>>,
        time: Res<Time>,
    ) {
        fn remove_block(
            ray: &RayTerrainIntersectionList,
            terrain_spaces: &mut Query<&mut SpaceModificationRequestList, With<TerrainSpace>>,
        ) {
            let mut contact_iter = ray.contacts.iter();

            let mut closest_contact: Option<(Entity, &RayTerrainIntersection)> = contact_iter
                .next()
                .and_then(|(entity, terrain_contact_vec)| {
                    terrain_contact_vec
                        .first()
                        .map(|contact| (*entity, contact))
                });

            for (terrain_space_entity, contacts) in contact_iter {
                if let Some(first) = contacts.first() {
                    if let Some((_old_terrain_space_entity, contact)) = closest_contact {
                        if contact.distance > first.distance {
                            closest_contact = Some((*terrain_space_entity, first));
                        }
                    } else {
                        closest_contact = Some((*terrain_space_entity, first));
                    }
                }
            }

            // None just means there weren't any contacts at all.
            if let Some((terrain_entity, intersection)) = closest_contact {
                if let Ok(mut modification_request_list) = terrain_spaces.get_mut(terrain_entity) {
                    modification_request_list.push(SpaceModificationRequest::ReplaceBlock {
                        coordinate: intersection.block_coordinate,
                        new_block: None,
                    });
                } else {
                    log::warn!("Terrain disappeared after the closest intersection was found.")
                }
            }
        }

        match input_state.primary_fire {
            ButtonState::JustPressed => {
                for (mut context, ray) in players.iter_mut() {
                    context.timer.reset();
                    context.button_held = true;

                    remove_block(ray, &mut terrain_spaces)
                }
            }
            ButtonState::Released => {
                for (mut context, _ray) in players.iter_mut() {
                    context.button_held = false;
                }
            }
            ButtonState::Held => {}
        }

        for (mut context, ray) in players.iter_mut() {
            if context.button_held {
                context.timer.tick(time.delta());

                for _ in 0..context.timer.times_finished_this_tick() {
                    remove_block(ray, &mut terrain_spaces);
                }
            }
        }
    }
}

#[derive(Debug, Component, Reflect)]
#[reflect(Component)]
struct BlockPlacementContext {
    timer: Timer,
    button_held: bool,
}

impl Default for BlockPlacementContext {
    fn default() -> Self {
        Self {
            timer: Timer::new(Duration::from_millis(250), TimerMode::Repeating),
            button_held: Default::default(),
        }
    }
}

#[derive(Debug, Component, Reflect)]
#[reflect(Component)]
struct BlockRemovalContext {
    timer: Timer,
    button_held: bool,
}

impl Default for BlockRemovalContext {
    fn default() -> Self {
        Self {
            timer: Timer::new(Duration::from_millis(250), TimerMode::Repeating),
            button_held: Default::default(),
        }
    }
}

/// Contains everything needed to add first-person fly camera behavior to your game
#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                PlayerEntity::process_inputs.before(PhysicsPlugin),
                PlayerEntity::place_block.pipe(crate::error_handler),
                PlayerEntity::remove_block,
            ),
        );

        EntitySerializationManager::register::<PlayerEntity, _>(app);
    }
}
