use anyhow::Result;
use bevy::{ecs::system::EntityCommands, math::Vec3Swizzles, prelude::*};
use proc_macros::entity_serialization;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, time::Duration};

use crate::{
    config::controls::{ButtonState, InputState, PlayerControlsPlugin},
    multiplayer::RemotePlayer,
    world::{
        physics::{
            Cylinder, PhysicsPlugin, Position, RayCast, RayTerrainIntersection,
            RayTerrainIntersectionList, Velocity,
        },
        spatial_entities::storage::{
            EntitySerializationManager, EntityStorage, Storable, ToSaveSpatial,
        },
        terrain::{
            terrain_space::{
                ModifyTerrain, SpaceModificationRequest, SpaceModificationRequestList,
            },
            Block, BlockRegistry, BlockTag, Chunk, LoadsTerrain, TerrainSpace,
        },
        ViewRadius,
    },
};

pub mod spawner;

const PLAYER_SPEED: f32 = 12.0;

#[entity_serialization(type_id = 3, marker = PlayerEntity, bootstrap = BootstrapEntityInfo::LocalPlayer)]
struct PlayerEntityParameters {
    velocity: Velocity,
    position: Position,
    player_info: PlayerEntity,
}

/// Marks a player as being local, making it responsive to local input controls.
#[derive(Component)]
pub struct LocalPlayer;

/// Indicates that this entity is a player's head.
/// Only local players get a head.
#[derive(Component)]
pub struct PlayerHead;

#[derive(Component, Serialize, Deserialize)]
pub struct PlayerEntity {
    pub name: String,
    pitch: f32,
}

impl PlayerEntity {
    pub fn spawn_local(
        parent: Entity,
        commands: &mut Commands,
        storage: &EntityStorage,
        position: Position,
        name: String,
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
                player_info: PlayerEntity { pitch, name },
            },
        )
        .insert(LocalPlayer);
        Ok(())
    }

    pub fn spawn_deactivated<'w, 's, 'a>(
        parent: Entity,
        commands: &'a mut Commands<'w, 's>,
        storage: &EntityStorage,
        position: Position,
        name: String,
        pitch: f32,
    ) -> Result<EntityCommands<'w, 's, 'a>> {
        let storable = storage.new_storable_component::<PlayerEntity, _>()?;

        Ok(Self::spawn_internal(
            parent,
            commands,
            storable,
            PlayerEntityParameters {
                velocity: Velocity::default(),
                position,
                player_info: PlayerEntity { pitch, name },
            },
        ))
    }

    pub fn add_body(commands: &mut EntityCommands) {
        commands.insert(Cylinder {
            height: 2.5,
            radius: 0.3,
        });
    }

    pub fn remove_body(commands: &mut EntityCommands) {
        commands.remove::<Cylinder>();
    }

    fn spawn_internal<'w, 's, 'a>(
        parent: Entity,
        commands: &'a mut Commands<'w, 's>,
        storable: Storable,
        parameters: PlayerEntityParameters,
    ) -> EntityCommands<'w, 's, 'a> {
        let mut entity_commands = commands.spawn((
            parameters.player_info,
            parameters.position,
            parameters.velocity,
            Transform::default(),
            GlobalTransform::default(),
            LoadsTerrain,
            ViewRadius { chunks: 8 },
            storable,
            ToSaveSpatial, // We want to save this as soon as its spawned.
        ));
        entity_commands.set_parent(parent);
        entity_commands
    }

    pub fn spawn_head(commands: &mut Commands, player_entity: Entity) {
        commands.entity(player_entity).with_children(|parent| {
            parent
                .spawn((
                    Transform::from_translation(Vec3::new(0.0, 2.0, 0.0)),
                    GlobalTransform::default(),
                    PlayerHead,
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

fn activate_players<C>(mut commands: Commands, players: Query<Entity, Added<C>>)
where
    C: Component,
{
    for player in players.iter() {
        let mut player = commands.entity(player);
        PlayerEntity::add_body(&mut player);
    }
}

fn deactivate_players<C>(mut commands: Commands, mut players: RemovedComponents<C>)
where
    C: Component,
{
    for player in &mut players {
        if let Some(mut player) = commands.get_entity(player) {
            PlayerEntity::remove_body(&mut player);
        }
    }
}

fn head_spawner(mut commands: Commands, players: Query<Entity, Added<LocalPlayer>>) {
    for player in players.iter() {
        PlayerEntity::spawn_head(&mut commands, player);
    }
}

fn head_remover(
    mut commands: Commands,
    heads: Query<(Entity, &Parent), With<PlayerHead>>,
    mut removed: RemovedComponents<LocalPlayer>,
) {
    for removed in &mut removed {
        for (head, player) in heads.iter() {
            if player.get() == removed {
                commands.entity(head).despawn_recursive();
            }
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                PlayerEntity::process_inputs
                    .before(PhysicsPlugin)
                    .after(PlayerControlsPlugin),
                PlayerEntity::place_block
                    .pipe(crate::error_handler)
                    .before(ModifyTerrain),
                PlayerEntity::remove_block.before(ModifyTerrain),
                head_spawner,
                head_remover,
                activate_players::<LocalPlayer>,
                deactivate_players::<LocalPlayer>,
                activate_players::<RemotePlayer>,
                deactivate_players::<RemotePlayer>,
            ),
        );

        EntitySerializationManager::register::<PlayerEntity, _>(app);
        spawner::setup(app);
    }
}
