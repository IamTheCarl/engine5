use anyhow::Result;
use bevy::ecs::event::{Events, ManualEventReader};
use bevy::input::mouse;
use bevy::prelude::*;
use bevy::window::{CursorGrabMode, PrimaryWindow};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::Duration;

/// Currently just a modified version of https://crates.io/crates/bevy_flycam.
///
use crate::world::physics::{
    Cylinder, Position, RayCast, RayTerrainIntersection, RayTerrainIntersectionList, Velocity,
};
use crate::world::terrain::{
    Block, BlockRegistry, BlockTag, Chunk, LoadsTerrain, TerrainSpace, UpdateMesh,
};

use super::spatial_entities::storage::{
    BootstrapEntityInfo, DataLoader, DataSaver, EntitySerializationManager, EntityStorage,
    EntityTypeId, SpatialEntity, Storable, ToSaveSpatial,
};

/// Keeps track of mouse motion events, pitch, and yaw
#[derive(Resource, Default)]
struct InputState {
    reader_motion: ManualEventReader<mouse::MouseMotion>,
    pitch: f32,
    yaw: f32,
}

/// Mouse sensitivity and movement speed

#[derive(Resource)]
pub struct MovementSettings {
    pub sensitivity: f32,
    pub speed: f32,
}

impl Default for MovementSettings {
    fn default() -> Self {
        Self {
            sensitivity: 0.00012,
            speed: 12.,
        }
    }
}

#[derive(Component, Reflect, Default)]
#[reflect(Component)]
pub struct MovementControl;

/// Grabs/ungrabs mouse cursor
fn toggle_grab_cursor(window: &mut Window) {
    match window.cursor.grab_mode {
        CursorGrabMode::None => {
            window.cursor.grab_mode = CursorGrabMode::Confined;
            window.cursor.visible = false;
        }
        _ => {
            window.cursor.grab_mode = CursorGrabMode::None;
            window.cursor.visible = true;
        }
    }
}

/// Grabs the cursor when game first starts
fn initial_grab_cursor(mut windows: Query<&mut Window, With<PrimaryWindow>>) {
    if let Ok(mut window) = windows.get_single_mut() {
        toggle_grab_cursor(&mut window);
    } else {
        warn!("Primary window not found for `initial_grab_cursor`!");
    }
}

#[derive(Deserialize)]
struct PlayerEntityParameters {
    velocity: Velocity,
    position: Position,
}

#[derive(Component)]
pub struct PlayerEntity;

impl SpatialEntity<(Entity, &Storable, &Position, &Velocity)> for PlayerEntity {
    const TYPE_ID: EntityTypeId = 0;
    const BOOTSTRAP: BootstrapEntityInfo = BootstrapEntityInfo::LocalPlayer;

    fn load(data_loader: DataLoader, commands: &mut Commands) -> Result<()> {
        let (storable, parameters) = data_loader.load::<PlayerEntityParameters>()?;
        Self::spawn_internal(commands, storable, parameters);
        Ok(())
    }

    fn save(
        (entity, storable, position, velocity): (Entity, &Storable, &Position, &Velocity),
        data_saver: DataSaver,
    ) {
        #[derive(Serialize)]
        struct PlayerStorageSerialization<'a> {
            velocity: &'a Velocity,
            position: &'a Position,
        }

        data_saver.save(
            entity,
            storable,
            &PlayerStorageSerialization { position, velocity },
        );
    }
}

impl PlayerEntity {
    /// Spawns the `Camera3dBundle` to be controlled
    pub fn spawn(
        commands: &mut Commands,
        storage: &EntityStorage,
        position: Position,
    ) -> Result<()> {
        let storable = storage.new_storable_component::<PlayerEntity, _>()?;

        Self::spawn_internal(
            commands,
            storable,
            PlayerEntityParameters {
                velocity: Velocity::default(),
                position,
            },
        );
        Ok(())
    }

    fn spawn_internal(
        commands: &mut Commands,
        storable: Storable,
        parameters: PlayerEntityParameters,
    ) {
        commands
            .spawn((
                Self,
                Cylinder {
                    height: 2.5,
                    radius: 0.3,
                },
                parameters.position,
                parameters.velocity,
                Transform::default(),
                GlobalTransform::default(),
                MovementControl,
                LoadsTerrain { radius: 8 },
                storable,
                ToSaveSpatial, // We want to save this as soon as its spawned.
            ))
            .with_children(|parent| {
                parent
                    .spawn((
                        Transform::from_translation(Vec3::new(0.0, 2.0, 0.0)),
                        GlobalTransform::default(),
                        MovementControl,
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
}

/// Handles keyboard input and movement
fn player_move(
    keys: Res<Input<KeyCode>>,
    windows: Query<&Window, With<PrimaryWindow>>,
    settings: Res<MovementSettings>,
    mut query: Query<(&Position, &mut Velocity), With<MovementControl>>,
) {
    if let Ok(window) = windows.get_single() {
        for (position, mut velocity) in query.iter_mut() {
            velocity.translation = Vec3::ZERO;
            let local_z = position.local_z();
            let forward = -Vec3::new(local_z.x, 0., local_z.z);
            let right = Vec3::new(local_z.z, 0., -local_z.x);

            for key in keys.get_pressed() {
                match window.cursor.grab_mode {
                    CursorGrabMode::None => (),
                    _ => match key {
                        KeyCode::E => velocity.translation += forward,
                        KeyCode::D => velocity.translation -= forward,
                        KeyCode::S => velocity.translation -= right,
                        KeyCode::F => velocity.translation += right,
                        KeyCode::Space => velocity.translation += Vec3::Y,
                        KeyCode::A => velocity.translation -= Vec3::Y,
                        _ => (),
                    },
                }
            }

            velocity.translation = velocity.translation.normalize_or_zero() * settings.speed;

            // position.translation += velocity * time.delta_seconds() * settings.speed;
        }
    } else {
        warn!("Primary window not found for `player_move`!");
    }
}

fn update_input_state(
    settings: Res<MovementSettings>,
    windows: Query<&Window, With<PrimaryWindow>>,
    mut state: ResMut<InputState>,
    motion: Res<Events<mouse::MouseMotion>>,
) {
    if let Ok(window) = windows.get_single() {
        let mut delta_state = state.as_mut();

        for event in delta_state.reader_motion.iter(&motion) {
            match window.cursor.grab_mode {
                CursorGrabMode::None => (),
                _ => {
                    // Using smallest of height or width ensures equal vertical and horizontal sensitivity
                    let window_scale = window.height().min(window.width());
                    delta_state.pitch -=
                        (settings.sensitivity * event.delta.y * window_scale).to_radians();
                    delta_state.yaw -=
                        (settings.sensitivity * event.delta.x * window_scale).to_radians();
                }
            }

            delta_state.pitch = delta_state.pitch.clamp(-1.54, 1.54);
        }
    } else {
        warn!("Primary window not found for `player_look`!");
    }
}

// TODO system to update player's position.
// TODO system to update player's transform.
/// Handles looking around if cursor is locked
fn player_look(
    state: Res<InputState>,
    mut query: Query<(&mut Transform, With<MovementControl>), Without<Position>>,
) {
    let delta_state = state.as_ref();
    for (mut transform, _) in query.iter_mut() {
        // Order is important to prevent unintended roll
        // transform.rotation = Quat::from_axis_angle(Vec3::Y, delta_state.yaw)
        //     * Quat::from_axis_angle(Vec3::X, delta_state.pitch);

        transform.rotation = Quat::from_axis_angle(Vec3::X, delta_state.pitch);
    }
}

fn player_turn(state: Res<InputState>, mut query: Query<(&mut Position, With<MovementControl>)>) {
    let delta_state = state.as_ref();
    for (mut position, _) in query.iter_mut() {
        // Order is important to prevent unintended roll
        position.rotation = delta_state.yaw;
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

#[allow(clippy::too_many_arguments)]
fn place_block(
    mut commands: Commands,
    time: Res<Time>,
    windows: Query<&Window, With<PrimaryWindow>>,
    buttons: Res<Input<MouseButton>>,
    block_registry: Res<BlockRegistry>,
    mut players: Query<(&mut BlockPlacementContext, &RayTerrainIntersectionList)>,
    mut terrain_spaces: Query<&mut TerrainSpace>,
    mut terrain: Query<&mut Chunk>,
) -> Result<()> {
    fn place_block(
        ray: &RayTerrainIntersectionList,
        terrain_spaces: &mut Query<&mut TerrainSpace>,
        terrain: &mut Query<&mut Chunk>,
        block: Block,
        commands: &mut Commands,
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
            if let Ok(terrain_space) = terrain_spaces.get_mut(terrain_entity) {
                terrain_space.get_block_mut(
                    terrain,
                    |terrain, entity| terrain.get_mut(entity).ok(),
                    intersection.block_coordinate + intersection.normal,
                    |block_memory| {
                        if let Some((chunk_entity, block_memory)) = block_memory {
                            if block_memory.is_none() {
                                *block_memory = Some(block);

                                if let Some(mut command_list) = commands.get_entity(chunk_entity) {
                                    command_list.insert(UpdateMesh);
                                }
                            }
                        }
                    },
                );
            } else {
                log::warn!("Terrain disappeared after the closest intersection was found.")
            }
        }
    }

    // TODO this should be based on the player's inventory selection.
    let block = block_registry
        .get_by_tag(&BlockTag::try_from("core:default")?)?
        .spawn();

    if let Ok(window) = windows.get_single() {
        match window.cursor.grab_mode {
            CursorGrabMode::None => {
                for (mut context, _ray) in players.iter_mut() {
                    context.button_held = false;
                }
            }
            CursorGrabMode::Locked | CursorGrabMode::Confined => {
                if buttons.just_pressed(MouseButton::Right) {
                    for (mut context, ray) in players.iter_mut() {
                        context.timer.reset();
                        context.button_held = true;

                        place_block(ray, &mut terrain_spaces, &mut terrain, block, &mut commands)
                    }
                }
                if buttons.just_released(MouseButton::Right) {
                    for (mut context, _ray) in players.iter_mut() {
                        context.button_held = false;
                    }
                }
            }
        }
    } else {
        for (mut context, _ray) in players.iter_mut() {
            context.button_held = false;
        }
        warn!("Primary window not found for `place_block`!");
    }

    for (mut context, ray) in players.iter_mut() {
        if context.button_held {
            context.timer.tick(time.delta());

            for _ in 0..context.timer.times_finished_this_tick() {
                place_block(ray, &mut terrain_spaces, &mut terrain, block, &mut commands);
            }
        }
    }

    Ok(())
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

// TODO this can be combined with the `place_block` system using some generics.
#[allow(clippy::too_many_arguments)]
fn remove_block(
    mut commands: Commands,
    time: Res<Time>,
    windows: Query<&Window, With<PrimaryWindow>>,
    buttons: Res<Input<MouseButton>>,
    mut players: Query<(&mut BlockRemovalContext, &RayTerrainIntersectionList)>,
    mut terrain_spaces: Query<&mut TerrainSpace>,
    mut terrain: Query<&mut Chunk>,
) {
    fn remove_block(
        ray: &RayTerrainIntersectionList,
        terrain_spaces: &mut Query<&mut TerrainSpace>,
        terrain: &mut Query<&mut Chunk>,
        commands: &mut Commands,
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
            if let Ok(terrain_space) = terrain_spaces.get_mut(terrain_entity) {
                terrain_space.get_block_mut(
                    terrain,
                    |terrain: &mut Query<&mut Chunk>, entity| terrain.get_mut(entity).ok(),
                    intersection.block_coordinate,
                    |block_memory| {
                        if let Some((chunk_entity, block_memory)) = block_memory {
                            if block_memory.is_some() {
                                *block_memory = None;

                                if let Some(mut command_list) = commands.get_entity(chunk_entity) {
                                    command_list.insert(UpdateMesh);
                                }
                            }
                        }
                    },
                );
            } else {
                log::warn!("Terrain disappeared after the closest intersection was found.")
            }
        }
    }

    if let Ok(window) = windows.get_single() {
        match window.cursor.grab_mode {
            CursorGrabMode::None => {
                for (mut context, _ray) in players.iter_mut() {
                    context.button_held = false;
                }
            }
            CursorGrabMode::Locked | CursorGrabMode::Confined => {
                if buttons.just_pressed(MouseButton::Left) {
                    for (mut context, ray) in players.iter_mut() {
                        context.timer.reset();
                        context.button_held = true;

                        remove_block(ray, &mut terrain_spaces, &mut terrain, &mut commands)
                    }
                }
                if buttons.just_released(MouseButton::Left) {
                    for (mut context, _ray) in players.iter_mut() {
                        context.button_held = false;
                    }
                }
            }
        }
    } else {
        for (mut context, _ray) in players.iter_mut() {
            context.button_held = false;
        }
        warn!("Primary window not found for `remove_block`!");
    }

    for (mut context, ray) in players.iter_mut() {
        if context.button_held {
            context.timer.tick(time.delta());

            for _ in 0..context.timer.times_finished_this_tick() {
                remove_block(ray, &mut terrain_spaces, &mut terrain, &mut commands);
            }
        }
    }
}

fn cursor_grab(keys: Res<Input<KeyCode>>, mut windows: Query<&mut Window, With<PrimaryWindow>>) {
    if let Ok(mut window) = windows.get_single_mut() {
        if keys.just_pressed(KeyCode::Escape) {
            toggle_grab_cursor(&mut window);
        }
    } else {
        warn!("Primary window not found for `cursor_grab`!");
    }
}

/// Contains everything needed to add first-person fly camera behavior to your game
#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<InputState>()
            .init_resource::<MovementSettings>()
            .add_startup_system(initial_grab_cursor)
            .add_system(player_look.after(update_input_state))
            .add_system(player_turn.after(update_input_state))
            .add_system(player_move.after(update_input_state))
            .add_system(update_input_state) // TODO should happen before movement update.
            .add_system(place_block.pipe(crate::error_handler))
            .add_system(remove_block)
            .add_system(cursor_grab);

        EntitySerializationManager::register::<PlayerEntity, _>(app);
    }
}
