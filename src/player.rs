use bevy::ecs::event::{Events, ManualEventReader};
use bevy::input::mouse::MouseMotion;
use bevy::prelude::*;
use bevy::window::CursorGrabMode;
use ordered_float::NotNan;

/// Currently just a modified version of https://crates.io/crates/bevy_flycam.
///
use crate::physics::{Cylinder, Position, SpatialHashOffset, Velocity};
use crate::terrain::{BlockLocalCoordinate, BlockRegistry, BlockTag, Chunk, TerrainPlugin};

/// Keeps track of mouse motion events, pitch, and yaw
#[derive(Resource, Default)]
struct InputState {
    reader_motion: ManualEventReader<MouseMotion>,
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
            speed: 6.,
        }
    }
}

#[derive(Component)]
pub struct MovementControl;

/// Grabs/ungrabs mouse cursor
fn toggle_grab_cursor(window: &mut Window) {
    match window.cursor_grab_mode() {
        CursorGrabMode::None => {
            window.set_cursor_grab_mode(CursorGrabMode::Confined);
            window.set_cursor_visibility(false);
        }
        _ => {
            window.set_cursor_grab_mode(CursorGrabMode::None);
            window.set_cursor_visibility(true);
        }
    }
}

/// Grabs the cursor when game first starts
fn initial_grab_cursor(mut windows: ResMut<Windows>) {
    if let Some(window) = windows.get_primary_mut() {
        toggle_grab_cursor(window);
    } else {
        warn!("Primary window not found for `initial_grab_cursor`!");
    }
}

/// Spawns the `Camera3dBundle` to be controlled
fn setup_player(mut commands: Commands, block_registry: Res<BlockRegistry>) {
    let default_tag = BlockTag::try_from("core:default").unwrap();
    let default_data = block_registry.get_by_tag(&default_tag).unwrap();
    let default_block = default_data.spawn();

    let mut hump_chunk = Chunk::new(Some(default_block));

    for (position, block) in hump_chunk.iter_mut() {
        let position = position.as_vec3();

        let height = ((position.x / 16.0) * std::f64::consts::PI as f32).sin()
            * ((position.z / 16.0) * std::f64::consts::PI as f32).sin()
            * 16.0;
        if position.y > height {
            *block = None;
        }
    }

    commands
        .spawn((
            // Cylinder {
            //     height: NotNan::new(3.0).unwrap(),
            //     radius: NotNan::new(2.0).unwrap(),
            // },
            hump_chunk,
            SpatialHashOffset {
                translation: Vec3::new(8.0, 0.0, 8.0),
            },
            Position {
                translation: Vec3::new(-2.0, 5.0, 5.0),
                rotation: 0.0,
            },
            Velocity::default(),
            Transform::default(),
            GlobalTransform::default(),
            MovementControl,
        ))
        .with_children(|parent| {
            parent
                .spawn((
                    Transform::from_translation(Vec3::new(0.0, 0.0, 0.0)),
                    GlobalTransform::default(),
                    MovementControl,
                ))
                .with_children(|parent| {
                    parent.spawn((Camera3dBundle {
                        transform: Transform::from_translation(Vec3::new(
                            0.0 + 0.1,
                            2.0,
                            0.0 + 32.0,
                        )),
                        ..Default::default()
                    },));
                });
        });
}

/// Handles keyboard input and movement
fn player_move(
    keys: Res<Input<KeyCode>>,
    windows: Res<Windows>,
    settings: Res<MovementSettings>,
    mut query: Query<(&Position, &mut Velocity), With<MovementControl>>,
) {
    if let Some(window) = windows.get_primary() {
        for (position, mut velocity) in query.iter_mut() {
            velocity.translation = Vec3::ZERO;
            let local_z = position.local_z();
            let forward = -Vec3::new(local_z.x, 0., local_z.z);
            let right = Vec3::new(local_z.z, 0., -local_z.x);

            for key in keys.get_pressed() {
                match window.cursor_grab_mode() {
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
    windows: Res<Windows>,
    mut state: ResMut<InputState>,
    motion: Res<Events<MouseMotion>>,
) {
    if let Some(window) = windows.get_primary() {
        let mut delta_state = state.as_mut();

        for ev in delta_state.reader_motion.iter(&motion) {
            match window.cursor_grab_mode() {
                CursorGrabMode::None => (),
                _ => {
                    // Using smallest of height or width ensures equal vertical and horizontal sensitivity
                    let window_scale = window.height().min(window.width());
                    delta_state.pitch -=
                        (settings.sensitivity * ev.delta.y * window_scale).to_radians();
                    delta_state.yaw -=
                        (settings.sensitivity * ev.delta.x * window_scale).to_radians();
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

fn cursor_grab(keys: Res<Input<KeyCode>>, mut windows: ResMut<Windows>) {
    if let Some(window) = windows.get_primary_mut() {
        if keys.just_pressed(KeyCode::Escape) {
            toggle_grab_cursor(window);
        }
    } else {
        warn!("Primary window not found for `cursor_grab`!");
    }
}

/// Contains everything needed to add first-person fly camera behavior to your game
#[derive(StageLabel)]
pub struct PlayerPlugin;

impl Plugin for PlayerPlugin {
    fn build(&self, app: &mut App) {
        app.add_startup_stage_after(TerrainPlugin, PlayerPlugin, SystemStage::parallel());
        app.add_startup_system_to_stage(PlayerPlugin, setup_player);

        app.init_resource::<InputState>()
            .init_resource::<MovementSettings>()
            .add_startup_system(initial_grab_cursor)
            .add_system(player_look)
            .add_system(player_turn)
            .add_system(player_move)
            .add_system(update_input_state) // TODO should happen before movement update.
            .add_system(cursor_grab);
    }
}
