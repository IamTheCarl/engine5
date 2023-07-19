use bevy::{
    input::{
        mouse::{self, MouseButtonInput},
        ButtonState,
    },
    prelude::*,
    window::{CursorGrabMode, PrimaryWindow},
};

#[derive(Resource, Default)]
pub struct InputState {
    pub horizontal_movement: Vec2,
    pub look_movement: Vec2,
    pub jumping: bool,
    pub crouching: bool,
}

#[derive(Debug)]
pub struct PrimaryFireEvent {
    pub button_state: ButtonState,
}

#[derive(Debug)]
pub struct SecondaryFireEvent {
    pub button_state: ButtonState,
}

// TODO this needs to be configurable.
#[derive(Resource)]
pub struct ControlSettings {
    pub mouse_sensitivity: f32,
}

impl Default for ControlSettings {
    fn default() -> Self {
        Self {
            mouse_sensitivity: 0.00012,
        }
    }
}

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

fn cursor_grab(keys: Res<Input<KeyCode>>, mut windows: Query<&mut Window, With<PrimaryWindow>>) {
    if let Ok(mut window) = windows.get_single_mut() {
        if keys.just_pressed(KeyCode::Escape) {
            toggle_grab_cursor(&mut window);
        }
    }
}

fn initial_grab_cursor(mut windows: Query<&mut Window, With<PrimaryWindow>>) {
    if let Ok(mut window) = windows.get_single_mut() {
        toggle_grab_cursor(&mut window);
    }
}

#[allow(clippy::too_many_arguments)]
pub fn update_input_state(
    keyboard_input_state: Res<Input<KeyCode>>,
    mut mouse_movement_input_events: EventReader<mouse::MouseMotion>,
    mut mouse_button_input_events: EventReader<MouseButtonInput>,
    windows: Query<&Window, With<PrimaryWindow>>,
    settings: Res<ControlSettings>,
    mut input_state: ResMut<InputState>,
    mut primary_fire_events_tx: EventWriter<PrimaryFireEvent>,
    mut secondary_fire_events_tx: EventWriter<SecondaryFireEvent>,
) {
    if let Ok(window) = windows.get_single() {
        // We have to reset the input state on each frame.
        *input_state = InputState::default();

        let forward = Vec2::new(0.0, 1.0);
        let right = Vec2::new(1.0, 0.0);

        let mut horizontal_movement = Vec2::ZERO;

        // Process keyboard inputs.
        match window.cursor.grab_mode {
            CursorGrabMode::Locked | CursorGrabMode::Confined => {
                for key in keyboard_input_state.get_pressed() {
                    match key {
                        KeyCode::E => horizontal_movement -= forward,
                        KeyCode::D => horizontal_movement += forward,
                        KeyCode::S => horizontal_movement -= right,
                        KeyCode::F => horizontal_movement += right,
                        KeyCode::Space => input_state.jumping = true,
                        KeyCode::A => input_state.crouching = true,
                        _ => (),
                    }
                }
            }
            CursorGrabMode::None => {}
        }

        // Process mouse movement.
        match window.cursor.grab_mode {
            CursorGrabMode::Locked | CursorGrabMode::Confined => {
                for event in mouse_movement_input_events.iter() {
                    let window_scale = window.height().min(window.width());
                    input_state.look_movement.y =
                        (settings.mouse_sensitivity * -event.delta.y * window_scale).to_radians();
                    input_state.look_movement.x =
                        (settings.mouse_sensitivity * -event.delta.x * window_scale).to_radians();
                }
            }
            CursorGrabMode::None => {}
        }

        // Process mouse buttons.
        for mouse_event in mouse_button_input_events.iter() {
            match mouse_event.button {
                MouseButton::Left => primary_fire_events_tx.send(PrimaryFireEvent {
                    button_state: mouse_event.state,
                }),
                MouseButton::Right => secondary_fire_events_tx.send(SecondaryFireEvent {
                    button_state: mouse_event.state,
                }),
                MouseButton::Middle => {}
                MouseButton::Other(_) => {}
            }
        }

        // Make sure multiple inputs don't add up to something greater than 1.0.
        let horizontal_length = horizontal_movement.length();
        let horizontal_normal = horizontal_movement.normalize_or_zero();
        input_state.horizontal_movement = horizontal_normal * horizontal_length.min(1.0);
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct PlayerControls;

impl Plugin for PlayerControls {
    fn build(&self, app: &mut App) {
        app.add_startup_system(|mut commands: Commands| {
            commands.insert_resource(InputState::default());
            commands.insert_resource(ControlSettings::default());
        });
        app.add_startup_system(initial_grab_cursor);
        app.add_system(cursor_grab);
        app.add_system(update_input_state);
        app.add_event::<PrimaryFireEvent>();
        app.add_event::<SecondaryFireEvent>();
    }
}
