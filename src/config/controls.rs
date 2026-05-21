use bevy::{
    input::mouse::AccumulatedMouseMotion,
    prelude::*,
    window::{CursorGrabMode, PrimaryWindow},
};
use bevy_console::ConsoleOpen;
use ordered_float::NotNan;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

use super::{Config, LoadConfigSet};
use crate::{world::WorldState, GameState};

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Debug)]
pub enum RealButton {
    Key { key_code: KeyCode },
    MouseButton { mouse_button: MouseButton },
    GamePad { gamepad_button: GamepadButton },
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Debug)]
pub enum MouseAxis {
    X,
    Y,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Debug)]
pub enum RealAnalogInput {
    Mouse {
        axis: MouseAxis,
        scale: NotNan<f32>,
    },
    GamePad {
        axis: GamepadAxis,
        scale: NotNan<f32>,
    },
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Debug)]
pub enum Button {
    RealButton {
        button: RealButton,
    },
    AnalogSimulatedButton {
        analog: RealAnalogInput,
        trigger_point: NotNan<f32>,
    },
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Debug)]
pub enum AnalogInput {
    RealAnalogInput {
        analog_input: RealAnalogInput,
    },
    Button {
        button: RealButton,
        scale: NotNan<f32>,
    },
}

#[derive(Serialize, Deserialize, Debug)]
pub struct AxisSet {
    x: HashSet<AnalogInput>,
    y: HashSet<AnalogInput>,
}

impl AxisSet {
    fn read(&self, frame: InputFrame) -> Vec2 {
        Vec2::new(self.x.read(frame), self.y.read(frame))
    }
}

#[derive(Resource, Serialize, Deserialize, Debug)]
#[serde(default)]
pub struct InputMap {
    horizontal_movement: AxisSet,
    look_movement: AxisSet,
    jumping: HashSet<Button>,
    crouching: HashSet<Button>,
    primary_fire: HashSet<Button>,
    secondary_fire: HashSet<Button>,
}

impl Config for InputMap {
    const CONFIG_FILE: &'static str = "input_map.yaml";
}

impl Default for InputMap {
    fn default() -> Self {
        Self {
            horizontal_movement: AxisSet {
                x: HashSet::from([
                    AnalogInput::Button {
                        button: RealButton::Key {
                            key_code: KeyCode::KeyD,
                        },
                        scale: NotNan::new(1.0).unwrap(),
                    },
                    AnalogInput::Button {
                        button: RealButton::Key {
                            key_code: KeyCode::KeyA,
                        },
                        scale: NotNan::new(-1.0).unwrap(),
                    },
                    AnalogInput::RealAnalogInput {
                        analog_input: RealAnalogInput::GamePad {
                            axis: GamepadAxis::LeftStickX,
                            scale: NotNan::new(1.0).unwrap(),
                        },
                    },
                ]),
                y: HashSet::from([
                    AnalogInput::Button {
                        button: RealButton::Key {
                            key_code: KeyCode::KeyW,
                        },
                        scale: NotNan::new(-1.0).unwrap(),
                    },
                    AnalogInput::Button {
                        button: RealButton::Key {
                            key_code: KeyCode::KeyS,
                        },
                        scale: NotNan::new(1.0).unwrap(),
                    },
                    AnalogInput::RealAnalogInput {
                        analog_input: RealAnalogInput::GamePad {
                            axis: GamepadAxis::LeftStickY,
                            scale: NotNan::new(-1.0).unwrap(),
                        },
                    },
                ]),
            },
            look_movement: AxisSet {
                x: HashSet::from([
                    AnalogInput::RealAnalogInput {
                        analog_input: RealAnalogInput::Mouse {
                            axis: MouseAxis::X,
                            scale: NotNan::new(0.00012).unwrap(),
                        },
                    },
                    AnalogInput::RealAnalogInput {
                        analog_input: RealAnalogInput::GamePad {
                            axis: GamepadAxis::RightStickX,
                            scale: NotNan::new(-1.0).unwrap(),
                        },
                    },
                ]),
                y: HashSet::from([
                    AnalogInput::RealAnalogInput {
                        analog_input: RealAnalogInput::Mouse {
                            axis: MouseAxis::Y,
                            scale: NotNan::new(0.00012).unwrap(),
                        },
                    },
                    AnalogInput::RealAnalogInput {
                        analog_input: RealAnalogInput::GamePad {
                            axis: GamepadAxis::RightStickY,
                            scale: NotNan::new(1.0).unwrap(),
                        },
                    },
                ]),
            },
            jumping: HashSet::from([
                Button::RealButton {
                    button: RealButton::Key {
                        key_code: KeyCode::Space,
                    },
                },
                Button::RealButton {
                    button: RealButton::GamePad {
                        gamepad_button: GamepadButton::RightThumb,
                    },
                },
            ]),
            crouching: HashSet::from([
                Button::RealButton {
                    button: RealButton::Key {
                        key_code: KeyCode::ShiftLeft,
                    },
                },
                Button::RealButton {
                    button: RealButton::GamePad {
                        gamepad_button: GamepadButton::LeftThumb,
                    },
                },
            ]),
            primary_fire: HashSet::from([
                Button::RealButton {
                    button: RealButton::MouseButton {
                        mouse_button: MouseButton::Left,
                    },
                },
                Button::AnalogSimulatedButton {
                    analog: RealAnalogInput::GamePad {
                        axis: GamepadAxis::RightZ,
                        scale: NotNan::new(1.0).unwrap(),
                    },
                    trigger_point: NotNan::new(0.5).unwrap(),
                },
                Button::RealButton {
                    button: RealButton::GamePad {
                        gamepad_button: GamepadButton::RightTrigger,
                    },
                },
            ]),
            secondary_fire: HashSet::from([
                Button::RealButton {
                    button: RealButton::MouseButton {
                        mouse_button: MouseButton::Right,
                    },
                },
                Button::AnalogSimulatedButton {
                    analog: RealAnalogInput::GamePad {
                        axis: GamepadAxis::LeftZ,
                        scale: NotNan::new(1.0).unwrap(),
                    },
                    trigger_point: NotNan::new(0.5).unwrap(),
                },
                Button::RealButton {
                    button: RealButton::GamePad {
                        gamepad_button: GamepadButton::LeftTrigger,
                    },
                },
            ]),
        }
    }
}

trait IsPressed {
    fn is_pressed(&self, frame: InputFrame) -> bool;
}

impl IsPressed for RealButton {
    fn is_pressed(&self, frame: InputFrame) -> bool {
        match self {
            RealButton::Key { key_code } => frame.keyboard_status.pressed(*key_code),
            RealButton::MouseButton { mouse_button } => frame.mouse_status.pressed(*mouse_button),
            RealButton::GamePad { gamepad_button } => {
                let mut pressed = false;

                for gamepad in frame.gamepad_entities.iter() {
                    pressed |= gamepad.pressed(*gamepad_button);
                }

                pressed
            }
        }
    }
}

impl IsPressed for Button {
    fn is_pressed(&self, frame: InputFrame) -> bool {
        match self {
            Button::RealButton { button } => button.is_pressed(frame),
            Button::AnalogSimulatedButton {
                analog,
                trigger_point,
            } => {
                let reading = analog.read(frame);

                reading >= trigger_point.into_inner()
            }
        }
    }
}

impl IsPressed for HashSet<Button> {
    fn is_pressed(&self, frame: InputFrame) -> bool {
        for button in self.iter() {
            if button.is_pressed(frame) {
                return true;
            }
        }

        false
    }
}

trait ReadAxis {
    fn read(&self, frame: InputFrame) -> f32;
}

impl ReadAxis for RealAnalogInput {
    fn read(&self, frame: InputFrame) -> f32 {
        match self {
            RealAnalogInput::Mouse { axis, scale } => match axis {
                MouseAxis::X => frame.mouse_movement.x * scale.into_inner(),
                MouseAxis::Y => frame.mouse_movement.y * scale.into_inner(),
            },
            RealAnalogInput::GamePad {
                axis: axis_type,
                scale,
            } => {
                let mut reading = 0.0;
                for gamepad in frame.gamepad_entities {
                    reading += gamepad.get(*axis_type).unwrap_or(0.0) * scale.into_inner();
                }

                reading * frame.time_delta
            }
        }
    }
}

impl ReadAxis for AnalogInput {
    fn read(&self, frame: InputFrame) -> f32 {
        match self {
            Self::RealAnalogInput { analog_input } => analog_input.read(frame),
            Self::Button { button, scale } => {
                if button.is_pressed(frame) {
                    scale.into_inner()
                } else {
                    0.0
                }
            }
        }
    }
}

impl ReadAxis for HashSet<AnalogInput> {
    fn read(&self, frame: InputFrame) -> f32 {
        let mut accumulated_input = 0.0;

        for axis in self.iter() {
            accumulated_input += axis.read(frame);
        }

        accumulated_input
    }
}

#[derive(PartialEq, Eq)]
pub enum ButtonState {
    JustPressed,
    Held,
    Released,
}

impl Default for ButtonState {
    fn default() -> Self {
        Self::Released
    }
}

impl ButtonState {
    pub fn is_pressed(&self) -> bool {
        matches!(self, Self::JustPressed | Self::Held)
    }

    pub fn just_pressed(&self) -> bool {
        matches!(self, Self::JustPressed)
    }
}

#[derive(Resource, Default)]
pub struct InputState {
    pub horizontal_movement: Vec2,
    pub look_movement: Vec2,
    pub jumping: ButtonState,
    pub crouching: ButtonState,
    pub primary_fire: ButtonState,
    pub secondary_fire: ButtonState,
}

fn initial_grab_cursor(mut cursors: Query<&mut bevy::window::CursorOptions, With<PrimaryWindow>>) {
    if let Ok(mut cursor) = cursors.single_mut() {
        cursor.grab_mode = CursorGrabMode::Confined;
        cursor.visible = false;
    }
}

fn release_cursor(mut cursors: Query<&mut bevy::window::CursorOptions, With<PrimaryWindow>>) {
    if let Ok(mut cursor) = cursors.single_mut() {
        cursor.grab_mode = CursorGrabMode::None;
        cursor.visible = true;
    }
}

#[derive(Clone, Copy)]
struct InputFrame<'a> {
    time_delta: f32,
    gamepad_entities: &'a Vec<&'a Gamepad>,
    mouse_movement: Vec2,
    gamepad_axes: &'a Axis<GamepadAxis>,
    keyboard_status: &'a ButtonInput<KeyCode>,
    mouse_status: &'a ButtonInput<MouseButton>,
}

#[allow(clippy::too_many_arguments)]
fn update_inputs(
    time: Res<Time>,
    mouse_movement: Res<AccumulatedMouseMotion>,
    gamepad_axes: Res<Axis<GamepadAxis>>,
    keyboard_status: Res<ButtonInput<KeyCode>>,
    mouse_status: Res<ButtonInput<MouseButton>>,
    gamepad_status: Res<ButtonInput<GamepadButton>>,
    windows: Query<(&Window, &bevy::window::CursorOptions), With<PrimaryWindow>>,
    gamepads: Query<&Gamepad>,
    input_map: Res<InputMap>,
    mut input_state: ResMut<InputState>,
) {
    let mouse_movement_input = if let Ok((window, cursor_options)) = windows.single() {
        let mut mouse_movement = Vec2::ZERO;
        match cursor_options.grab_mode {
            bevy::window::CursorGrabMode::Locked | bevy::window::CursorGrabMode::Confined => {
                let window_scale = window.resolution.scale_factor();

                mouse_movement += Vec2::new(
                    -mouse_movement.x * window_scale,
                    -mouse_movement.y * window_scale,
                ) * time.delta().as_secs_f32()
                    * 100.0;
            }
            bevy::window::CursorGrabMode::None => {}
        }

        mouse_movement
    } else {
        Vec2::ZERO
    };

    let gamepad_entities: Vec<&Gamepad> = gamepads.iter().collect();

    let frame = InputFrame {
        time_delta: 1.0, // Horizontal movement automatically has the time delta applied to it by the physics engine.
        gamepad_entities: &gamepad_entities,
        mouse_movement: mouse_movement_input,
        gamepad_axes: &gamepad_axes,
        keyboard_status: &keyboard_status,
        mouse_status: &mouse_status,
    };

    let horizontal_movement = input_map.horizontal_movement.read(frame);
    // We have to cull horizontal movement to have a norm of 1.0, otherwise multiple controllers can be used for a speed boost.
    let norm = horizontal_movement.length();
    let direction = horizontal_movement.normalize_or_zero();
    input_state.horizontal_movement = direction * norm.min(1.0);

    let frame = InputFrame {
        time_delta: time.delta_secs(),
        gamepad_entities: &gamepad_entities,
        mouse_movement: mouse_movement_input,
        gamepad_axes: &gamepad_axes,
        keyboard_status: &keyboard_status,
        mouse_status: &mouse_status,
    };

    input_state.look_movement = input_map.look_movement.read(frame);

    fn update_button_state(
        frame: InputFrame,
        button_state: &mut ButtonState,
        button_mapping: &HashSet<Button>,
    ) {
        *button_state = if button_mapping.is_pressed(frame) {
            if *button_state == ButtonState::Held || *button_state == ButtonState::JustPressed {
                ButtonState::Held
            } else {
                ButtonState::JustPressed
            }
        } else {
            ButtonState::Released
        };
    }

    update_button_state(frame, &mut input_state.jumping, &input_map.jumping);
    update_button_state(frame, &mut input_state.crouching, &input_map.crouching);
    update_button_state(
        frame,
        &mut input_state.primary_fire,
        &input_map.primary_fire,
    );
    update_button_state(
        frame,
        &mut input_state.secondary_fire,
        &input_map.secondary_fire,
    );
}

fn detect_gamepads(_gamepads: Query<&Gamepad>) {
    // I want to accept input from all controllers because my computer is weird and makes
    // controller 0 the LED driver built into my motherboard. I assume other computers will have issues like this.
    // TODO: restore when bevy_ui_navigation is available
}

fn update_ui_input_map(_input_map: Res<InputMap>) {
    // TODO: restore when bevy_ui_navigation is available
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct PlayerControlsPlugin;

impl Plugin for PlayerControlsPlugin {
    fn build(&self, app: &mut App) {
        fn setup(mut commands: Commands) {
            commands.insert_resource(InputState::default());
        }

        app.add_systems(Startup, setup.after(LoadConfigSet));

        app.configure_sets(Update, PlayerControlsPlugin);

        InputMap::app_setup(app);
        app.add_systems(
            OnEnter(WorldState::Running),
            initial_grab_cursor.in_set(PlayerControlsPlugin),
        );
        app.add_systems(
            OnExit(WorldState::Running),
            release_cursor.in_set(PlayerControlsPlugin),
        );
        app.add_systems(Update, update_ui_input_map.in_set(PlayerControlsPlugin));

        app.add_systems(
            Update,
            (
                detect_gamepads.in_set(PlayerControlsPlugin),
                update_inputs
                    .run_if(in_state(GameState::InGame))
                    .in_set(PlayerControlsPlugin),
            ),
        );
    }
}
