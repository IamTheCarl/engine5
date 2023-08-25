use bevy::{
    prelude::*,
    window::{PrimaryWindow, WindowMode},
};
use bevy_ui_navigation::menu::{MenuBuilder, MenuSetting, NavMarker};

use crate::ui::{
    setup_submenu,
    widgets::{
        spawn_button, spawn_combo, spawn_numaric_input, BackButton, Combo, NumaricInput, RangeLimit,
    },
};

#[derive(Component)]
struct GraphicsSettingsMenu;

#[derive(Component, Clone)]
struct GraphicsSettingsMenuMarker;

pub fn spawn(commands: &mut Commands, parent: Entity) {
    let menu_entity = commands
        .spawn((
            NodeBundle {
                style: Style {
                    display: Display::None,
                    width: Val::Percent(100.0),
                    height: Val::Percent(100.0),
                    align_items: AlignItems::Center,
                    align_content: AlignContent::Center,
                    justify_content: JustifyContent::Center,
                    flex_direction: FlexDirection::Column,
                    ..Default::default()
                },
                ..Default::default()
            },
            MenuSetting::new(),
            MenuBuilder::EntityParent(parent),
            GraphicsSettingsMenu,
            NavMarker(GraphicsSettingsMenuMarker),
        ))
        .with_children(|commands| {
            commands.spawn(TextBundle::from_section(
                "Graphics Settings",
                TextStyle {
                    font_size: 60.0,
                    color: Color::WHITE,
                    ..Default::default()
                },
            ));
            commands.spawn(NodeBundle {
                style: Style {
                    height: Val::Percent(20.0),
                    ..Default::default()
                },
                ..Default::default()
            });
        })
        .id();

    spawn_combo(
        commands,
        FullscreenModeSetting,
        "Fullscreen Mode",
        1,
        ["disabled", "enabled"],
    )
    .set_parent(menu_entity);
    spawn_combo(commands, (), "Start in Fullscreen Mode", 0, ["no", "yes"]).set_parent(menu_entity);
    spawn_numaric_input(
        commands,
        "View Distance",
        "chunks",
        false,
        NumaricInput::new::<2, 0>(10, 0).unwrap(),
    )
    .insert(RangeLimit::new(NumaricInput::new::<2, 0>(8, 0).unwrap()..))
    .set_parent(menu_entity);
    spawn_button(commands, "Back", BackButton).set_parent(menu_entity);
}

#[derive(Component)]
struct FullscreenModeSetting;

fn fullscreen_toggle(
    keys: Res<Input<KeyCode>>,
    mut windows: Query<&mut Window, With<PrimaryWindow>>,
    mut fullscreen_combos: Query<&mut Combo, With<FullscreenModeSetting>>,
) {
    if let Ok(mut window) = windows.get_single_mut() {
        if keys.just_pressed(KeyCode::F11) {
            let go_fullscreen = matches!(window.mode, WindowMode::Windowed);

            for mut combo in fullscreen_combos.iter_mut() {
                combo.selection = if go_fullscreen { 1 } else { 0 };
            }

            window.mode = if go_fullscreen {
                WindowMode::Fullscreen
            } else {
                WindowMode::Windowed
            };
        }
    }
}

fn initalize_fullscreen_combo(
    mut combos: Query<&mut Combo, Added<FullscreenModeSetting>>,
    windows: Query<&Window, With<PrimaryWindow>>,
) {
    if let Ok(window) = windows.get_single() {
        for mut combo in combos.iter_mut() {
            combo.selection = if matches!(window.mode, WindowMode::Windowed) {
                0
            } else {
                1
            };
        }
    }
}

fn fullscreen_combo_selection_handler(
    combos: Query<&Combo, (Changed<Combo>, With<FullscreenModeSetting>)>,
    mut windows: Query<&mut Window, With<PrimaryWindow>>,
) {
    if let Ok(mut window) = windows.get_single_mut() {
        if let Ok(combo) = combos.get_single() {
            window.mode = match combo.selection {
                1 => WindowMode::Fullscreen,
                _ => WindowMode::Windowed,
            };
        }
    }
}

pub fn setup(app: &mut App) {
    setup_submenu::<GraphicsSettingsMenu, GraphicsSettingsMenuMarker>(app);

    app.add_systems(
        Update,
        (
            fullscreen_toggle,
            initalize_fullscreen_combo,
            fullscreen_combo_selection_handler,
        ),
    );
}
