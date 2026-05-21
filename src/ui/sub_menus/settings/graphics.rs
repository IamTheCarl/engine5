use bevy::{
    prelude::*,
    window::{MonitorSelection, PrimaryWindow, WindowMode},
};
// TODO: restore bevy_ui_navigation
// use bevy_ui_navigation::menu::{MenuBuilder, MenuSetting, NavMarker};

use crate::{
    config::{graphics::GraphicsConfig, LoadConfigSet},
    ui::{
        // TODO: restore setup_submenu when bevy_ui_navigation is available
        // sub_menus::setup_submenu,
        widgets::{
            spawn_button, spawn_combo, spawn_numaric_input, BackButton, Combo, NumaricInput,
            RangeLimit,
        },
    },
};

#[derive(Component)]
struct GraphicsSettingsMenu;

#[derive(Component, Clone)]
struct GraphicsSettingsMenuMarker;

pub fn spawn(commands: &mut Commands, parent: Entity, graphics_config: &GraphicsConfig) {
    let menu_entity = commands
        .spawn((
            Node {
                display: Display::None,
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                align_items: AlignItems::Center,
                align_content: AlignContent::Center,
                justify_content: JustifyContent::Center,
                flex_direction: FlexDirection::Column,
                ..Default::default()
            },
            GraphicsSettingsMenu,
        ))
        .with_children(|commands| {
            commands.spawn((
                Text::new("Graphics Settings"),
                TextFont {
                    font_size: 60.0,
                    ..Default::default()
                },
                TextColor(Color::WHITE),
            ));
            commands.spawn(Node {
                height: Val::Percent(20.0),
                ..Default::default()
            });
        })
        .id();

    spawn_combo(
        commands,
        FullscreenModeSetting,
        "Fullscreen Mode",
        match graphics_config.start_in_fullscreen_mode {
            true => 1,
            false => 0,
        },
        ["disabled", "enabled"],
    )
    .insert(ChildOf(menu_entity));
    spawn_combo(
        commands,
        InitialFullscreenModeSetting,
        "Start in Fullscreen Mode",
        match graphics_config.start_in_fullscreen_mode {
            true => 1,
            false => 0,
        },
        ["no", "yes"],
    )
    .insert(ChildOf(menu_entity));
    spawn_numaric_input(
        commands,
        "View Distance",
        "chunks",
        false,
        NumaricInput::new::<2, 0>(graphics_config.chunk_view_radius as isize, 0).unwrap(),
    )
    .insert(ViewDistanceSetting)
    .insert(RangeLimit::new(NumaricInput::new::<2, 0>(8, 0).unwrap()..))
    .insert(ChildOf(menu_entity));
    spawn_button(commands, "Back", BackButton).insert(ChildOf(menu_entity));
}

#[derive(Component)]
struct FullscreenModeSetting;

fn fullscreen_toggle(
    keys: Res<ButtonInput<KeyCode>>,
    mut windows: Query<&mut Window, With<PrimaryWindow>>,
    mut fullscreen_combos: Query<&mut Combo, With<FullscreenModeSetting>>,
) {
    if let Ok(mut window) = windows.single_mut() {
        if keys.just_pressed(KeyCode::F11) {
            let go_fullscreen = matches!(window.mode, WindowMode::Windowed);

            for mut combo in fullscreen_combos.iter_mut() {
                combo.selection = if go_fullscreen { 1 } else { 0 };
            }

            window.mode = if go_fullscreen {
                WindowMode::BorderlessFullscreen(MonitorSelection::Current)
            } else {
                WindowMode::Windowed
            };
        }
    }
}

#[derive(Component)]
struct InitialFullscreenModeSetting;

fn set_initial_fullscreen_mode(
    graphics_config: Res<GraphicsConfig>,
    mut windows: Query<&mut Window, With<PrimaryWindow>>,
) {
    if let Ok(mut window) = windows.single_mut() {
        window.mode = if graphics_config.start_in_fullscreen_mode {
            WindowMode::BorderlessFullscreen(MonitorSelection::Current)
        } else {
            WindowMode::Windowed
        };
    }
}

fn initalize_fullscreen_combo(
    mut combos: Query<&mut Combo, Added<FullscreenModeSetting>>,
    windows: Query<&Window, With<PrimaryWindow>>,
) {
    if let Some(window) = windows.iter().next() {
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
    if let Some(mut window) = windows.iter_mut().next() {
        if let Some(combo) = combos.iter().next() {
            window.mode = match combo.selection {
                1 => WindowMode::BorderlessFullscreen(MonitorSelection::Current),
                _ => WindowMode::Windowed,
            };
        }
    }
}

fn initial_fullscreen_combo_selection_handler(
    combos: Query<&Combo, (Changed<Combo>, With<InitialFullscreenModeSetting>)>,
    mut graphics_config: ResMut<GraphicsConfig>,
) {
    if let Some(combo) = combos.iter().next() {
        graphics_config.start_in_fullscreen_mode = combo.selection == 1;
    }
}

#[derive(Component)]
struct ViewDistanceSetting;

fn update_view_distance_setting(
    numaric_input: Query<&NumaricInput, (Changed<NumaricInput>, With<ViewDistanceSetting>)>,
    mut graphics_config: ResMut<GraphicsConfig>,
) {
    if let Some(numaric_input) = numaric_input.iter().next() {
        graphics_config.chunk_view_radius = numaric_input.as_unsigned();
    }
}

pub fn setup(app: &mut App) {
    app.add_systems(Startup, set_initial_fullscreen_mode.after(LoadConfigSet));

    app.add_systems(
        Update,
        (
            fullscreen_toggle,
            initalize_fullscreen_combo,
            fullscreen_combo_selection_handler,
            initial_fullscreen_combo_selection_handler,
            update_view_distance_setting,
        ),
    );
}
