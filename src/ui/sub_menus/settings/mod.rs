use bevy::prelude::*;
// TODO: restore bevy_ui_navigation
// use bevy_ui_navigation::menu::{MenuBuilder, MenuSetting, NavMarker};

use crate::{
    config::graphics::GraphicsConfig,
    ui::widgets::{spawn_button, BackButton},
};

// TODO: restore bevy_ui_navigation
// use super::setup_submenu;

mod controls;
mod graphics;

#[derive(Component)]
struct SettingsMenu;

#[derive(Component, Clone)]
struct SettingsMenuMarker;

#[derive(Component)]
struct GraphicsButton;

#[derive(Component)]
struct ControlsButton;

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
            SettingsMenu,
        ))
        .with_children(|commands| {
            commands.spawn((
                Text::new("Settings"),
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

    let graphics_button_entity = spawn_button(commands, "Graphics", GraphicsButton)
        .insert(ChildOf(menu_entity))
        .id();
    let controls_button_entity = spawn_button(commands, "Controls", ControlsButton)
        .insert(ChildOf(menu_entity))
        .id();

    spawn_button(commands, "Back", BackButton).insert(ChildOf(menu_entity));

    graphics::spawn(commands, graphics_button_entity, graphics_config);
    controls::spawn(commands, controls_button_entity);
}

pub fn setup(app: &mut App) {
    graphics::setup(app);
    controls::setup(app);
}
