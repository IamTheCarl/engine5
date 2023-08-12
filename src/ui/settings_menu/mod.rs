use bevy::prelude::*;
use bevy_ui_navigation::menu::{MenuBuilder, MenuSetting, NavMarker};

use super::{setup_submenu, spawn_button, BackButton};

mod graphics;

#[derive(Component)]
struct SettingsMenu;

#[derive(Component, Clone)]
struct SettingsMenuMarker;

#[derive(Component)]
struct GraphicsButton;

#[derive(Component)]
struct ControlsButton;

pub fn spawn(mut commands: Commands, parent: Entity) {
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
            SettingsMenu,
            NavMarker(SettingsMenuMarker),
        ))
        .with_children(|commands| {
            commands.spawn(TextBundle::from_section(
                "Settings",
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

    spawn_button(&mut commands, "Graphics", GraphicsButton).set_parent(menu_entity);
    spawn_button(&mut commands, "Controls", ControlsButton).set_parent(menu_entity);
    spawn_button(&mut commands, "Back", BackButton).set_parent(menu_entity);
}

pub fn setup(app: &mut App) {
    setup_submenu::<SettingsMenu, SettingsMenuMarker>(app);
}
