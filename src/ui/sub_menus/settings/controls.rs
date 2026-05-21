use std::path::Path;

use bevy::prelude::*;
// TODO: restore bevy_ui_navigation
// use bevy_ui_navigation::{
//     menu::{MenuBuilder, MenuSetting, NavMarker},
//     prelude::{NavEvent, NavRequest},
// };

// TODO: restore setup_submenu when bevy_ui_navigation is available
// use super::setup_submenu;

use crate::{
    config::Config,
    ui::widgets::{spawn_button, BackButton},
};

#[derive(Component)]
struct ControlsMenu;

#[derive(Component, Clone)]
struct ControlsMenuMarker;

#[derive(Component)]
struct OpenButton;

#[derive(Component)]
struct ReloadButton;

pub fn spawn(commands: &mut Commands, parent: Entity) {
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
            ControlsMenu,
        ))
        .with_children(|commands| {
            commands.spawn((
                Text::new("Controls"),
                TextFont {
                    font_size: 60.0,
                    ..Default::default()
                },
                TextColor(Color::WHITE),
            ));

            const APOLOGY_LETTER: &str = include_str!("apology_letter.txt");

            commands
                .spawn(Node {
                    width: Val::Percent(60.0),
                    ..Default::default()
                })
                .with_children(|parent| {
                    parent.spawn((
                        Text::new(APOLOGY_LETTER),
                        TextFont {
                            font_size: 30.0,
                            ..Default::default()
                        },
                        TextColor(Color::WHITE),
                    ));
                });
        })
        .id();

    spawn_button(commands, "Open Config File", OpenButton).insert(ChildOf(menu_entity));
    spawn_button(commands, "Reload Config File", ReloadButton).insert(ChildOf(menu_entity));
    spawn_button(commands, "Back", BackButton).insert(ChildOf(menu_entity));
}

// TODO: restore bevy_ui_navigation event handlers
// fn process_open_button(
//     mut events: EventReader<NavEvent>,
//     open_buttons: Query<(), With<OpenButton>>,
// ) {
//     for event in events.iter() {
//         if let NavEvent::NoChanges { from, request } = event {
//             if matches!(request, NavRequest::Action) && open_buttons.contains(*from.first()) {
//                 let config_file_path = Path::new(crate::config::file_paths::CONFIG_DIRECTORY)
//                     .join(crate::config::controls::InputMap::CONFIG_FILE);
//                 let config_file_path = if let Ok(config_file_path) = config_file_path.canonicalize() {
//                     config_file_path
//                 } else {
//                     config_file_path
//                 };
//                 if let Err(error) = open::that(config_file_path) {
//                     log::error!("Failed to open controls config file: {:?}", error);
//                 }
//             }
//         }
//     }
// }

// fn process_reload_button(
//     mut commands: Commands,
//     mut events: EventReader<NavEvent>,
//     reload_buttons: Query<(), With<ReloadButton>>,
// ) {
//     for event in events.iter() {
//         if let NavEvent::NoChanges { from, request } = event {
//             if matches!(request, NavRequest::Action) && reload_buttons.contains(*from.first()) {
//                 commands.insert_resource(crate::config::controls::InputMap::load_or_default());
//             }
//         }
//     }
// }

pub fn setup(_app: &mut App) {}
