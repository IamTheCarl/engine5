use std::path::Path;

use bevy::prelude::*;
use bevy_ui_navigation::{
    menu::{MenuBuilder, MenuSetting, NavMarker},
    prelude::{NavEvent, NavRequest},
};

use super::setup_submenu;

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
            ControlsMenu,
            NavMarker(ControlsMenuMarker),
        ))
        .with_children(|commands| {
            commands.spawn(TextBundle::from_section(
                "Controls",
                TextStyle {
                    font_size: 60.0,
                    color: Color::WHITE,
                    ..Default::default()
                },
            ));

            const APOLOGY_LETTER: &str = include_str!("apology_letter.txt");

            commands
                .spawn(NodeBundle {
                    style: Style {
                        width: Val::Percent(60.0),
                        ..Default::default()
                    },
                    ..Default::default()
                })
                .with_children(|parent| {
                    parent.spawn(TextBundle::from_section(
                        APOLOGY_LETTER,
                        TextStyle {
                            font_size: 30.0,
                            color: Color::WHITE,
                            ..Default::default()
                        },
                    ));
                });
        })
        .id();

    spawn_button(commands, "Open Config File", OpenButton).set_parent(menu_entity);
    spawn_button(commands, "Reload Config File", ReloadButton).set_parent(menu_entity);
    spawn_button(commands, "Back", BackButton).set_parent(menu_entity);
}

fn process_open_button(
    mut events: EventReader<NavEvent>,
    open_buttons: Query<(), With<OpenButton>>,
) {
    for event in events.iter() {
        if let NavEvent::NoChanges { from, request } = event {
            if matches!(request, NavRequest::Action) && open_buttons.contains(*from.first()) {
                let config_file_path = Path::new(crate::config::file_paths::CONFIG_DIRECTORY)
                    .join(crate::config::controls::InputMap::CONFIG_FILE);

                let config_file_path = if let Ok(config_file_path) = config_file_path.canonicalize()
                {
                    config_file_path
                } else {
                    config_file_path
                };

                if let Err(error) = open::that(config_file_path) {
                    log::error!("Failed to open controls config file: {:?}", error);
                }
            }
        }
    }
}

fn process_reload_button(
    mut commands: Commands,
    mut events: EventReader<NavEvent>,
    reload_buttons: Query<(), With<ReloadButton>>,
) {
    for event in events.iter() {
        if let NavEvent::NoChanges { from, request } = event {
            if matches!(request, NavRequest::Action) && reload_buttons.contains(*from.first()) {
                commands.insert_resource(crate::config::controls::InputMap::load_or_default());
            }
        }
    }
}

pub fn setup(app: &mut App) {
    setup_submenu::<ControlsMenu, ControlsMenuMarker>(app);

    app.add_systems(Update, (process_open_button, process_reload_button));
}
