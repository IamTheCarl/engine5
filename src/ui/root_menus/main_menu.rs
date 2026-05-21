use std::path::Path;

use crate::{
    config::{file_paths, graphics::GraphicsConfig, player_info::PlayerInfo},
    error_handler,
    ui::{
        // TODO: restore setup_submenu when bevy_ui_navigation is available
        // sub_menus::{setup_submenu},
        sub_menus::settings,
        widgets::{spawn_button, spawn_prioritized_button},
    },
    world::{self, terrain::BlockRegistry, WorldState},
    GameState,
};
use anyhow::Result;
use bevy::prelude::*;
// TODO: restore bevy_ui_navigation
// use bevy_ui_navigation::{
//     menu::{MenuBuilder, MenuSetting, NavMarker},
//     prelude::{NavEvent, NavRequest},
//     NavRequestSystem,
// };

#[derive(Component)]
struct MainMenu;

#[derive(Component, Clone)]
struct MainMenuMarker;

#[derive(Component)]
struct SinglePlayerButton;

#[derive(Component)]
struct SettingsButton;

#[derive(Component)]
struct QuitButton;

fn spawn(mut commands: Commands, graphics_config: Res<GraphicsConfig>) {
    commands.spawn(Camera2d::default());

    let menu_entity = commands
        .spawn((
            Node {
                display: Display::Flex,
                width: Val::Percent(100.0),
                height: Val::Percent(100.0),
                align_items: AlignItems::Center,
                align_content: AlignContent::Center,
                justify_content: JustifyContent::Center,
                flex_direction: FlexDirection::Column,
                ..Default::default()
            },
            MainMenu,
        ))
        .with_children(|commands| {
            commands.spawn((
                Text::new("Engine 5"),
                TextFont {
                    font_size: 60.0,
                    ..Default::default()
                },
                TextColor(Color::WHITE),
            ));
            commands.spawn((
                Text::new("A creation of The Carl"),
                TextFont {
                    font_size: 15.0,
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

    spawn_prioritized_button(&mut commands, "Single Player", SinglePlayerButton)
        .insert(ChildOf(menu_entity));
    let settings_button_entity = spawn_button(&mut commands, "Settings", SettingsButton)
        .insert(ChildOf(menu_entity))
        .id();
    spawn_button(&mut commands, "Quit", QuitButton).insert(ChildOf(menu_entity));

    settings::spawn(&mut commands, settings_button_entity, &graphics_config);
}

fn despawn(mut commands: Commands, main_menu: Query<Entity, With<MainMenu>>) {
    for menu in main_menu.iter() {
        commands.entity(menu).despawn();
    }
}

// TODO: restore bevy_ui_navigation event handler
// fn handle_selections(
//     mut commands: Commands,
//     mut events: EventReader<NavEvent>,
//     mut next_app_state: ResMut<NextState<GameState>>,
//     mut next_world_state: ResMut<NextState<WorldState>>,
//     player_info: Res<PlayerInfo>,
//     block_registry: Res<BlockRegistry>,
//     single_player_buttons: Query<(), With<SinglePlayerButton>>,
//     quit_buttons: Query<(), With<QuitButton>>,
// ) -> Result<()> {
//     for event in events.iter() {
//         if let NavEvent::NoChanges { from, request } = event {
//             if matches!(request, NavRequest::Action) {
//                 if quit_buttons.contains(*from.first()) {
//                     next_app_state.set(GameState::ShuttingDown);
//                 }
//                 if single_player_buttons.contains(*from.first()) {
//                     world::raw_open_file_backed_world(
//                         &mut commands,
//                         Some(&player_info),
//                         &block_registry,
//                         Path::new(file_paths::SAVE_DIRECTORY).join("test"),
//                     )?;
//                     next_app_state.set(GameState::InGame);
//                     next_world_state.set(WorldState::Running);
//                 }
//             }
//         }
//     }
//     Ok(())
// }

pub fn setup(app: &mut App) {
    app.add_systems(OnEnter(GameState::MainMenu), spawn);
    app.add_systems(OnExit(GameState::MainMenu), despawn);
}
