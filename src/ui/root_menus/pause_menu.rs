use crate::{
    config::graphics::GraphicsConfig,
    error_handler,
    ui::{
        sub_menus::settings,
        // TODO: restore setup_submenu when bevy_ui_navigation is available
        // sub_menus::setup_submenu,
        widgets::{spawn_button, spawn_prioritized_button},
    },
    world::WorldState,
    GameState,
};

use anyhow::Result;
use bevy::prelude::*;
// TODO: restore bevy_ui_navigation
// use bevy_ui_navigation::{
//     menu::NavMarker,
//     prelude::{MenuBuilder, MenuSetting, NavEvent, NavRequest},
//     NavRequestSystem,
// };

#[derive(Component)]
struct PauseMenu;

#[derive(Component, Clone)]
struct PauseMenuMarker;

#[derive(Component)]
struct ReturnToGameButton;

#[derive(Component)]
struct SettingsButton;

#[derive(Component)]
struct QuitToMainMenuButton;

#[derive(Component)]
struct QuitToDesktopButton;

fn spawn(mut commands: Commands, graphics_config: Res<GraphicsConfig>) {
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
            PauseMenu,
        ))
        .with_children(|commands| {
            commands.spawn((
                Text::new("Paused"),
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

    spawn_prioritized_button(&mut commands, "Return to game", ReturnToGameButton)
        .insert(ChildOf(menu_entity));
    let settings_button_entity = spawn_button(&mut commands, "Settings", SettingsButton)
        .insert(ChildOf(menu_entity))
        .id();
    spawn_button(&mut commands, "Quit to main menu", QuitToMainMenuButton)
        .insert(ChildOf(menu_entity));
    spawn_button(&mut commands, "Quit to desktop", QuitToDesktopButton)
        .insert(ChildOf(menu_entity));

    settings::spawn(&mut commands, settings_button_entity, &graphics_config);
}

fn despawn(mut commands: Commands, pause_menu: Query<Entity, With<PauseMenu>>) {
    for menu in pause_menu.iter() {
        commands.entity(menu).despawn();
    }
}

// TODO: restore bevy_ui_navigation event handler
// fn handle_selections(
//     mut events: EventReader<NavEvent>,
//     mut next_app_state: ResMut<NextState<GameState>>,
//     mut next_world_state: ResMut<NextState<WorldState>>,
//     return_to_game_buttons: Query<(), With<ReturnToGameButton>>,
//     quit_to_main_menu_buttons: Query<(), With<QuitToMainMenuButton>>,
//     quit_to_desktop_buttons: Query<(), With<QuitToDesktopButton>>,
// ) -> Result<()> {
//     for event in events.iter() {
//         if let NavEvent::NoChanges { from, request } = event {
//             if matches!(request, NavRequest::Action) {
//                 if quit_to_desktop_buttons.contains(*from.first()) {
//                     next_app_state.set(GameState::ShuttingDown);
//                     next_world_state.set(WorldState::Unloaded);
//                 }
//                 if quit_to_main_menu_buttons.contains(*from.first()) {
//                     next_app_state.set(GameState::MainMenu);
//                     next_world_state.set(WorldState::Unloaded);
//                 }
//                 if return_to_game_buttons.contains(*from.first()) {
//                     next_world_state.set(WorldState::Running);
//                 }
//             }
//         }
//     }
//     Ok(())
// }

fn enter_pause_state(
    keys: Res<ButtonInput<KeyCode>>,
    mut next_world_state: ResMut<NextState<WorldState>>,
) {
    if keys.just_pressed(KeyCode::Escape) {
        next_world_state.set(WorldState::Paused);
    }
}

// TODO: restore bevy_ui_navigation escape handler
// fn exit_pause_state(
//     mut events: EventReader<NavEvent>,
//     mut next_world_state: ResMut<NextState<WorldState>>,
//     menu_items: Query<(), With<PauseMenuMarker>>,
// ) {
//     for event in events.iter() {
//         if let NavEvent::NoChanges { from, request } = event {
//             if matches!(request, NavRequest::Cancel) && menu_items.contains(*from.first()) {
//                 next_world_state.set(WorldState::Running);
//             }
//         }
//     }
// }

pub fn setup(app: &mut App) {
    app.add_systems(OnEnter(WorldState::Paused), spawn);
    app.add_systems(OnExit(WorldState::Paused), despawn);
    app.add_systems(
        Update,
        enter_pause_state.run_if(in_state(WorldState::Running)),
    );
}
