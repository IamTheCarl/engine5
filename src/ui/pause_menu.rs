use crate::{error_handler, world::WorldState, AppState};

use super::spawn_button;
use anyhow::Result;
use bevy::prelude::*;
use bevy_ui_navigation::{
    prelude::{NavEvent, NavRequest},
    NavRequestSystem,
};

#[derive(Component)]
struct PauseMenu;

#[derive(Component)]
struct ReturnToGameButton;

#[derive(Component)]
struct SettingsButton;

#[derive(Component)]
struct QuitToMainMenuButton;

#[derive(Component)]
struct QuitToDesktopButton;

fn spawn(mut commands: Commands) {
    commands
        .spawn((
            NodeBundle {
                style: Style {
                    display: Display::Flex,
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
            PauseMenu,
        ))
        .with_children(|commands| {
            commands.spawn(TextBundle::from_section(
                "Paused",
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

            spawn_button(commands, "Return to game", ReturnToGameButton);
            spawn_button(commands, "Settings", SettingsButton);
            spawn_button(commands, "Quit to main menu", QuitToMainMenuButton);
            spawn_button(commands, "Quit to desktop", QuitToDesktopButton);
        });
}

fn despawn(mut commands: Commands, main_menu: Query<Entity, With<PauseMenu>>) {
    for menu in main_menu.iter() {
        if let Some(menu) = commands.get_entity(menu) {
            menu.despawn_recursive();
        }
    }
}

fn handle_selections(
    mut events: EventReader<NavEvent>,
    mut next_app_state: ResMut<NextState<AppState>>,
    mut next_world_state: ResMut<NextState<WorldState>>,

    return_to_game_buttons: Query<(), With<ReturnToGameButton>>,
    quit_to_main_menu_buttons: Query<(), With<QuitToMainMenuButton>>,
    quit_to_desktop_buttons: Query<(), With<QuitToDesktopButton>>,
) -> Result<()> {
    for event in events.iter() {
        if let NavEvent::NoChanges { from, request } = event {
            if matches!(request, NavRequest::Action) {
                if quit_to_desktop_buttons.contains(*from.first()) {
                    next_app_state.set(AppState::ShuttingDown);
                    next_world_state.set(WorldState::Unloaded);
                }

                if quit_to_main_menu_buttons.contains(*from.first()) {
                    next_app_state.set(AppState::MainMenu);
                    next_world_state.set(WorldState::Unloaded);
                }

                if return_to_game_buttons.contains(*from.first()) {
                    next_world_state.set(WorldState::Running);
                }
            }
        }
    }

    Ok(())
}

fn enter_pause_state(
    keys: Res<Input<KeyCode>>,
    mut next_world_state: ResMut<NextState<WorldState>>,
) {
    if keys.just_pressed(KeyCode::Escape) {
        next_world_state.set(WorldState::Paused);
    }
}

fn exit_pause_state(
    keys: Res<Input<KeyCode>>,
    mut next_world_state: ResMut<NextState<WorldState>>,
) {
    if keys.just_pressed(KeyCode::Escape) {
        next_world_state.set(WorldState::Running);
    }
}

pub fn setup(app: &mut App) {
    app.add_systems(OnEnter(WorldState::Paused), spawn);
    app.add_systems(OnExit(WorldState::Paused), despawn);
    app.add_systems(
        Update,
        handle_selections
            .pipe(error_handler)
            .after(NavRequestSystem)
            .run_if(in_state(WorldState::Paused)),
    );
    app.add_systems(
        Update,
        (
            enter_pause_state.run_if(in_state(WorldState::Running)),
            exit_pause_state.run_if(in_state(WorldState::Paused)),
        ),
    );
}
