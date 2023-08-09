use crate::AppState;

use super::spawn_button;
use bevy::prelude::*;
use bevy_ui_navigation::{
    prelude::{NavEvent, NavRequest},
    NavRequestSystem,
};

#[derive(Component)]
struct MainMenu;

#[derive(Component)]
struct SinglePlayerButton;

#[derive(Component)]
struct SettingsButton;

#[derive(Component)]
struct QuitButton;

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
            MainMenu,
        ))
        .with_children(|commands| {
            commands.spawn(TextBundle::from_section(
                "Engine 5",
                TextStyle {
                    font_size: 60.0,
                    color: Color::WHITE,
                    ..Default::default()
                },
            ));
            commands.spawn(TextBundle::from_section(
                "A creation of The Carl",
                TextStyle {
                    font_size: 15.0,
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

            spawn_button(commands, "Single Player", SinglePlayerButton);
            spawn_button(commands, "Settings", SettingsButton);
            spawn_button(commands, "Quit", QuitButton);
        });
}

fn despawn(mut commands: Commands, main_menu: Query<Entity, With<MainMenu>>) {
    for menu in main_menu.iter() {
        if let Some(menu) = commands.get_entity(menu) {
            menu.despawn_recursive();
        }
    }
}

fn handle_selections(
    mut events: EventReader<NavEvent>,
    mut next_state: ResMut<NextState<AppState>>,
    quit_buttons: Query<(), With<QuitButton>>,
) {
    for event in events.iter() {
        if let NavEvent::NoChanges { from, request } = event {
            if matches!(request, NavRequest::Action) && quit_buttons.contains(*from.first()) {
                next_state.set(AppState::ShuttingDown);
            }
        }
    }
}

pub fn setup(app: &mut App) {
    app.add_systems(OnEnter(AppState::MainMenu), spawn);
    app.add_systems(OnExit(AppState::MainMenu), despawn);
    app.add_systems(
        Update,
        handle_selections
            .after(NavRequestSystem)
            .run_if(in_state(AppState::MainMenu)),
    );
}
