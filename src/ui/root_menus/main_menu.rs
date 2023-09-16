use std::path::Path;

use crate::{
    config::{file_paths, graphics::GraphicsConfig},
    error_handler,
    ui::{
        sub_menus::{settings, setup_submenu},
        widgets::{spawn_button, spawn_prioritized_button},
    },
    world::{self, terrain::BlockRegistry, WorldState},
    GameState,
};
use anyhow::Result;
use bevy::prelude::*;
use bevy_ui_navigation::{
    menu::{MenuBuilder, MenuSetting, NavMarker},
    prelude::{NavEvent, NavRequest},
    NavRequestSystem,
};

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
    commands.spawn((Camera2dBundle::default(), MenuSetting::default()));

    let menu_entity = commands
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
            MenuSetting::new(),
            MenuBuilder::Root,
            NavMarker(MainMenuMarker),
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
        })
        .id();

    spawn_prioritized_button(&mut commands, "Single Player", SinglePlayerButton)
        .set_parent(menu_entity);
    let settings_button_entity = spawn_button(&mut commands, "Settings", SettingsButton)
        .set_parent(menu_entity)
        .id();
    spawn_button(&mut commands, "Quit", QuitButton).set_parent(menu_entity);

    settings::spawn(&mut commands, settings_button_entity, &graphics_config);
}

fn despawn(mut commands: Commands, main_menu: Query<Entity, With<MenuSetting>>) {
    for menu in main_menu.iter() {
        if let Some(menu) = commands.get_entity(menu) {
            menu.despawn_recursive();
        }
    }
}

fn handle_selections(
    mut commands: Commands,
    mut events: EventReader<NavEvent>,
    mut next_app_state: ResMut<NextState<GameState>>,
    mut next_world_state: ResMut<NextState<WorldState>>,

    block_registry: Res<BlockRegistry>,
    single_player_buttons: Query<(), With<SinglePlayerButton>>,
    quit_buttons: Query<(), With<QuitButton>>,
) -> Result<()> {
    for event in events.iter() {
        if let NavEvent::NoChanges { from, request } = event {
            if matches!(request, NavRequest::Action) {
                if quit_buttons.contains(*from.first()) {
                    next_app_state.set(GameState::ShuttingDown);
                }

                if single_player_buttons.contains(*from.first()) {
                    world::raw_open_file_backed_world(
                        &mut commands,
                        &block_registry,
                        Path::new(file_paths::SAVE_DIRECTORY).join("test"),
                    )?;

                    next_app_state.set(GameState::InGame);
                    next_world_state.set(WorldState::Running);
                }
            }
        }
    }

    Ok(())
}

pub fn setup(app: &mut App) {
    app.add_systems(OnEnter(GameState::MainMenu), spawn);
    app.add_systems(OnExit(GameState::MainMenu), despawn);

    setup_submenu::<MainMenu, MainMenuMarker>(app);
    app.add_systems(
        Update,
        (handle_selections
            .pipe(error_handler)
            .after(NavRequestSystem)
            .run_if(in_state(GameState::MainMenu)),),
    );
}
