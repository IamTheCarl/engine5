use crate::AppState;

use super::{spawn_button, Clipboard};
use bevy::prelude::*;
use bevy_ui_navigation::{
    prelude::{NavEvent, NavRequest},
    NavRequestSystem,
};

#[derive(Resource)]
pub struct ErrorContext {
    pub error: anyhow::Error,
}

#[derive(Component)]
struct FatalErrorDisplay;

#[derive(Component)]
struct QuitButton;

#[derive(Component)]
struct CopyButton;

fn extract_error_context(error_context: &Option<Res<ErrorContext>>) -> String {
    error_context
        .as_ref()
        .map_or(String::from("No error context was provided."), |context| {
            format!("{:?}", context.error)
        })
}

fn spawn(mut commands: Commands, error_context: Option<Res<ErrorContext>>) {
    let error_text = extract_error_context(&error_context);
    log::error!("Fatal Error: {:?}", error_text);

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
            FatalErrorDisplay,
        ))
        .with_children(|commands| {
            commands.spawn(TextBundle::from_section(
                "Fatal Error",
                TextStyle {
                    font_size: 60.0,
                    color: Color::WHITE,
                    ..Default::default()
                },
            ));
            commands
                .spawn(NodeBundle {
                    style: Style {
                        display: Display::Flex,
                        flex_direction: FlexDirection::Column,
                        width: Val::Percent(60.0),
                        ..Default::default()
                    },
                    background_color: Color::DARK_GRAY.into(),
                    ..Default::default()
                })
                .with_children(|commands| {
                    commands.spawn(TextBundle::from_section(
                        "I'm sorry, but the game has crashed.",
                        TextStyle {
                            font_size: 30.0,
                            color: Color::WHITE,
                            ..Default::default()
                        },
                    ));
                    commands.spawn(TextBundle::from_section(
                        error_text,
                        TextStyle {
                            font_size: 20.0,
                            color: Color::WHITE,
                            ..Default::default()
                        },
                    ));
                });

            commands.spawn(NodeBundle {
                style: Style {
                    height: Val::Percent(20.0),
                    ..Default::default()
                },
                ..Default::default()
            });

            spawn_button(commands, "Quit", QuitButton);
            spawn_button(commands, "Copy error to Clipboard", CopyButton);
        });
}

fn despawn(mut commands: Commands, main_menu: Query<Entity, With<FatalErrorDisplay>>) {
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
    copy_buttons: Query<(), With<CopyButton>>,
    error_context: Option<Res<ErrorContext>>,
    mut clipboard: Option<ResMut<Clipboard>>,
) {
    for event in events.iter() {
        if let NavEvent::NoChanges { from, request } = event {
            if matches!(request, NavRequest::Action) {
                if quit_buttons.contains(*from.first()) {
                    next_state.set(AppState::ShuttingDown);
                }

                if copy_buttons.contains(*from.first()) {
                    // TODO we need some kind of on-screen conformation of this event.
                    match clipboard.as_mut() {
                        Some(clipboard) => {
                            match clipboard.set_contents(extract_error_context(&error_context)) {
                                Ok(()) => {
                                    log::info!("Copied error message to clipboard.");

                                    // Linux requires this, for some reason...
                                    // https://github.com/alacritty/copypasta/issues/49
                                    clipboard.get_contents().ok();
                                }
                                Err(error) => {
                                    log::error!(
                                        "Failed to copy error message to system clipboard: {:?}",
                                        error
                                    );
                                }
                            }
                        }
                        None => {
                            log::error!("Clipboard was unavailable when trying to copy error message to it.");
                        }
                    }
                }
            }
        }
    }
}

pub fn setup(app: &mut App) {
    app.add_systems(OnEnter(AppState::FatalError), spawn);
    app.add_systems(OnExit(AppState::FatalError), despawn);
    app.add_systems(
        Update,
        handle_selections
            .after(NavRequestSystem)
            .run_if(in_state(AppState::FatalError)),
    );
}
