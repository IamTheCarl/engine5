use crate::{
    ui::{
        widgets::{spawn_button, spawn_prioritized_button},
        Clipboard,
    },
    GameState,
};

use bevy::prelude::*;
// TODO: restore bevy_ui_navigation
// use bevy_ui_navigation::{
//     prelude::{NavEvent, NavRequest},
//     NavRequestSystem,
// };

#[derive(Resource)]
pub struct ErrorContext {
    pub error: String,
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

    commands.spawn((Camera2d::default(), FatalErrorDisplay));

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
            FatalErrorDisplay,
        ))
        .with_children(|commands| {
            commands.spawn((
                Text::new("Fatal Error"),
                TextFont {
                    font_size: 60.0,
                    ..Default::default()
                },
                TextColor(Color::WHITE),
            ));
            commands
                .spawn((
                    Node {
                        display: Display::Flex,
                        flex_direction: FlexDirection::Column,
                        width: Val::Percent(60.0),
                        ..Default::default()
                    },
                    BackgroundColor(Color::srgba(0.25, 0.25, 0.25, 1.0).into()),
                ))
                .with_children(|commands| {
                    commands.spawn((
                        Text::new("I'm sorry, but the game has crashed."),
                        TextFont {
                            font_size: 30.0,
                            ..Default::default()
                        },
                        TextColor(Color::WHITE),
                    ));
                    commands.spawn((
                        Text::new(error_text),
                        TextFont {
                            font_size: 20.0,
                            ..Default::default()
                        },
                        TextColor(Color::WHITE),
                    ));
                });

            commands.spawn(Node {
                height: Val::Percent(20.0),
                ..Default::default()
            });
        })
        .id();

    spawn_button(&mut commands, "Quit", QuitButton).insert(ChildOf(menu_entity));
    spawn_prioritized_button(&mut commands, "Copy error to Clipboard", CopyButton)
        .insert(ChildOf(menu_entity));
}

fn despawn(mut commands: Commands, main_menu: Query<Entity, With<FatalErrorDisplay>>) {
    for menu in main_menu.iter() {
        commands.entity(menu).despawn();
    }
}

// TODO: restore bevy_ui_navigation event handler
// fn handle_selections(
//     mut events: EventReader<NavEvent>,
//     mut next_state: ResMut<NextState<GameState>>,
//     quit_buttons: Query<(), With<QuitButton>>,
//     copy_buttons: Query<(), With<CopyButton>>,
//     error_context: Option<Res<ErrorContext>>,
//     mut clipboard: Option<ResMut<Clipboard>>,
// ) {
//     for event in events.iter() {
//         if let NavEvent::NoChanges { from, request } = event {
//             if matches!(request, NavRequest::Action) {
//                 if quit_buttons.contains(*from.first()) {
//                     next_state.set(GameState::ShuttingDown);
//                 }
//                 if copy_buttons.contains(*from.first()) {
//                     match clipboard.as_mut() {
//                         Some(clipboard) => {
//                             match clipboard.set_contents(extract_error_context(&error_context)) {
//                                 Ok(()) => {
//                                     log::info!("Copied error message to clipboard.");
//                                     clipboard.get_contents().ok();
//                                 }
//                                 Err(error) => {
//                                     log::error!(
//                                         "Failed to copy error message to system clipboard: {:?}",
//                                         error
//                                     );
//                                 }
//                             }
//                         }
//                         None => {
//                             log::error!("Clipboard was unavailable when trying to copy error message to it.");
//                         }
//                     }
//                 }
//             }
//         }
//     }
// }

pub fn setup(app: &mut App) {
    app.add_systems(OnEnter(GameState::FatalError), spawn);
    app.add_systems(OnExit(GameState::FatalError), despawn);
}
