//! Buttons are a single input the player can select to trigger an event, such as moving to a new menu, resuming the game, extiting to desktop, etc.

use bevy::{ecs::system::EntityCommands, prelude::*};
// TODO: restore bevy_ui_navigation
// use bevy_ui_navigation::{
//     prelude::{FocusState, Focusable, NavEvent, NavRequest},
//     NavRequestSystem,
// };

// TODO: stub button_system for bevy_ui_navigation restore
// fn button_system(
//     mut interaction_query: Query<(&Focusable, &mut BackgroundColor), Changed<Focusable>>,
// ) {
//     for (focusable, mut material) in interaction_query.iter_mut() {
//         if let FocusState::Focused = focusable.state() {
//             *material = Color::srgba(1.0, 0.5, 0.0, 1.0)_RED.into();
//         } else {
//             *material = Color::srgba(0.25, 0.25, 0.25, 1.0).into();
//         }
//     }
// }

pub fn spawn_button<'w, 's, 'a, L: Bundle>(
    commands: &'a mut Commands,
    text: impl Into<String>,
    label: L,
) -> EntityCommands<'a> {
    spawn_button_raw(commands, (), text, label)
}

pub fn spawn_prioritized_button<'w, 's, 'a, L: Bundle>(
    commands: &'a mut Commands,
    text: impl Into<String>,
    label: L,
) -> EntityCommands<'a> {
    spawn_button(commands, text, label)
}

fn spawn_button_raw<'w, 's, 'a, L: Bundle>(
    commands: &'a mut Commands,
    _focus: (),
    text: impl Into<String>,
    label: L,
) -> EntityCommands<'a> {
    let mut entity_commands = commands.spawn((
        Button,
        Node {
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            width: Val::Percent(100.0),
            ..Default::default()
        },
        BackgroundColor(Color::srgba(0.25, 0.25, 0.25, 1.0).into()),
        label,
    ));
    entity_commands.with_children(|parent| {
        parent.spawn((
            Text::new(text),
            TextFont {
                font_size: 40.0,
                ..Default::default()
            },
            TextColor(Color::WHITE),
        ));
    });

    entity_commands
}

#[derive(Component)]
pub struct BackButton;

// TODO: stub process_back_buttons for bevy_ui_navigation restore
// fn process_back_buttons(
//     mut events: EventReader<NavEvent>,
//     mut nav_requests: MessageWriter<NavRequest>,
//     back_buttons: Query<(), With<BackButton>>,
// ) {
//     for event in events.iter() {
//         if let NavEvent::NoChanges { from, request } = event {
//             if matches!(request, NavRequest::Action) && back_buttons.contains(*from.first()) {
//                 nav_requests.send(NavRequest::Cancel);
//             }
//         }
//     }
// }

pub fn setup(app: &mut App) {
    // TODO: restore bevy_ui_navigation setup
    // app.add_systems(
    //     Update,
    //     (
    //         button_system.after(NavRequestSystem),
    //         process_back_buttons.after(NavRequestSystem),
    //     ),
    // );
}
