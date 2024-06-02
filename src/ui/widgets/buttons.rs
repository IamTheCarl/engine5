//! Buttons are a single input the player can select to trigger an event, such as moving to a new menu, resuming the game, extiting to desktop, etc.

use bevy::{ecs::system::EntityCommands, prelude::*};
use bevy_ui_navigation::{
    prelude::{FocusState, Focusable, NavEvent, NavRequest},
    NavRequestSystem,
};

fn button_system(
    mut interaction_query: Query<(&Focusable, &mut BackgroundColor), Changed<Focusable>>,
) {
    for (focusable, mut material) in interaction_query.iter_mut() {
        if let FocusState::Focused = focusable.state() {
            *material = Color::ORANGE_RED.into();
        } else {
            *material = Color::DARK_GRAY.into();
        }
    }
}

pub fn spawn_button<'w, 's, 'a, L: Bundle>(
    commands: &'a mut Commands<'w, 's>,
    text: impl Into<String>,
    label: L,
) -> EntityCommands<'a> {
    spawn_button_raw::<L>(commands, Focusable::new(), text, label)
}

pub fn spawn_prioritized_button<'w, 's, 'a, L: Bundle>(
    commands: &'a mut Commands<'w, 's>,
    text: impl Into<String>,
    label: L,
) -> EntityCommands<'a> {
    spawn_button_raw::<L>(commands, Focusable::new().prioritized(), text, label)
}

fn spawn_button_raw<'w, 's, 'a, L: Bundle>(
    commands: &'a mut Commands<'w, 's>,
    focus: Focusable,
    text: impl Into<String>,
    label: L,
) -> EntityCommands<'a> {
    let mut entity_commands = commands.spawn((
        ButtonBundle {
            style: Style {
                justify_content: JustifyContent::Center,
                align_items: AlignItems::Center,
                width: Val::Percent(100.0),
                ..Default::default()
            },
            background_color: Color::DARK_GRAY.into(),
            ..Default::default()
        },
        focus,
        label,
    ));
    entity_commands.with_children(|parent| {
        parent.spawn(TextBundle::from_section(
            text,
            TextStyle {
                font_size: 40.0,
                color: Color::WHITE,
                ..Default::default()
            },
        ));
    });

    entity_commands
}

#[derive(Component)]
pub struct BackButton;

fn process_back_buttons(
    mut events: EventReader<NavEvent>,
    mut nav_requests: EventWriter<NavRequest>,

    back_buttons: Query<(), With<BackButton>>,
) {
    for event in events.read() {
        if let NavEvent::NoChanges { from, request } = event {
            if matches!(request, NavRequest::Action) && back_buttons.contains(*from.first()) {
                nav_requests.send(NavRequest::Cancel);
            }
        }
    }
}

pub fn setup(app: &mut App) {
    app.add_systems(
        Update,
        (
            button_system.after(NavRequestSystem),
            process_back_buttons.after(NavRequestSystem),
        ),
    );
}
