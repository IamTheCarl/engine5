use anyhow::Result;
use bevy::{prelude::*, utils::synccell::SyncCell};
use bevy_ui_navigation::prelude::*;
use copypasta::{ClipboardContext, ClipboardProvider};

mod fatal_error_display;
mod main_menu;
mod pause_menu;

pub use fatal_error_display::ErrorContext;

use crate::error_handler;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct UserInterface;

impl Plugin for UserInterface {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                // So that the UI _feels_ smooth, make sure to update the visual
                // after the navigation system ran
                button_system.after(NavRequestSystem),
            ),
        );

        app.add_systems(Startup, setup_clipboard.pipe(error_handler));

        main_menu::setup(app);
        fatal_error_display::setup(app);
        pause_menu::setup(app);
    }
}

#[derive(Resource)]
pub struct Clipboard {
    pub clipboard: SyncCell<ClipboardContext>,
}

impl std::ops::Deref for Clipboard {
    type Target = dyn ClipboardProvider;

    fn deref(&self) -> &Self::Target {
        // The only reason we even have this is because DerefMut requires it.
        unimplemented!()
    }
}

impl std::ops::DerefMut for Clipboard {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.clipboard.get()
    }
}

fn setup_clipboard(mut commands: Commands) -> Result<()> {
    let clipboard = ClipboardContext::new().map_err(|error| {
        anyhow::anyhow!("Failed to get handle to system clipboard: {:?}", error)
    })?;

    commands.insert_resource(Clipboard {
        clipboard: SyncCell::new(clipboard),
    });

    Ok(())
}

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

fn spawn_button<L: Component>(commands: &mut ChildBuilder, text: impl Into<String>, label: L) {
    commands
        .spawn((
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
            Focusable::default(),
            label,
        ))
        .with_children(|parent| {
            parent.spawn(TextBundle::from_section(
                text,
                TextStyle {
                    font_size: 40.0,
                    color: Color::WHITE,
                    ..Default::default()
                },
            ));
        });
}
