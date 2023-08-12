use anyhow::Result;
use bevy::{prelude::*, utils::synccell::SyncCell};
use bevy_ui_navigation::{prelude::*, NavMarkerPropagationPlugin};
use copypasta::{ClipboardContext, ClipboardProvider};

mod fatal_error_display;
mod main_menu;
mod pause_menu;
mod settings_menu;

pub use fatal_error_display::ErrorContext;

use crate::error_handler;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct UserInterface;

impl Plugin for UserInterface {
    fn build(&self, app: &mut App) {
        app.add_systems(
            Update,
            (
                button_system.after(NavRequestSystem),
                process_back_buttons.after(NavRequestSystem),
            ),
        );

        app.add_systems(Startup, setup_clipboard.pipe(error_handler));

        main_menu::setup(app);
        fatal_error_display::setup(app);
        pause_menu::setup(app);
        settings_menu::setup(app);
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

fn spawn_button<L: Component>(
    commands: &mut ChildBuilder,
    text: impl Into<String>,
    label: L,
) -> Entity {
    spawn_button_raw::<L>(commands, Focusable::new(), text, label)
}

fn spawn_prioritized_button<L: Component>(
    commands: &mut ChildBuilder,
    text: impl Into<String>,
    label: L,
) -> Entity {
    spawn_button_raw::<L>(commands, Focusable::new().prioritized(), text, label)
}

fn spawn_button_raw<L: Component>(
    commands: &mut ChildBuilder,
    focus: Focusable,
    text: impl Into<String>,
    label: L,
) -> Entity {
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
            focus,
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
        })
        .id()
}

#[derive(Component)]
pub struct BackButton;

fn process_back_buttons(
    mut events: EventReader<NavEvent>,
    mut nav_requests: EventWriter<NavRequest>,

    back_buttons: Query<(), With<BackButton>>,
) {
    for event in events.iter() {
        if let NavEvent::NoChanges { from, request } = event {
            if matches!(request, NavRequest::Action) && back_buttons.contains(*from.first()) {
                nav_requests.send(NavRequest::Cancel);
            }
        }
    }
}

pub fn setup_submenu<Menu: Component, Marker: Component + Clone>(app: &mut App) {
    fn hide_when_unfocused<Menu: Component, Marker: Component + Clone>(
        menu_items: Query<(), With<Marker>>,
        mut menu_styles: Query<&mut Style, With<Menu>>,
        mut removed: RemovedComponents<Focused>,
    ) {
        for removal in removed.iter() {
            if menu_items.contains(removal) {
                for mut menu_style in menu_styles.iter_mut() {
                    menu_style.display = Display::None;
                }
            }
        }
    }
    fn show_when_focused<Menu: Component, Marker: Component + Clone>(
        menu_items: Query<(), (Added<Focused>, With<Marker>)>,
        mut menu_styles: Query<&mut Style, With<Menu>>,
    ) {
        if menu_items.get_single().is_ok() {
            for mut menu_style in menu_styles.iter_mut() {
                menu_style.display = Display::Flex;
            }
        }
    }

    app.add_plugins(NavMarkerPropagationPlugin::<Marker>::new());
    app.add_systems(
        Update,
        (
            hide_when_unfocused::<Menu, Marker>.after(NavRequestSystem),
            show_when_focused::<Menu, Marker>
                .after(hide_when_unfocused::<Menu, Marker>)
                .after(NavRequestSystem),
        ),
    );
}
