use anyhow::Result;
use bevy::{prelude::*, utils::synccell::SyncCell};
use bevy_ui_navigation::{prelude::*, NavMarkerPropagationPlugin};
use copypasta::{ClipboardContext, ClipboardProvider};

mod fatal_error_display;
mod main_menu;
mod pause_menu;
mod settings_menu;
pub mod widgets;

pub use fatal_error_display::ErrorContext;

use crate::error_handler;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct UserInterface;

impl Plugin for UserInterface {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, setup_clipboard.pipe(error_handler));
        app.add_plugins(NavMarkerPropagationPlugin::<OverlayMenu>::new());

        widgets::setup(app);

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

#[derive(Component, Clone)]
pub struct OverlayMenu;

pub fn setup_submenu<Menu: Component, Marker: Component + Clone>(app: &mut App) {
    fn hide_when_unfocused<Menu: Component, Marker: Component + Clone>(
        menu_items: Query<(), With<Marker>>,
        overlay_menus: Query<(), (Added<Focused>, With<OverlayMenu>)>,
        mut menu_styles: Query<&mut Style, With<Menu>>,
        mut removed: RemovedComponents<Focused>,
    ) {
        if overlay_menus.is_empty() {
            for removal in removed.iter() {
                if menu_items.contains(removal) {
                    for mut menu_style in menu_styles.iter_mut() {
                        menu_style.display = Display::None;
                    }
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
