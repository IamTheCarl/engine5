use anyhow::Result;
use bevy::{prelude::*, utils::synccell::SyncCell};
use bevy_ui_navigation::NavMarkerPropagationPlugin;
use copypasta::{ClipboardContext, ClipboardProvider};

mod root_menus;
mod sub_menus;
mod widgets;

pub use root_menus::fatal_error_display::ErrorContext;

use crate::error_handler;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct UserInterface;

impl Plugin for UserInterface {
    fn build(&self, app: &mut App) {
        app.add_systems(Startup, setup_clipboard.pipe(error_handler));
        app.add_plugins(NavMarkerPropagationPlugin::<sub_menus::OverlayMenu>::new());

        widgets::setup(app);

        root_menus::main_menu::setup(app);
        root_menus::fatal_error_display::setup(app);
        root_menus::pause_menu::setup(app);
        sub_menus::settings::setup(app);
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
