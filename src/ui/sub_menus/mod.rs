use bevy::prelude::*;
use bevy_ui_navigation::{prelude::*, NavMarkerPropagationPlugin};

pub mod settings;

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
            for removal in removed.read() {
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
