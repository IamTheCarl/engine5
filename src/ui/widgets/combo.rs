//! Combos allow the user to select one item from a list of items.
//! Examples of this include:
//! * Boolean True/False, Yes/No, Enabled/Disabled
//! * Multiple Choice A, B, C, or D

use crate::ui::widgets::spawn_button;
use bevy::{ecs::system::EntityCommands, prelude::*};
// TODO: restore bevy_ui_navigation
// use bevy_ui_navigation::{
//     menu::NavMarker,
//     prelude::{Focusable, Focused, MenuBuilder, MenuSetting, NavEvent, NavRequest},
//     NavMarkerPropagationPlugin, NavRequestSystem,
// };

#[derive(Component)]
pub struct Combo {
    pub selection: usize,
    options: Vec<String>,
    status_indicator: Entity,
}

#[derive(Component)]
struct ComboStatus;

pub fn spawn_combo<'w, 's, 'a, Tag>(
    commands: &'a mut Commands,
    tag: Tag,
    text: &str,
    initial_selection: usize,
    options: impl IntoIterator<Item = impl Into<String>>,
) -> EntityCommands<'a>
where
    Tag: Bundle,
{
    let options: Vec<String> = options.into_iter().map(|s| s.into()).collect();
    assert!(!options.is_empty(), "Options list cannot be empty.");

    let selection = initial_selection.min(options.len() - 1);
    let initial_selection = options[selection].clone();

    let inner_node_bundle_entity = commands
        .spawn((Node {
            display: Display::Flex,
            width: Val::Percent(50.0),
            justify_content: JustifyContent::SpaceBetween,
            align_content: AlignContent::Center,
            ..Default::default()
        },))
        .id();

    commands
        .spawn((
            Text::new(format!("{}:", text)),
            TextFont {
                font_size: 40.0,
                ..Default::default()
            },
            TextColor(Color::WHITE),
            ComboStatus,
        ))
        .insert(ChildOf(inner_node_bundle_entity));

    let status_indicator = commands
        .spawn((
            Text::new(initial_selection),
            TextFont {
                font_size: 40.0,
                ..Default::default()
            },
            TextColor(Color::WHITE),
            ComboStatus,
        ))
        .insert(ChildOf(inner_node_bundle_entity))
        .id();

    let outer_node_bundle_entity = commands
        .spawn((Node {
            display: Display::Flex,
            width: Val::Percent(100.0),
            align_content: AlignContent::Center,
            align_items: AlignItems::Center,
            flex_direction: FlexDirection::Column,
            ..Default::default()
        },))
        .id();

    let combo = Combo {
        selection,
        options,
        status_indicator,
    };

    let _combo_menu_entity = spawn_combo_selection_menu(commands, &combo).id();

    let button_entity = commands
        .spawn((
            Button,
            Node {
                width: Val::Percent(100.0),
                align_items: AlignItems::Center,
                flex_direction: FlexDirection::Column,
                ..Default::default()
            },
            BackgroundColor(Color::srgba(0.25, 0.25, 0.25, 1.0).into()),
            combo,
            tag,
        ))
        .add_child(inner_node_bundle_entity)
        .insert(ChildOf(outer_node_bundle_entity))
        .id();

    let mut outer_node_bundle = commands.entity(outer_node_bundle_entity);
    outer_node_bundle.add_child(_combo_menu_entity);

    outer_node_bundle
}

// TODO: stub selection menu for bevy_ui_navigation restore
fn spawn_combo_selection_menu<'a>(
    _commands: &'a mut Commands,
    _combo: &Combo,
) -> EntityCommands<'a> {
    let e = _commands.spawn(Node::default()).id();
    _commands.entity(e)
}

fn update_combo_text(
    combos: Query<&Combo, Changed<Combo>>,
    mut combo_status_indicators: Query<&mut Text, With<ComboStatus>>,
) {
    for combo in combos.iter() {
        if let Ok(mut status_indicator) = combo_status_indicators.get_mut(combo.status_indicator) {
            let text = combo.options[combo.selection.min(combo.options.len() - 1)].clone();
            status_indicator.0 = text;
        }
    }
}

// TODO: stub navigation-dependent components and systems
// #[derive(Component)]
// struct ComboEntity(Entity);

// #[derive(Component, Clone)]
// struct ComboMenuMarker {
//     menu_root: Entity,
// }

// #[derive(Component)]
// struct ComboMenu;

// #[derive(Component)]
// struct OptionIndex(usize);

// TODO: stub navigation handlers for bevy_ui_navigation restore
// fn hide_combo_when_unfocused(
//     menu_items: Query<&ComboMenuMarker>,
//     overlay_menus: Query<(), (Added<Focused>, With<OverlayMenu>)>,
//     mut menu_styles: Query<&mut Style, With<ComboMenu>>,
//     mut removed: RemovedComponents<Focused>,
// ) {
//     if overlay_menus.is_empty() {
//         for removal in removed.iter() {
//             if let Ok(menu_item) = menu_items.get(removal) {
//                 if let Ok(mut menu_style) = menu_styles.get_mut(menu_item.menu_root) {
//                     menu_style.display = Display::None;
//                 }
//             }
//         }
//     }
// }

// fn show_combo_when_focused(
//     menu_items: Query<&ComboMenuMarker, Added<Focused>>,
//     mut menu_styles: Query<&mut Style, With<ComboMenu>>,
// ) {
//     if let Ok(menu_item) = menu_items.get_single() {
//         if let Ok(mut menu_style) = menu_styles.get_mut(menu_item.menu_root) {
//             menu_style.display = Display::Flex;
//         }
//     }
// }

// fn handle_combo_selection(
//     mut events: EventReader<NavEvent>,
//     mut nav_requests: MessageWriter<NavRequest>,
//     combo_menu_buttons: Query<(&ChildOf, &OptionIndex), With<Button>>,
//     combo_references: Query<&ComboEntity>,
//     mut combos: Query<&mut Combo>,
// ) {
//     for event in events.iter() {
//         if let NavEvent::NoChanges { from, request } = event {
//             if matches!(request, NavRequest::Action) {
//                 if let Ok((parent, option_index)) = combo_menu_buttons.get(*from.first()) {
//                     let combo_reference = combo_references.get(parent.0).unwrap();
//                     let mut combo = combos.get_mut(combo_reference.0).unwrap();
//                     combo.selection = option_index.0;
//                     nav_requests.send(NavRequest::Cancel);
//                 }
//             }
//         }
//     }
// }

pub fn setup(app: &mut App) {
    // TODO: restore bevy_ui_navigation setup
    // app.add_plugins(NavMarkerPropagationPlugin::<ComboMenuMarker>::new());
    // app.add_systems(
    //     Update,
    //     (
    //         hide_combo_when_unfocused.after(NavRequestSystem),
    //         show_combo_when_focused
    //             .after(hide_combo_when_unfocused)
    //             .after(NavRequestSystem),
    //         handle_combo_selection.after(NavRequestSystem),
    //         update_combo_text.after(NavRequestSystem),
    //     ),
    // );
    app.add_systems(Update, update_combo_text);
}
