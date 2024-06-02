//! Combos allow the user to select one item from a list of items.
//! Examples of this include:
//! * Boolean True/False, Yes/No, Enabled/Disabled
//! * Multiple Choice A, B, C, or D

use crate::ui::{sub_menus::OverlayMenu, widgets::spawn_button};
use bevy::{ecs::system::EntityCommands, prelude::*};
use bevy_ui_navigation::{
    menu::NavMarker,
    prelude::{Focusable, Focused, MenuBuilder, MenuSetting, NavEvent, NavRequest},
    NavMarkerPropagationPlugin, NavRequestSystem,
};

#[derive(Component)]
pub struct Combo {
    pub selection: usize,
    options: Vec<String>,
    status_indicator: Entity,
}

#[derive(Component)]
struct ComboStatus;

pub fn spawn_combo<'w, 's, 'a, Tag>(
    commands: &'a mut Commands<'w, 's>,
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
        .spawn(NodeBundle {
            style: Style {
                display: Display::Flex,
                width: Val::Percent(50.0),
                justify_content: JustifyContent::SpaceBetween,
                align_content: AlignContent::Center,
                ..Default::default()
            },
            ..Default::default()
        })
        .id();

    commands
        .spawn((
            TextBundle::from_section(
                format!("{}:", text),
                TextStyle {
                    font_size: 40.0,
                    color: Color::WHITE,
                    ..Default::default()
                },
            ),
            ComboStatus,
        ))
        .set_parent(inner_node_bundle_entity);

    let status_indicator = commands
        .spawn((
            TextBundle::from_section(
                initial_selection,
                TextStyle {
                    font_size: 40.0,
                    color: Color::WHITE,
                    ..Default::default()
                },
            ),
            ComboStatus,
        ))
        .set_parent(inner_node_bundle_entity)
        .id();

    let outer_node_bundle_entity = commands
        .spawn((
            NodeBundle {
                style: Style {
                    display: Display::Flex,
                    width: Val::Percent(100.0),
                    align_content: AlignContent::Center,
                    align_items: AlignItems::Center,
                    flex_direction: FlexDirection::Column,
                    ..Default::default()
                },
                ..Default::default()
            },
            MenuBuilder::Root,
        ))
        .id();

    let combo = Combo {
        selection,
        options,
        status_indicator,
    };

    let combo_menu_entity = spawn_combo_selection_menu(commands, &combo).id();

    let button_entity = commands
        .spawn((
            ButtonBundle {
                style: Style {
                    width: Val::Percent(100.0),
                    // justify_content: JustifyContent::Center,
                    align_items: AlignItems::Center,
                    flex_direction: FlexDirection::Column,
                    ..Default::default()
                },
                background_color: Color::DARK_GRAY.into(),
                ..Default::default()
            },
            combo,
            tag,
            Focusable::new(),
        ))
        .add_child(inner_node_bundle_entity)
        .set_parent(outer_node_bundle_entity)
        .id();

    commands.entity(combo_menu_entity).insert((
        MenuBuilder::EntityParent(button_entity),
        ComboEntity(button_entity),
    ));

    let mut outer_node_bundle = commands.entity(outer_node_bundle_entity);
    outer_node_bundle.add_child(combo_menu_entity);

    outer_node_bundle
}

fn update_combo_text(
    combos: Query<&Combo, Changed<Combo>>,
    mut combo_status_indicators: Query<&mut Text, With<ComboStatus>>,
) {
    for combo in combos.iter() {
        if let Ok(mut status_indicator) = combo_status_indicators.get_mut(combo.status_indicator) {
            let text = combo.options[combo.selection.min(combo.options.len() - 1)].clone();
            status_indicator.sections[0].value = text;
        }
    }
}

#[derive(Component)]
struct ComboEntity(Entity);

#[derive(Component, Clone)]
struct ComboMenuMarker {
    menu_root: Entity,
}

#[derive(Component)]
struct ComboMenu;

#[derive(Component)]
struct OptionIndex(usize);

fn spawn_combo_selection_menu<'w, 's, 'a>(
    commands: &'a mut Commands<'w, 's>,
    combo: &Combo,
) -> EntityCommands<'a> {
    let mut buttons = Vec::new();

    for (index, option) in combo.options.iter().enumerate() {
        buttons.push(
            spawn_button(commands, option.clone(), ())
                .insert(OptionIndex(index))
                .id(),
        );
    }

    let mut node_bundle = commands.spawn((
        NodeBundle {
            style: Style {
                display: Display::None,
                justify_content: JustifyContent::Center,
                flex_direction: FlexDirection::Column,
                width: Val::Percent(40.0),
                // height: Val::Percent(100.0),
                ..Default::default()
            },
            background_color: Color::DARK_GRAY.into(),
            ..Default::default()
        },
        MenuSetting::new(),
        ComboMenu,
        NavMarker(OverlayMenu),
    ));

    let node_bundle_entity = node_bundle.id();
    node_bundle.insert(NavMarker(ComboMenuMarker {
        menu_root: node_bundle_entity,
    }));

    for button in buttons {
        node_bundle.add_child(button);
    }
    node_bundle
}

fn hide_combo_when_unfocused(
    menu_items: Query<&ComboMenuMarker>,
    overlay_menus: Query<(), (Added<Focused>, With<OverlayMenu>)>,
    mut menu_styles: Query<&mut Style, With<ComboMenu>>,
    mut removed: RemovedComponents<Focused>,
) {
    if overlay_menus.is_empty() {
        for removal in removed.read() {
            if let Ok(menu_item) = menu_items.get(removal) {
                if let Ok(mut menu_style) = menu_styles.get_mut(menu_item.menu_root) {
                    menu_style.display = Display::None;
                }
            }
        }
    }
}

fn show_combo_when_focused(
    menu_items: Query<&ComboMenuMarker, Added<Focused>>,
    mut menu_styles: Query<&mut Style, With<ComboMenu>>,
) {
    if let Ok(menu_item) = menu_items.get_single() {
        if let Ok(mut menu_style) = menu_styles.get_mut(menu_item.menu_root) {
            menu_style.display = Display::Flex;
        }
    }
}

fn handle_combo_selection(
    mut events: EventReader<NavEvent>,
    mut nav_requests: EventWriter<NavRequest>,
    combo_menu_buttons: Query<(&Parent, &OptionIndex), With<Button>>,
    combo_references: Query<&ComboEntity>,
    mut combos: Query<&mut Combo>,
) {
    for event in events.read() {
        if let NavEvent::NoChanges { from, request } = event {
            if matches!(request, NavRequest::Action) {
                if let Ok((parent, option_index)) = combo_menu_buttons.get(*from.first()) {
                    let combo_reference = combo_references.get(parent.get()).unwrap();
                    let mut combo = combos.get_mut(combo_reference.0).unwrap();

                    combo.selection = option_index.0;
                    nav_requests.send(NavRequest::Cancel);
                }
            }
        }
    }
}

pub fn setup(app: &mut App) {
    app.add_plugins(NavMarkerPropagationPlugin::<ComboMenuMarker>::new());
    app.add_systems(
        Update,
        (
            hide_combo_when_unfocused.after(NavRequestSystem),
            show_combo_when_focused
                .after(hide_combo_when_unfocused)
                .after(NavRequestSystem),
            handle_combo_selection.after(NavRequestSystem),
            update_combo_text.after(NavRequestSystem),
        ),
    );
}
