use bevy::{log::LogPlugin, prelude::*, window::ExitCondition};

mod map;

fn main() {
    App::new()
        .add_plugins((
            DefaultPlugins
                .set(AssetPlugin::default())
                .set(WindowPlugin {
                    primary_window: Some(Window {
                        title: "Engine 5".to_string(),
                        ..Default::default()
                    }),
                    exit_condition: ExitCondition::DontExit, // We handle the exit ourselves.
                    ..default()
                })
                .disable::<LogPlugin>(),
            Engine5::new(),
        ))
        .run();
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
struct Engine5;

impl Engine5 {
    fn new() -> Self {
        Self
    }
}

impl Plugin for Engine5 {
    fn build(&self, app: &mut App) {
        app.add_plugins((map::MapPlugin,));
        app.configure_sets(Update, Engine5);
    }
}

// pub fn error_handler(
//     In(result): In<anyhow::Result<()>>,
//     mut next_state: ResMut<NextState<GameState>>,
// ) {
//     // Only switch to error state if there was actually an error.
//     if let Err(error) = result {
//         log::error!("Switching to error state. Caused by: {error}");
//         // commands.insert_resource(ErrorContext { error });
//         next_state.set(GameState::FatalError);
//     }
// }
