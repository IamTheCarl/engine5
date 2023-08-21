use bevy::prelude::*;

mod buttons;
mod combo;

pub use buttons::{spawn_button, spawn_prioritized_button, BackButton};
pub use combo::{spawn_combo, Combo};

pub fn setup(app: &mut App) {
    buttons::setup(app);
    combo::setup(app);
}
