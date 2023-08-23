//! Widgets are building blocks of the User Interface.

use bevy::prelude::*;

mod buttons;
mod combo;
mod numaric_input;

pub use buttons::{spawn_button, spawn_prioritized_button, BackButton};
pub use combo::{spawn_combo, Combo};
pub use numaric_input::{spawn_numaric_input, NumaricInput, Sign};

pub fn setup(app: &mut App) {
    buttons::setup(app);
    combo::setup(app);
    numaric_input::setup(app);
}
