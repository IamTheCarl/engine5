use bevy::prelude::*;
use serde::{Deserialize, Serialize};

use super::Config;

#[derive(Resource, Serialize, Deserialize)]
pub struct PlayerInfo {
    pub name: String,
}

impl Default for PlayerInfo {
    fn default() -> Self {
        let name = whoami::realname();

        Self { name }
    }
}

impl Config for PlayerInfo {
    const CONFIG_FILE: &'static str = "player_info.yaml";
}

pub(crate) fn setup(app: &mut App) {
    PlayerInfo::app_setup(app);
}
