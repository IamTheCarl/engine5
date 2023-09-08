use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use vec_rand::RandVec;

use super::Config;

#[derive(Resource, Serialize, Deserialize)]
pub struct PlayerInfo {
    pub client_id: u64,
    pub name: String,
}

impl Default for PlayerInfo {
    fn default() -> Self {
        let name = whoami::realname();

        // The MAC address is guarenteed to be unique, so prefer that.
        // If for some weird reason we can't get one, generate a random number and hope for the best.
        let client_id_vec = mac_address::get_mac_address()
            .ok()
            .flatten()
            .map(|mac| {
                let mut bytes: Vec<u8> = mac.bytes().into();
                bytes.extend([0, 0]);
                bytes
            })
            .unwrap_or(RandVec::generate(8));

        let mut client_id = [0; 8];
        client_id.copy_from_slice(&client_id_vec);

        let client_id = u64::from_le_bytes(client_id);

        Self { name, client_id }
    }
}

impl Config for PlayerInfo {
    const CONFIG_FILE: &'static str = "player_info.yaml";
}

pub(crate) fn setup(app: &mut App) {
    PlayerInfo::app_setup(app);
}
