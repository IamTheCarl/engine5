use std::{collections::HashSet, time::Duration};

use bevy::prelude::*;
use proc_macros::ref_serializable;
use renet::{ChannelConfig, ConnectionConfig, SendType};
use serde::{Deserialize, Serialize};

use crate::{
    world::{
        spatial_entities::storage::TracerId,
        terrain::{terrain_space::ChunkModificationRequest, Block, Chunk, ChunkIndex},
    },
    GameState,
};

mod client;
pub use client::ClientContext;

mod host;
pub use host::HostContext;

const RESEND_TIME: Duration = Duration::from_millis(300);

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct MultiplayerPlugin;

impl Plugin for MultiplayerPlugin {
    fn build(&self, app: &mut App) {
        // If we unload the world, we disconnect from the host or end our hosting session.
        app.add_systems(OnExit(GameState::InGame), |mut commands: Commands| {
            commands.remove_resource::<HostContext>();
            commands.remove_resource::<ClientContext>();
        });

        app.add_event::<PlayerConnect>();
        app.add_event::<PlayerDisconnect>();

        HostContext::setup(app);
        ClientContext::setup(app);
    }
}

pub enum ServerChannels {
    UpdateEntity,
    UpdateTerrain,
}

impl From<ServerChannels> for u8 {
    fn from(value: ServerChannels) -> Self {
        match value {
            ServerChannels::UpdateEntity => 0,
            ServerChannels::UpdateTerrain => 1,
        }
    }
}

impl ServerChannels {
    fn config() -> Vec<ChannelConfig> {
        vec![
            ChannelConfig {
                channel_id: ServerChannels::UpdateEntity.into(),
                max_memory_usage_bytes: 1024 * 64 * 64,
                send_type: SendType::Unreliable,
            },
            ChannelConfig {
                channel_id: ServerChannels::UpdateTerrain.into(),
                max_memory_usage_bytes: Chunk::CHUNK_DIAMETER as usize
                    * Chunk::CHUNK_DIAMETER as usize
                    * Chunk::CHUNK_DIAMETER as usize
                    * std::mem::size_of::<Block>(),
                send_type: SendType::ReliableOrdered {
                    resend_time: RESEND_TIME,
                },
            },
        ]
    }
}

pub enum ClientChannels {
    PlayerCommand,
}

impl From<ClientChannels> for u8 {
    fn from(value: ClientChannels) -> Self {
        match value {
            ClientChannels::PlayerCommand => 0,
        }
    }
}

impl ClientChannels {
    fn config() -> Vec<ChannelConfig> {
        vec![ChannelConfig {
            channel_id: ServerChannels::UpdateEntity.into(),
            max_memory_usage_bytes: 1024 * 64 * 64,
            send_type: SendType::ReliableOrdered {
                resend_time: RESEND_TIME,
            },
        }]
    }
}

fn connection_config() -> ConnectionConfig {
    ConnectionConfig {
        server_channels_config: ServerChannels::config(),
        client_channels_config: ClientChannels::config(),
        ..Default::default()
    }
}

#[derive(Debug, Event)]
pub struct PlayerConnect {
    client_id: u64,
    name: String,
}

#[derive(Debug, Event)]
pub struct PlayerDisconnect {
    client_id: u64,
}

#[derive(Debug, Component)]
#[component(storage = "SparseSet")]
struct ToTransmitTerrain {
    terrain_space_tracer_id: TracerId,
    to_clients: HashSet<u64>,
}

#[derive(Debug, Component)]
struct SendTerrain {
    loaded_chunks: HashSet<ChunkIndex>,
}

#[ref_serializable]
enum ChunkUpdate {
    Load {
        terrain_space: TracerId,
        index: ChunkIndex,
        #[owned_box]
        #[borrowed_ref]
        chunk: Chunk,
    },
    Unload {
        terrain_space: TracerId,
        index: ChunkIndex,
    },
    Modify(ChunkModificationRequest),
}

#[derive(Serialize, Deserialize)]
struct ClientHeader {
    name: String,
}

// TODO send terrain modification requests.
