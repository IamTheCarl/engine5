use std::{mem::size_of, time::Duration};

use anyhow::{Context, Result};
use bevy::prelude::*;
use bytes::Bytes;
use proc_macros::ref_serializable;
use renet::{ChannelConfig, ConnectionConfig, SendType};
use serde::{Deserialize, Serialize};

use crate::{
    world::{
        spatial_entities::storage::{EntityTypeId, SaveSystemSet, TracerId},
        terrain::{terrain_space::ChunkModificationRequest, Block, Chunk, ChunkIndex},
    },
    GameState,
};

mod client;
pub use client::ClientContext;

mod host;
pub use host::{HostContext, ToTransmitEntity};

const RESEND_TIME: Duration = Duration::from_millis(300);

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct MultiplayerPlugin;

impl Plugin for MultiplayerPlugin {
    fn build(&self, app: &mut App) {
        // We run this before the save system set so that the save system will
        // know which entities need to be serialized.
        app.configure_set(Update, MultiplayerPlugin.before(SaveSystemSet));

        // If we unload the world, we disconnect from the host or end our hosting session.
        app.add_systems(OnExit(GameState::InGame), |mut commands: Commands| {
            commands.remove_resource::<HostContext>();
            commands.remove_resource::<ClientContext>();
        });

        app.add_event::<PlayerConnect>();
        app.add_event::<PlayerDisconnect>();
        app.add_event::<EntityUpdate>();

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

#[derive(Debug, Event)]
pub struct EntityUpdate {
    pub tracer_id: TracerId,
    pub type_id: EntityTypeId,
    pub payload: Bytes,
}

impl EntityUpdate {
    const TRACER_ID_LENGTH: usize = size_of::<TracerId>();
    const TYPE_ID_LENGTH: usize = size_of::<EntityTypeId>();

    const TRACER_ID_END: usize = Self::TRACER_ID_LENGTH;
    const TYPE_ID_END: usize = Self::TRACER_ID_END + Self::TYPE_ID_LENGTH;

    fn deserialize(source: &Bytes) -> Result<Self> {
        let tracer_id_source = &source
            .get(..Self::TRACER_ID_END)
            .context("Unexpected end of data when reading tracer id.")?;
        let mut tracer_id = [0u8; Self::TRACER_ID_LENGTH];
        tracer_id.copy_from_slice(tracer_id_source);
        let tracer_id = TracerId::from_le_bytes(tracer_id);

        let type_id_source = &source
            .get(Self::TRACER_ID_END..Self::TYPE_ID_END)
            .context("Unexpected end of data when reading entity type id.")?;
        let mut type_id = [0u8; Self::TYPE_ID_LENGTH];
        type_id.copy_from_slice(type_id_source);
        let type_id = EntityTypeId::from_le_bytes(type_id);

        // The rest is the payload.
        let payload = source.slice(Self::TYPE_ID_END..);

        Ok(Self {
            tracer_id,
            type_id,
            payload,
        })
    }

    pub fn serialize(
        serializer: impl FnOnce(&mut Vec<u8>) -> Result<(TracerId, EntityTypeId)>,
    ) -> Result<Bytes> {
        let mut bytes: Vec<u8> = Vec::new();
        bytes.extend([0u8; Self::TRACER_ID_LENGTH + Self::TYPE_ID_LENGTH]); // Make a space for the TracerId.

        let (tracer_id, entity_type_id) = serializer(&mut bytes)?;

        // Now that we have that information, we can populate it.
        bytes[..Self::TRACER_ID_END].copy_from_slice(&tracer_id.to_le_bytes());
        bytes[Self::TRACER_ID_END..Self::TYPE_ID_END]
            .copy_from_slice(&entity_type_id.to_le_bytes());

        Ok(bytes.into())
    }
}

// TODO send terrain modification requests.
