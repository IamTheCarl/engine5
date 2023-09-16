use std::{
    collections::HashSet,
    net::{SocketAddr, UdpSocket},
    time::{Duration, SystemTime},
};

use anyhow::{Context, Result};
use bevy::prelude::*;
use bytes::Bytes;
use renet::{
    transport::{NetcodeServerTransport, ServerAuthentication, ServerConfig},
    RenetServer,
};

use crate::{
    multiplayer::connection_config,
    world::{
        physics::Position,
        spatial_entities::{
            storage::{EntityStorage, Storable},
            types::player::{spawner::PlayerSpawner, LocalPlayer, PlayerEntity},
        },
        terrain::{Chunk, ChunkIndex, ChunkPosition, TerrainSpace},
        ViewRadius, WorldEntity,
    },
};

use super::{
    ChunkUpdate, ChunkUpdateRef, ClientHeader, PlayerConnect, PlayerDisconnect, SendTerrain,
    ServerChannels, ToTransmitTerrain,
};

#[derive(Resource)]
pub struct HostContext {
    server: RenetServer,
    transport: NetcodeServerTransport,
}

impl HostContext {
    pub fn setup(app: &mut App) {
        app.add_systems(
            FixedUpdate,
            (
                Self::update_host.run_if(resource_exists::<Self>()),
                Self::control_client_terrain
                    .run_if(resource_exists::<Self>())
                    .after(Self::update_host),
                Self::transmit_terrain
                    .run_if(resource_exists::<Self>())
                    .after(Self::control_client_terrain),
                Self::spawn_remote_player
                    .after(Self::update_host)
                    .run_if(resource_exists::<Self>()),
                Self::despawn_remote_player
                    .after(Self::update_host)
                    .run_if(resource_exists::<Self>()),
            ),
        );
    }

    pub fn start_session(
        commands: &mut Commands,
        socket_address: SocketAddr,
        max_clients: usize,
    ) -> Result<()> {
        let socket = UdpSocket::bind(socket_address).context("Failed to bind UDP socket.")?;
        let socket_address = socket
            .local_addr()
            .context("Failed to get local address for UDP socket.")?;

        log::info!(
            "Opening server session on socket {} with a max of {} players.",
            socket_address,
            max_clients
        );

        let server_config = ServerConfig {
            max_clients,
            protocol_id: 0,
            public_addr: socket_address,
            authentication: ServerAuthentication::Unsecure,
        };
        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .context("Failed to get current time.")?;

        let transport = NetcodeServerTransport::new(current_time, server_config, socket)
            .context("Failed to build transport layer.")?;

        let server = RenetServer::new(connection_config());

        commands.insert_resource(Self { server, transport });

        Ok(())
    }

    pub fn end_session(commands: &mut Commands) {
        commands.remove_resource::<Self>();
    }

    fn update(
        &mut self,
        time_delta: Duration,
        mut player_connect_event: EventWriter<PlayerConnect>,
        mut player_disconnect_event: EventWriter<PlayerDisconnect>,
    ) -> Result<()> {
        self.server.update(time_delta);
        self.transport
            .update(time_delta, &mut self.server)
            .context("Error in transport layer.")?;

        // Announce client connections.
        while let Some(event) = self.server.get_event() {
            match event {
                renet::ServerEvent::ClientConnected { client_id } => {
                    let player_info = self.transport.user_data(client_id).and_then(|user_data| {
                        match bincode::deserialize::<ClientHeader>(&user_data) {
                            Ok(user_data) => Some(user_data),
                            Err(error) => {
                                log::error!(
                                    "Failed to deserialize header for client {}: {:?}",
                                    client_id,
                                    error
                                );
                                None
                            }
                        }
                    });

                    if let Some(player_info) = player_info {
                        log::info!("New client: {}", client_id);
                        player_connect_event.send(PlayerConnect {
                            client_id,
                            name: player_info.name,
                        });
                    } else {
                        log::error!("Could not decode player info from client {}", client_id);
                        self.server.disconnect(client_id);
                    }
                }
                renet::ServerEvent::ClientDisconnected { client_id, reason } => {
                    log::info!("Client {} disconnected. Reason: {}", client_id, reason);
                    player_disconnect_event.send(PlayerDisconnect { client_id });
                }
            }
        }

        Ok(())
    }

    fn update_host(
        mut context: ResMut<Self>,
        mut commands: Commands,
        time: Res<Time>,
        player_connect_event: EventWriter<PlayerConnect>,
        player_disconnect_event: EventWriter<PlayerDisconnect>,
    ) {
        if let Err(error) =
            context.update(time.delta(), player_connect_event, player_disconnect_event)
        {
            log::error!("Error in transport layer: {:?}", error);
            log::warn!("Hosting session will be terminated.");

            commands.remove_resource::<Self>();
        }
    }

    #[allow(clippy::complexity)]
    fn despawn_remote_player(
        mut commands: Commands,
        online_players: Query<(Entity, &RemoteClientPlayer)>,
        mut player_disconnect_events: EventReader<PlayerDisconnect>,
    ) {
        'event_loop: for event in player_disconnect_events.iter() {
            // Check if the player already exists in the world.
            for (player_entity, remote_player) in online_players.iter() {
                if remote_player.client_id == event.client_id {
                    // This is our player! Deactivate it!
                    commands
                        .entity(player_entity)
                        .remove::<RemoteClientPlayer>();

                    break 'event_loop;
                }
            }

            // I guess they were never logged in?
        }
    }

    #[allow(clippy::complexity)]
    fn spawn_remote_player(
        mut commands: Commands,
        world_entity: Res<WorldEntity>,
        storage: Res<EntityStorage>,
        offline_players: Query<
            (Entity, &PlayerEntity),
            Without<LocalPlayer>, // We can kick-out already present local players.
        >,
        player_spawn: Query<&Position, With<PlayerSpawner>>,
        mut player_connect_events: EventReader<PlayerConnect>,
    ) {
        // FIXME This needs to kick out an old session if a player logs in twice.

        'event_loop: for event in player_connect_events.iter() {
            let remote_player = RemoteClientPlayer {
                client_id: event.client_id,
            };
            let player_name: &str = event.name.as_ref();

            // Check if the player already exists in the world.
            for (player_entity, offline_player) in offline_players.iter() {
                if offline_player.name == player_name {
                    // This is our player! Activate it!
                    commands.entity(player_entity).insert(remote_player);
                    log::info!("Welcome back {}", player_name);

                    break 'event_loop;
                }
            }

            // Looks like they don't.
            // Spawn them in.
            if let Ok(spawn_position) = player_spawn.get_single() {
                match PlayerEntity::spawn_deactivated(
                    world_entity.entity,
                    &mut commands,
                    &storage,
                    spawn_position.clone(),
                    player_name.to_string(),
                    0.0,
                ) {
                    Ok(mut player) => {
                        player.insert(remote_player);
                        log::info!("Welcome new player {}", player_name);
                    }
                    Err(error) => log::error!("Failed to spawn remote player: {:?}", error),
                }
            }
        }
    }

    fn control_client_terrain(
        mut context: ResMut<Self>,
        mut commands: Commands,
        terrain_loaders: Query<(&Position, &ViewRadius, &SendTerrain, &RemoteClientPlayer)>,
        terrain_spaces: Query<(&Storable, &Position, &TerrainSpace)>,
        mut terrain_chunks: Query<Option<&mut ToTransmitTerrain>, With<Chunk>>,
    ) {
        for (space_storage, space_position, space) in terrain_spaces.iter() {
            for (loader_position, view_radius, send_terrain, remote_player) in
                terrain_loaders.iter()
            {
                let loader_position_in_chunk_space = space_position.quat()
                    * (loader_position.translation - space_position.translation);
                let base_chunk_index =
                    (loader_position_in_chunk_space / Chunk::CHUNK_DIAMETER as f32).as_ivec3();

                let mut chunks_in_range = HashSet::new();

                let radius_squared = (view_radius.chunks * view_radius.chunks) as f32;
                for x in -view_radius.chunks..view_radius.chunks {
                    let z_range = (radius_squared - (x * x) as f32).sqrt().ceil() as i32;
                    for z in -z_range..z_range {
                        let y_range = (radius_squared - (x * x) as f32 - (z * z) as f32)
                            .sqrt()
                            .ceil() as i32;

                        for y in -y_range..y_range {
                            let chunk_index = base_chunk_index + ChunkIndex::new(x, y, z);
                            chunks_in_range.insert(chunk_index);
                        }
                    }
                }

                // Only send them terrain they don't already have.
                let to_send = chunks_in_range.difference(&send_terrain.loaded_chunks);
                for chunk_index in to_send {
                    // We don't send this information out here. We send it out later in another
                    // system once we know everyone who needs a copy of this data. It saves us a lot of time
                    // encoding and a lot of memory on duplicate buffers.
                    if let Some(chunk_entity) = space.get_loaded_chunk_entity(chunk_index) {
                        if let Some(mut to_transmit) =
                            terrain_chunks.get_mut(chunk_entity).ok().flatten()
                        {
                            to_transmit.to_clients.insert(remote_player.client_id);
                        } else {
                            commands.entity(chunk_entity).insert(ToTransmitTerrain {
                                terrain_space_tracer_id: space_storage.id(),
                                to_clients: HashSet::from([remote_player.client_id]),
                            });
                        }
                    }
                }

                // We need to instruct the client to unload chunks.
                // We do this instead of the client doing it themselves so we can keep careful track of what information they should already have.
                // TODO the player should generally determine when they want to unload terrain.
                let to_unload = send_terrain.loaded_chunks.difference(&chunks_in_range);
                for chunk_index in to_unload {
                    match bincode::serialize(&ChunkUpdate::Unload {
                        terrain_space: space_storage.id(),
                        index: *chunk_index,
                    }) {
                        Ok(message) => {
                            context.server.send_message(
                                remote_player.client_id,
                                ServerChannels::UpdateTerrain,
                                message,
                            );
                        }
                        Err(error) => {
                            // This won't exactly kill the game but it is very problematic.
                            // It means that the client will have a permanently loaded chunk now.
                            log::error!(
                                "Failed to encode chunk unload request to client: {:?}",
                                error
                            );
                        }
                    }
                }
            }
        }
    }

    fn transmit_terrain(
        mut context: ResMut<Self>,
        mut commands: Commands,
        terrain_chunks: Query<(Entity, &ToTransmitTerrain, &ChunkPosition, &Chunk)>,
    ) {
        for (chunk_entity, to_transmit, position, chunk) in terrain_chunks.iter() {
            let message = ChunkUpdateRef::Load {
                terrain_space: to_transmit.terrain_space_tracer_id,
                index: position.index,
                chunk,
            };

            match bincode::serialize(&message) {
                Ok(message) => {
                    let message: Bytes = message.into();
                    for client_id in to_transmit.to_clients.iter() {
                        // Convert to bytes for more efficient re-transmission.

                        context.server.send_message(
                            *client_id,
                            ServerChannels::UpdateTerrain,
                            message.clone(),
                        );
                    }
                }
                Err(error) => log::error!(
                    "Failed to encode terrain chunk for transmission: {:?}",
                    error
                ),
            }

            commands.entity(chunk_entity).remove::<ToTransmitTerrain>();
        }
    }
}

#[derive(Debug, Component)]
pub struct RemoteClientPlayer {
    client_id: u64,
}
