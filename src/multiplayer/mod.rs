use std::{
    net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, UdpSocket},
    time::{Duration, SystemTime},
};

use anyhow::{anyhow, Context, Result};
use bevy::prelude::*;
use renet::{
    transport::{
        ClientAuthentication, NetcodeClientTransport, NetcodeDisconnectReason,
        NetcodeServerTransport, ServerAuthentication, ServerConfig,
    },
    ConnectionConfig, RenetClient, RenetServer,
};

use crate::GameState;

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
pub struct MultiplayerPlugin;

impl Plugin for MultiplayerPlugin {
    fn build(&self, app: &mut App) {
        // If we unload the world, we disconnect from the host or end our hosting session.
        app.add_systems(OnExit(GameState::InGame), |mut commands: Commands| {
            commands.remove_resource::<HostContext>();
            commands.remove_resource::<ClientContext>();
        });

        HostContext::setup(app);
        ClientContext::setup(app);
    }
}

#[derive(Resource)]
pub struct HostContext {
    server: RenetServer,
    transport: NetcodeServerTransport,
}

impl HostContext {
    fn setup(app: &mut App) {
        app.add_systems(
            Update,
            (Self::update_host.run_if(resource_exists::<Self>()),),
        );
    }

    pub fn start_session(
        commands: &mut Commands,
        socket_address: SocketAddr,
        max_clients: usize,
    ) -> Result<()> {
        let socket = UdpSocket::bind(socket_address).context("Failed to bind UDP socket.")?;
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

        let server = RenetServer::new(ConnectionConfig::default());

        commands.insert_resource(Self { server, transport });

        Ok(())
    }

    pub fn end_session(commands: &mut Commands) {
        commands.remove_resource::<Self>();
    }

    fn update(&mut self, time_delta: Duration) -> Result<()> {
        self.server.update(time_delta);
        self.transport
            .update(time_delta, &mut self.server)
            .context("Error in transport layer.")?;

        // Announce client connections.
        while let Some(event) = self.server.get_event() {
            match event {
                renet::ServerEvent::ClientConnected { client_id } => {
                    log::info!("New client: {}", client_id);
                }
                renet::ServerEvent::ClientDisconnected { client_id, reason } => {
                    log::info!("Client {} disconnected. Reason: {}", client_id, reason);
                }
            }
        }

        Ok(())
    }

    fn update_host(mut context: ResMut<Self>, mut commands: Commands, time: Res<Time>) {
        if let Err(error) = context.update(time.delta()) {
            log::error!("Error in transport layer: {:?}", error);
            log::warn!("Hosting session will be terminated.");

            commands.remove_resource::<Self>();
        }
    }
}

#[derive(Resource)]
pub struct ClientContext {
    client_id: u64,
    client: RenetClient,
    transport: NetcodeClientTransport,
    addresses: Box<dyn Iterator<Item = SocketAddr> + Sync + Send>,
}

impl ClientContext {
    fn setup(app: &mut App) {
        app.add_systems(
            Update,
            (Self::update_client.run_if(resource_exists::<Self>()),),
        );
    }

    pub fn start_session(
        commands: &mut Commands,
        mut addresses: Box<dyn Iterator<Item = SocketAddr> + Sync + Send>,
        client_id: u64,
    ) -> Result<()> {
        let remote_address = addresses
            .next()
            .context("No addresses available for server.")?;

        let (client, transport) = Self::create_client_transport_pair(remote_address, client_id)?;

        commands.insert_resource(Self {
            client_id,
            client,
            transport,
            addresses,
        });

        Ok(())
    }

    fn select_local_socket(remote_address: &SocketAddr) -> Result<SocketAddr> {
        let port = port_selector::random_free_udp_port()
            .context("Could not find any open ports on the local system to connect with.")?;

        // In many operating systems, you can only connect to an IPV6 from an IPV6
        // and only an IPV4 can communicate with an IPV4, so make sure to select the right kind of socket when doing that.
        let local_ip_address = if remote_address.is_ipv4() {
            IpAddr::V4(Ipv4Addr::UNSPECIFIED)
        } else {
            IpAddr::V6(Ipv6Addr::UNSPECIFIED)
        };

        Ok(SocketAddr::new(local_ip_address, port))
    }

    fn create_client_transport_pair(
        remote_address: SocketAddr,
        client_id: u64,
    ) -> Result<(RenetClient, NetcodeClientTransport)> {
        log::info!("Trying address `{}`", remote_address);

        let socket = UdpSocket::bind(Self::select_local_socket(&remote_address)?)
            .context("Failed to bind UDP socket.")?;
        let client_config = ClientAuthentication::Unsecure {
            protocol_id: 0,
            client_id,
            server_addr: remote_address,
            user_data: None,
        };

        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .context("Failed to get current time.")?;

        let transport = NetcodeClientTransport::new(current_time, client_config, socket)
            .context("Failed to build transport layer.")?;

        let client = RenetClient::new(ConnectionConfig::default());

        Ok((client, transport))
    }

    fn try_new_address(&mut self) -> Result<()> {
        let remote_address = self
            .addresses
            .next()
            .context("No more addresses available for server.")?;

        let (client, transport) =
            Self::create_client_transport_pair(remote_address, self.client_id)?;

        self.transport = transport;
        self.client = client;

        Ok(())
    }

    pub fn end_session(commands: &mut Commands) {
        commands.remove_resource::<Self>();
    }

    fn update(&mut self, time_delta: Duration) -> Result<()> {
        self.client.update(time_delta);
        match self.transport.update(time_delta, &mut self.client) {
            Err(error) => {
                match self.transport.disconnect_reason() {
                    Some(NetcodeDisconnectReason::ConnectionRequestTimedOut) => {
                        // Nobody replied.
                        // Try a different address.
                        self.try_new_address()
                    }
                    _ => Err(anyhow!(dbg!(error))),
                }
            }
            _ => {
                // TODO collect messages and distribute them.

                Ok(())
            }
        }
    }

    fn update_client(mut context: ResMut<Self>, mut commands: Commands, time: Res<Time>) {
        if let Err(error) = context.update(time.delta()) {
            log::error!("Error in transport layer: {:?}", error);
            log::warn!("Client session will be terminated.");

            commands.remove_resource::<Self>();
        }
    }
}
