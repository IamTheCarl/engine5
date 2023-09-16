use std::{
    net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, UdpSocket},
    time::{Duration, SystemTime},
};

use anyhow::{anyhow, Context, Result};
use bevy::prelude::*;
use renet::{
    transport::{
        ClientAuthentication, NetcodeClientTransport, NetcodeDisconnectReason,
        NETCODE_USER_DATA_BYTES,
    },
    ConnectionConfig, RenetClient,
};

use crate::multiplayer::{ClientChannels, ServerChannels};

use super::ClientHeader;

#[derive(Resource)]
pub struct ClientContext {
    client_id: u64,
    client_header: [u8; NETCODE_USER_DATA_BYTES],
    client: RenetClient,
    transport: NetcodeClientTransport,
    addresses: Box<dyn Iterator<Item = SocketAddr> + Sync + Send>,
}

impl ClientContext {
    pub fn setup(app: &mut App) {
        app.add_systems(
            FixedUpdate,
            Self::update_client.run_if(resource_exists::<Self>()),
        );
    }

    pub fn start_session(
        commands: &mut Commands,
        mut addresses: Box<dyn Iterator<Item = SocketAddr> + Sync + Send>,
        client_id: u64,
        player_name: String,
    ) -> Result<()> {
        let client_header = ClientHeader { name: player_name };
        let mut raw_client_header = [0u8; NETCODE_USER_DATA_BYTES];
        bincode::serialize_into(&mut raw_client_header[..], &client_header)
            .context("Failed to serialize client header.")?;
        let client_header = raw_client_header;

        let remote_address = addresses
            .next()
            .context("No addresses available for server.")?;

        let (client, transport) =
            Self::create_client_transport_pair(remote_address, client_id, &client_header)?;

        commands.insert_resource(Self {
            client_id,
            client_header,
            client,
            transport,
            addresses,
        });

        Ok(())
    }

    fn select_local_socket(remote_address: &SocketAddr) -> Result<SocketAddr> {
        // In many operating systems, you can only connect to an IPV6 from an IPV6
        // and only an IPV4 can communicate with an IPV4, so make sure to select the right kind of socket when doing that.
        let local_ip_address = if remote_address.is_ipv4() {
            IpAddr::V4(Ipv4Addr::UNSPECIFIED)
        } else {
            IpAddr::V6(Ipv6Addr::UNSPECIFIED)
        };

        Ok(SocketAddr::new(local_ip_address, 0))
    }

    fn create_client_transport_pair(
        remote_address: SocketAddr,
        client_id: u64,
        client_header: &[u8; NETCODE_USER_DATA_BYTES],
    ) -> Result<(RenetClient, NetcodeClientTransport)> {
        log::info!("Trying address `{}`", remote_address);

        let socket = UdpSocket::bind(Self::select_local_socket(&remote_address)?)
            .context("Failed to bind UDP socket.")?;
        let client_config = ClientAuthentication::Unsecure {
            protocol_id: 0,
            client_id,
            server_addr: remote_address,
            user_data: Some(*client_header),
        };

        let current_time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .context("Failed to get current time.")?;

        let transport = NetcodeClientTransport::new(current_time, client_config, socket)
            .context("Failed to build transport layer.")?;

        let client = RenetClient::new(ConnectionConfig {
            server_channels_config: ServerChannels::config(),
            client_channels_config: ClientChannels::config(),
            ..Default::default()
        });

        Ok((client, transport))
    }

    fn try_new_address(&mut self) -> Result<()> {
        let remote_address = self
            .addresses
            .next()
            .context("No more addresses available for server.")?;

        let (client, transport) = Self::create_client_transport_pair(
            remote_address,
            self.client_id,
            &self.client_header,
        )?;

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
                    _ => Err(anyhow!(error)),
                }
            }
            _ => Ok(()),
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
