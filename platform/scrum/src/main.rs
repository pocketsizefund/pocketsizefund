use anyhow::Result;
use scrum::run;
use std::net::TcpListener;
use std::net::{IpAddr, Ipv4Addr, SocketAddr};

#[tokio::main]
async fn main() -> Result<()> {
    let port: u16 = match std::env::var("SERVER_PORT") {
        Ok(port_str) => match port_str.parse() {
            Ok(port) => port,
            Err(e) => {
                tracing::warn!("Failed to parse port: {}, defaulting to 8080", e);
                8080
            }
        },
        Err(e) => {
            tracing::warn!("Error reading `SERVER_PORT`, defaulting to 8080: {}", e);
            8080
        }
    };
    let socket = SocketAddr::new(IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1)), port);
    let listener = TcpListener::bind(socket).expect("Failed to bind to port");

    run(listener)?.await?;
    Ok(())
}
