use std::net::TcpListener;

use discordbot::run;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    run(TcpListener::bind("0.0.0.0:8080").expect("failed to bind to port"))?.await
}
