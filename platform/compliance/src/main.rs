use compliance::run;
use std::net::TcpListener;

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    let port = std::env::var("PORT").unwrap_or("8080".to_string());
    run(TcpListener::bind(format!("0.0.0.0:{}", port)).expect("failed to bind to port"))?.await
}
