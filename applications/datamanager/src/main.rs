mod data;
mod equity_bars;
mod equity_details;
mod errors;
mod health;
mod portfolios;
mod predictions;
mod router;
mod state;
mod storage;

use router::create_app;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[tokio::main]
async fn main() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "example=debug,tower_http=debug,axum=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let app = create_app().await;
    let listener = tokio::net::TcpListener::bind("0.0.0.0:8080").await.unwrap();

    if let Err(e) = axum::serve(listener, app).await {
        tracing::error!("Server error: {}", e);
        std::process::exit(1);
    }
}
