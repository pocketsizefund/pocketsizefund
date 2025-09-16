use datamanager::create_app;
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
    let listener = tokio::net::TcpListener::bind("0.0.0.0:9000").await.unwrap();

    axum::serve(listener, app).await.unwrap();
}
