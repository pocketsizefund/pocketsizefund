use axum::{Router, routing::get};
use reqwest::Client;
use tower_http::trace::TraceLayer;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

mod routes;
use routes::equity;
use routes::health;

#[derive(Clone)]
struct AlpacaSecrets {
    base: String,
    key: String,
    secret: String,
}

#[derive(Clone)]
struct PolygonSecrets {
    base: String,
    key: String,
}

#[derive(Clone)]
struct AppState {
    client: Client,
    polygon: PolygonSecrets,
    alpaca: AlpacaSecrets,
}

impl AppState {
    fn from_env() -> Self {
        let client = Client::builder()
            .timeout(std::time::Duration::from_secs(10))
            .build()
            .unwrap();

        Self {
            client,
            polygon: PolygonSecrets {
                base: std::env::var("POLYGON_BASE_URL")
                    .unwrap_or("https://api.polygon.io".to_string()),
                key: std::env::var("POLYGON_API_KEY")
                    .expect("POLYGON_API_KEY must be set in environment"),
            },
            alpaca: AlpacaSecrets {
                base: std::env::var("ALPACA_BASE_URL")
                    .unwrap_or("https://data.alpaca.markets".to_string()),
                key: std::env::var("ALPACA_API_KEY")
                    .expect("ALPACA_API_KEY must be set in environment"),
                secret: std::env::var("ALPACA_API_SECRET")
                    .expect("ALPACA_API_SECRET must be set in environment"),
            },
        }
    }
}

#[tokio::main]
async fn main() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "example=debug,tower_http=debug,axum=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let state = AppState::from_env();

    let app = Router::new()
        .route("/health", get(health::check))
        .merge(equity::router())
        .with_state(state)
        .layer(TraceLayer::new_for_http());

    let listener = tokio::net::TcpListener::bind("0.0.0.0:8089").await.unwrap();

    axum::serve(listener, app).await.unwrap();
}
