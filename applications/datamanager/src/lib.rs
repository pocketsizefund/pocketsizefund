use aws_sdk_s3::Client as S3Client;
use axum::{routing::get, Router};
use reqwest::Client;
use tower_http::trace::TraceLayer;

pub mod routes;
use routes::equity;
use routes::health;
use routes::prediction;

#[derive(Clone)]
pub struct AlpacaSecrets {
    pub base: String,
    pub key: String,
    pub secret: String,
}

#[derive(Clone)]
pub struct PolygonSecrets {
    pub base: String,
    pub key: String,
}

#[derive(Clone)]
pub struct AppState {
    pub client: Client,
    pub polygon: PolygonSecrets,
    pub alpaca: AlpacaSecrets,
    pub s3_client: S3Client,
    pub bucket_name: String,
}

impl AppState {
    pub async fn from_env() -> Self {
        let client = Client::builder()
            .timeout(std::time::Duration::from_secs(10))
            .build()
            .unwrap();

        let config = aws_config::load_defaults(aws_config::BehaviorVersion::latest()).await;
        let s3_client = S3Client::new(&config);
        let bucket_name =
            std::env::var("S3_BUCKET_NAME").unwrap_or("pocketsizefund-ml-data".to_string());

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
            s3_client,
            bucket_name,
        }
    }
}

pub async fn create_app() -> Router<AppState> {
    let state = AppState::from_env().await;

    Router::<AppState>::new()
        .route("/health", get(health::check))
        .merge(prediction::router())
        .merge(equity::router())
        .with_state(state)
        .layer(TraceLayer::new_for_http())
}
