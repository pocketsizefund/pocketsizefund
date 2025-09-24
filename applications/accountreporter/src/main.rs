use accountreporter::run_account_monitor;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[tokio::main]
async fn main() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "accountreporter=info,rdkafka=warn".into()),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .json()
                .with_current_span(false)
                .with_span_list(false)
        )
        .init();

    tracing::info!("Starting accountreporter service");

    if let Err(e) = run_account_monitor().await {
        tracing::error!("Account monitor failed: {}", e);
        std::process::exit(1);
    }
}
