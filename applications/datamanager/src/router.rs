use crate::equity_bars;
use crate::health;
use crate::portfolios;
use crate::predictions;
use crate::state::State;
use axum::{
    routing::{get, post},
    Router,
};
use tower_http::trace::TraceLayer;

pub async fn create_app() -> Router {
    let state = State::from_env().await;

    Router::new()
        .route("/health", get(health::get_health))
        .route("/predictions", post(predictions::save))
        .route("/predictions", get(predictions::query))
        .route("/portfolios", post(portfolios::save))
        .route("/portfolios", get(portfolios::get))
        .route("/equity-bars", post(equity_bars::sync))
        .route("/equity-bars", get(equity_bars::fetch))
        .with_state(state)
        .layer(TraceLayer::new_for_http())
}
