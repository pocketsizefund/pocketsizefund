use tracing::{debug, error, info, warn};

use chrono::NaiveDate;

use axum::{
    Router,
    extract::{Json, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::{get, post},
};

use crate::AppState;

#[derive(serde::Deserialize)]
struct DailySync {
    date: NaiveDate,
}

async fn fetch(State(state): State<AppState>) -> Response {
    info!("hello");
    (StatusCode::OK).into_response()
}

async fn sync(State(state): State<AppState>, payload: Json<DailySync>) -> Response {
    info!("name: {}", payload.date);
    let url = format!(
        "{}/v2/aggs/grouped/locale/us/market/stocks/{}",
        state.polygon.base, payload.date
    );
    info!("url: {}", url);
    let response = state
        .client
        .get(url)
        .header("accept", "application/json")
        .query(&[("adjusted", "true"), ("apiKey", state.polygon.key.as_str())])
        .send()
        .await
        .expect("failed to send request");

    let body = response.text().await.expect("failed to read response");
    // .json()
    // .await
    // .expect("failed to parse response");
    info!("response: {}", body);
    (StatusCode::OK).into_response()
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/equity", get(fetch))
        .route("/equity", post(sync))
}
