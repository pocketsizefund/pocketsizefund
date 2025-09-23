use crate::state::State;
use crate::storage::{query_portfolio_dataframe_from_s3, write_portfolio_dataframe_to_s3};
use axum::{
    extract::{Json, State as AxumState},
    http::StatusCode,
    response::IntoResponse,
};
use chrono::{DateTime, Utc};
use polars::prelude::*;
use serde::Deserialize;
use tracing::info;

#[derive(Deserialize)]
pub struct SavePortfolioPayload {
    pub data: DataFrame,
    pub timestamp: DateTime<Utc>,
}

pub async fn save(
    AxumState(state): AxumState<State>,
    Json(payload): Json<SavePortfolioPayload>,
) -> impl IntoResponse {
    let portfolio = payload.data;

    let timestamp = payload.timestamp;

    match write_portfolio_dataframe_to_s3(&state, &portfolio, &timestamp).await {
        Ok(s3_key) => {
            info!("Successfully uploaded DataFrame to S3 at key: {}", s3_key);
            let response_message = format!(
                "DataFrame created with {} rows and uploaded to S3: {}",
                portfolio.height(),
                s3_key
            );

            (StatusCode::OK, response_message).into_response()
        }
        Err(err) => {
            info!("Failed to upload to S3: {}", err);
            let json_output = portfolio.to_string();

            (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("S3 upload failed: {}\n\n{}", err, json_output),
            )
                .into_response()
        }
    }
}

pub async fn get(AxumState(state): AxumState<State>) -> impl IntoResponse {
    info!("Fetching portfolio from S3");

    match query_portfolio_dataframe_from_s3(&state, &Utc::now()).await {
        Ok(dataframe) => (StatusCode::OK, dataframe.to_string()).into_response(),
        Err(err) => {
            info!("Failed to fetch portfolio from S3: {}", err);
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("Failed to fetch portfolio: {}", err),
            )
                .into_response()
        }
    }
}
