use crate::state::State;
use crate::storage::{query_portfolio_dataframe_from_s3, write_portfolio_dataframe_to_s3};
use axum::{
    extract::{Json, Query, State as AxumState},
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
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("S3 upload failed: {}", err),
            )
                .into_response()
        }
    }
}

#[derive(Deserialize)]
pub struct QueryParameters {
    timestamp: Option<DateTime<Utc>>,
}

pub async fn get(
    AxumState(state): AxumState<State>,
    Query(parameters): Query<QueryParameters>,
) -> impl IntoResponse {
    info!("Fetching portfolio from S3");

    let timestamp: Option<DateTime<Utc>> = parameters.timestamp;

    match query_portfolio_dataframe_from_s3(&state, timestamp).await {
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
