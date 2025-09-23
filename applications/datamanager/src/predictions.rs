use crate::state::State;
use crate::storage::{
    query_predictions_dataframe_from_s3, write_predictions_dataframe_to_s3, PredictionQuery,
};
use axum::{
    extract::{Json, Query, State as AxumState},
    http::StatusCode,
    response::IntoResponse,
};
use chrono::{DateTime, Utc};
use polars::prelude::*;
use serde::Deserialize;
use serde_json;
use tracing::info;
use urlencoding::decode;

#[derive(Deserialize)]
pub struct SavePredictionsPayload {
    pub data: DataFrame,
    pub timestamp: DateTime<Utc>,
}

#[derive(Deserialize)]
pub struct QueryPredictionsParameters {
    pub data: String, // URL-encoded JSON string
}

pub async fn save(
    AxumState(state): AxumState<State>,
    Json(payload): Json<SavePredictionsPayload>,
) -> impl IntoResponse {
    let predictions = payload.data;

    let timestamp = payload.timestamp;

    match write_predictions_dataframe_to_s3(&state, &predictions, &timestamp).await {
        Ok(s3_key) => {
            info!("Successfully uploaded DataFrame to S3 at key: {}", s3_key);
            let response_message = format!(
                "DataFrame created with {} rows and uploaded to S3: {}",
                predictions.height(),
                s3_key
            );

            (StatusCode::OK, response_message).into_response()
        }
        Err(err) => {
            info!("Failed to upload to S3: {}", err);
            let json_output = predictions.to_string();

            (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("S3 upload failed: {}\n\n{}", err, json_output),
            )
                .into_response()
        }
    }
}

pub async fn query(
    AxumState(state): AxumState<State>,
    Query(parameters): Query<QueryPredictionsParameters>,
) -> impl IntoResponse {
    info!("Fetching predictions from S3");

    let decoded = match decode(&parameters.data) {
        Ok(decoded) => decoded.into_owned(),
        Err(e) => {
            return (
                StatusCode::BAD_REQUEST,
                format!("Failed to decode query parameter: {}", e),
            )
                .into_response();
        }
    };

    let predictions_query: Vec<PredictionQuery> = match serde_json::from_str(&decoded) {
        Ok(query) => query,
        Err(e) => {
            return (
                StatusCode::BAD_REQUEST,
                format!("Failed to parse JSON: {}", e),
            )
                .into_response();
        }
    };

    match query_predictions_dataframe_from_s3(&state, predictions_query).await {
        Ok(dataframe) => (StatusCode::OK, dataframe.to_string()).into_response(),
        Err(err) => {
            info!("Failed to query S3 data: {}", err);
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("Query failed: {}", err),
            )
                .into_response()
        }
    }
}
