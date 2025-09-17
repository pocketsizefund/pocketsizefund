use crate::AppState;
use aws_credential_types::provider::error::CredentialsError;
use aws_credential_types::provider::ProvideCredentials;
use aws_sdk_s3::primitives::ByteStream;
use axum::{
    body::Body,
    extract::{Json, State},
    http::{header, StatusCode},
    response::{IntoResponse, Response},
    routing::{get, post},
    Router,
};
use chrono::{DateTime, Utc};
use duckdb::{Connection, Error as DuckError};
use polars::prelude::*;
use serde::Serialize;
use std::io::Cursor;
use thiserror::Error as ThisError;
use tracing::{debug, info};

#[derive(ThisError, Debug)]
enum Error {
    #[error("DuckDB error: {0}")]
    DuckDBError(#[from] DuckError),
    #[error("Credentials error: {0}")]
    CredentialsError(#[from] CredentialsError),
    #[error("Other error: {0}")]
    OtherError(String),
}

#[derive(serde::Deserialize)]
struct SavePredictionsPayload {
    data: DataFrame,
    timestamp: DateTime<Utc>,
}

#[derive(serde::Deserialize)]
struct QueryPredictionsPayload {
    positions: Vec<QueryPredictionsPositionPayload>,
    #[allow(dead_code)]
    timestamp: DateTime<Utc>,
}

#[derive(serde::Deserialize)]
struct QueryPredictionsPositionPayload {
    ticker: String,
    timestamp: DateTime<Utc>,
}

#[derive(Debug, Serialize)]
struct Prediction {
    ticker: String,
    timestamp: i64,
    quantile_10: f64,
    quantile_50: f64,
    quantile_90: f64,
}

async fn save_prediction(
    State(state): State<AppState>,
    Json(payload): Json<SavePredictionsPayload>,
) -> impl IntoResponse {
    let predictions = payload.data;

    let timestamp = payload.timestamp;

    match upload_dataframe_to_s3(&state, &predictions, &timestamp).await {
        Ok(s3_key) => {
            info!("Successfully uploaded DataFrame to S3 at key: {}", s3_key);
            let response_message = format!(
                "DataFrame created with {} rows and uploaded to S3: {}",
                predictions.height(),
                s3_key
            );

            (StatusCode::OK, response_message)
        }
        Err(err) => {
            info!("Failed to upload to S3: {}", err);
            let json_output = predictions.to_string();

            (
                StatusCode::OK,
                format!("S3 upload failed: {}\n\n{}", err, json_output),
            )
        }
    }
}

async fn upload_dataframe_to_s3(
    state: &AppState,
    dataframe: &DataFrame,
    date: &DateTime<Utc>,
) -> Result<String, Error> {
    info!("Uploading predictions DataFrame to S3 as parquet");

    let year = date.format("%Y");
    let month = date.format("%m");
    let day = date.format("%d");

    let key = format!(
        "equity/predictions/daily/year={}/month={}/day={}/data.parquet",
        year, month, day,
    );

    let mut buffer = Vec::new();
    {
        let cursor = Cursor::new(&mut buffer);
        let writer = ParquetWriter::new(cursor);
        match writer.finish(&mut dataframe.clone()) {
            Ok(_) => {
                println!(
                    "DataFrame successfully converted to parquet, size: {} bytes",
                    buffer.len()
                );
            }
            Err(err) => {
                return Err(Error::OtherError(format!(
                    "Failed to write parquet: {}",
                    err
                )));
            }
        }
    }

    let body = ByteStream::from(buffer);

    match state
        .s3_client
        .put_object()
        .bucket(&state.bucket_name)
        .key(&key)
        .body(body)
        .content_type("application/octet-stream")
        .send()
        .await
    {
        Ok(_) => {
            info!(
                "Successfully uploaded parquet file to s3://{}/{}",
                state.bucket_name, key
            );
            Ok(key)
        }
        Err(err) => Err(Error::OtherError(format!(
            "Failed to upload to S3: {}",
            err
        ))),
    }
}

async fn query_prediction(
    State(state): State<AppState>,
    Json(payload): Json<QueryPredictionsPayload>,
) -> impl IntoResponse {
    info!("Fetching equity data from S3 partitioned files");

    match query_s3_parquet_data(&state, payload.positions).await {
        Ok(dataframe) => {
            let json_string = dataframe.to_string();
            let mut response = Response::new(Body::from(json_string));
            response
                .headers_mut()
                .insert(header::CONTENT_TYPE, "application/json".parse().unwrap());
            *response.status_mut() = StatusCode::OK;
            response
        }
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

async fn query_s3_parquet_data(
    state: &AppState,
    positions: Vec<QueryPredictionsPositionPayload>,
) -> Result<DataFrame, Error> {
    let connection = Connection::open_in_memory()?;

    connection.execute_batch("INSTALL httpfs; LOAD httpfs;")?;

    let config = aws_config::load_defaults(aws_config::BehaviorVersion::latest()).await;
    let provider = config
        .credentials_provider()
        .ok_or_else(|| Error::OtherError("No AWS credentials provider found".into()))?;
    let credentials = provider.provide_credentials().await?;
    let region = config
        .region()
        .map(|r| r.as_ref().to_string())
        .unwrap_or_else(|| "us-east-1".to_string());
    let session_token = credentials.session_token().unwrap_or_default();
    let s3_config = format!(
        "
            SET s3_region='{}';
            SET s3_url_style='path';
            SET s3_access_key_id='{}';
            SET s3_secret_access_key='{}';
            SET s3_session_token='{}';
        ",
        region,
        credentials.access_key_id(),
        credentials.secret_access_key(),
        session_token
    );

    connection.execute_batch(&s3_config)?;

    let mut s3_paths = Vec::new();
    let mut tickers = Vec::new();

    for position in positions {
        let year = position.timestamp.format("%Y");
        let month = position.timestamp.format("%m");
        let day = position.timestamp.format("%d");

        let s3_path = format!(
            "s3://{}/equity/predictions/daily/year={}/month={}/day={}/data.parquet",
            state.bucket_name, year, month, day
        );

        s3_paths.push(s3_path);

        tickers.push(position.ticker);
    }

    info!("Querying {} S3 files", s3_paths.len());

    let s3_paths_query = s3_paths
        .iter()
        .map(|path| format!("SELECT * FROM '{}'", path))
        .collect::<Vec<_>>()
        .join(" UNION ALL ");

    let tickers_query = tickers
        .iter()
        .map(|ticker| format!("'{}'", ticker))
        .collect::<Vec<_>>()
        .join(", ");

    let query = format!(
        "
        SELECT
            ticker,
            timestamp,
            quantile_10,
            quantile_50,
            quantile_90
        FROM ({})
        WHERE ticker IN ({})
        ORDER BY timestamp, ticker
        ",
        s3_paths_query, tickers_query,
    );

    debug!("Executing export SQL: {}", query);

    let mut statement = connection.prepare(&query)?;

    let predictions_iterator = statement.query_map([], |row| {
        Ok(Prediction {
            ticker: row.get(0)?,
            timestamp: row.get(1)?,
            quantile_10: row.get(2)?,
            quantile_50: row.get(3)?,
            quantile_90: row.get(4)?,
        })
    })?;

    let predictions: Vec<Prediction> = predictions_iterator
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| Error::OtherError(format!("Failed to collect predictions: {}", e)))?;

    df!(
        "ticker" => predictions.iter().map(|p| p.ticker.as_str()).collect::<Vec<_>>(),
        "timestamp" => predictions.iter().map(|p| p.timestamp).collect::<Vec<_>>(),
        "quantile_10" => predictions.iter().map(|p| p.quantile_10).collect::<Vec<_>>(),
        "quantile_50" => predictions.iter().map(|p| p.quantile_50).collect::<Vec<_>>(),
        "quantile_90" => predictions.iter().map(|p| p.quantile_90).collect::<Vec<_>>(),
    )
    .map_err(|e| Error::OtherError(format!("Failed to create DataFrame: {}", e)))
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/predictions", post(save_prediction))
        .route("/predictions", get(query_prediction))
}
