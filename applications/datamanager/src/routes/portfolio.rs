use crate::AppState;
use aws_credential_types::provider::error::CredentialsError;
use aws_credential_types::provider::ProvideCredentials;
use aws_sdk_s3::primitives::ByteStream;
use axum::{
    extract::{Json, State},
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
    Router,
};
use chrono::{DateTime, Utc};
use duckdb::{Connection, Error as DuckError};
use polars::prelude::*;
use serde::Deserialize;
use std::io::Cursor;
use thiserror::Error as ThisError;
use tracing::{debug, info};

#[derive(ThisError, Debug)]
enum Error {
    #[error("DuckDB error: {0}")]
    DuckDB(#[from] DuckError),
    #[error("Credentials error: {0}")]
    Credentials(#[from] CredentialsError),
    #[error("Other error: {0}")]
    Other(String),
}

#[derive(Deserialize)]
struct SavePortfolioPayload {
    data: DataFrame,
    timestamp: DateTime<Utc>,
}

#[derive(Deserialize)]
struct Portfolio {
    ticker: String,
    timestamp: i64,
    side: String,
    dollar_amount: f64,
}

async fn save_portfolio(
    State(state): State<AppState>,
    Json(payload): Json<SavePortfolioPayload>,
) -> impl IntoResponse {
    let portfolio = payload.data;

    let timestamp = payload.timestamp;

    match upload_dataframe_to_s3(&state, &portfolio, &timestamp).await {
        Ok(s3_key) => {
            info!("Successfully uploaded DataFrame to S3 at key: {}", s3_key);
            let response_message = format!(
                "DataFrame created with {} rows and uploaded to S3: {}",
                portfolio.height(),
                s3_key
            );

            (StatusCode::OK, response_message)
        }
        Err(err) => {
            info!("Failed to upload to S3: {}", err);
            let json_output = portfolio.to_string();

            (
                StatusCode::INTERNAL_SERVER_ERROR,
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
    info!("Uploading portfolio DataFrame to S3 as parquet");

    let year = date.format("%Y");
    let month = date.format("%m");
    let day = date.format("%d");

    let key = format!(
        "equity/portfolios/daily/year={}/month={}/day={}/data.parquet",
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
                return Err(Error::Other(format!("Failed to write parquet: {}", err)));
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
        Err(err) => Err(Error::Other(format!("Failed to upload to S3: {}", err))),
    }
}

async fn get_portfolio(State(state): State<AppState>) -> impl IntoResponse {
    info!("Fetching portfolio from S3");

    match query_s3_parquet_data(&state, &Utc::now()).await {
        Ok(df) => {
            let json_output = df.to_string();
            (StatusCode::OK, json_output)
        }
        Err(err) => {
            info!("Failed to query S3 parquet data: {}", err);
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("Failed to fetch portfolio: {}", err),
            )
        }
    }
}

async fn query_s3_parquet_data(
    state: &AppState,
    timestamp: &DateTime<Utc>,
) -> Result<DataFrame, Error> {
    let connection = Connection::open_in_memory()?;

    connection.execute_batch("INSTALL httpfs; LOAD httpfs;")?;

    let config = aws_config::load_defaults(aws_config::BehaviorVersion::latest()).await;
    let provider = config
        .credentials_provider()
        .ok_or_else(|| Error::Other("No AWS credentials provider found".into()))?;
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

    let year = timestamp.format("%Y");
    let month = timestamp.format("%m");
    let day = timestamp.format("%d");

    let s3_path = format!(
        "s3://{}/equity/portfolios/daily/year={}/month={}/day={}/data.parquet",
        state.bucket_name, year, month, day
    );

    info!("Querying 1 S3 file");

    let s3_paths_query = format!("SELECT * FROM '{}'", s3_path);

    let query = format!(
        "
        SELECT
            ticker,
            timestamp,
            side,
            dollar_amount
        FROM ({})
        ORDER BY timestamp, ticker
        LIMIT 1
        ",
        s3_paths_query,
    );

    debug!("Executing export SQL: {}", query);

    let mut statement = connection.prepare(&query)?;

    let portfolios_iterator = statement.query_map([], |row| {
        Ok(Portfolio {
            ticker: row.get(0)?,
            timestamp: row.get(1)?,
            side: row.get(2)?,
            dollar_amount: row.get(3)?,
        })
    })?;

    let portfolios: Vec<Portfolio> = portfolios_iterator.filter_map(Result::ok).collect();

    df!(
        "ticker" => portfolios.iter().map(|p| p.ticker.clone()).collect::<Vec<_>>(),
        "timestamp" => portfolios.iter().map(|p| p.timestamp).collect::<Vec<_>>(),
        "side" => portfolios.iter().map(|p| p.side.clone()).collect::<Vec<_>>(),
        "dollar_amount" => portfolios.iter().map(|p| p.dollar_amount).collect::<Vec<_>>(),
    )
    .map_err(|e| Error::Other(format!("Failed to create DataFrame: {}", e)))
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/portfolio", post(save_portfolio))
        .route("/portfolio", get(get_portfolio))
}
