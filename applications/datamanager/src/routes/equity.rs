use tracing::{debug, info};
use chrono::Utc;

use aws_credential_types::provider::ProvideCredentials;
use aws_sdk_s3::primitives::ByteStream;
use chrono::NaiveDate;
use duckdb::{Connection, Result as DuckResult};
use polars::prelude::ParquetWriter;
use polars::prelude::*;

use std::io::Cursor;

use axum::{
    Router,
    extract::{Json, State},
    http::{StatusCode, header},
    response::{IntoResponse, Response},
    routing::{get, post},
    body::Body,
};

use crate::AppState;

#[derive(serde::Deserialize)]
struct DailySync {
    date: NaiveDate,
}

#[derive(serde::Deserialize)]
struct DateRangeQuery {
    start_date: NaiveDate,
    end_date: NaiveDate,
}

#[derive(serde::Deserialize, Debug)]
struct BarResult {
    #[serde(rename = "T")]
    ticker: String,
    // TODO: money types
    c: Option<f64>,
    h: Option<f64>,
    l: Option<f64>,
    n: Option<f64>,
    o: Option<f64>,
    // otc: bool,
    t: i64,
    v: Option<f64>,
    vw: Option<f64>,
}

#[derive(serde::Deserialize, Debug)]
struct PolygonResponse {
    adjusted: bool,
    #[serde(rename = "queryCount")]
    query_count: u64,
    request_id: String,
    #[serde(rename = "resultsCount")]
    results_count: u64,
    status: String,
    results: Option<Vec<BarResult>>,
}

async fn upload_dataframe_to_s3(
    df: &DataFrame,
    state: &AppState,
    date: &NaiveDate,
) -> Result<String, String> {
    let year = date.format("%Y");
    let month = date.format("%m");
    let day = date.format("%d");

    let key = format!(
        "equity/bars/daily/year={}/month={}/day={}/data.parquet",
        year, month, day
    );

    let mut buffer = Vec::new();
    {
        let cursor = Cursor::new(&mut buffer);
        let writer = ParquetWriter::new(cursor);
        match writer.finish(&mut df.clone()) {
            Ok(_) => {
                info!(
                    "DataFrame successfully converted to parquet, size: {} bytes",
                    buffer.len()
                );
            }
            Err(err) => {
                return Err(format!("Failed to write parquet: {}", err));
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
        Err(err) => Err(format!("Failed to upload to S3: {}", err)),
    }
}

async fn fetch(State(state): State<AppState>, query: Option<Json<DateRangeQuery>>) -> Response {
    info!("Fetching equity data from S3 partitioned files");

    match query_s3_parquet_data(&state, query).await {
        Ok(parquet_data) => {
            let mut response = Response::new(Body::from(parquet_data));
            response.headers_mut().insert(
                header::CONTENT_TYPE,
                "application/octet-stream".parse().unwrap(),
            );
            response.headers_mut().insert(
                "Content-Disposition",
                "attachment; filename=\"equity_data.parquet\"".parse().unwrap(),
            );
            *response.status_mut() = StatusCode::OK;
            response
        },
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
    query: Option<Json<DateRangeQuery>>,
) -> Result<Vec<u8>, String> {
    let conn = Connection::open_in_memory()
        .map_err(|e| format!("Failed to create DuckDB connection: {}", e))?;

    conn.execute_batch("INSTALL httpfs; LOAD httpfs;")
        .map_err(|e| format!("Failed to load httpfs extension: {}", e))?;

    let config = aws_config::load_defaults(aws_config::BehaviorVersion::latest()).await;
    let credentials = config
        .credentials_provider()
        .unwrap()
        .provide_credentials()
        .await
        .map_err(|e| format!("Failed to get AWS credentials: {}", e))?;

    let s3_config = format!(
        "
        SET s3_region='us-east-1';
        SET s3_url_style='path';
        SET s3_access_key_id='{}';
        SET s3_secret_access_key='{}';
    ",
        credentials.access_key_id(),
        credentials.secret_access_key()
    );

    conn.execute_batch(&s3_config)
        .map_err(|e| format!("Failed to configure S3 settings: {}", e))?;

    let (start_date, end_date) = match query {
        Some(q) => (q.start_date, q.end_date),
        None => {
            let end_date = chrono::Utc::now().naive_utc().date();
            let start_date = end_date - chrono::Duration::days(7);
            (start_date, end_date)
        }
    };

    info!("Querying data from {} to {}", start_date, end_date);

    let mut s3_paths = Vec::new();
    let mut current_date = start_date;

    while current_date <= end_date {
        let year = current_date.format("%Y");
        let month = current_date.format("%m");
        let day = current_date.format("%d");

        let s3_path = format!(
            "s3://{}/equity/bars/daily/year={}/month={}/day={}/data.parquet",
            state.bucket_name, year, month, day
        );
        s3_paths.push(s3_path);

        current_date = current_date + chrono::Duration::days(1);
    }

    if s3_paths.is_empty() {
        return Err("No files to query for the given date range".to_string());
    }

    info!("Querying {} S3 files", s3_paths.len());

    let s3_paths_str = s3_paths
        .iter()
        .map(|path| format!("SELECT * FROM '{}'", path))
        .collect::<Vec<_>>()
        .join(" UNION ALL ");

    // Create a temporary parquet file path
    let temp_file = format!("/tmp/query_result_{}.parquet", Utc::now().timestamp_micros());
    
    let export_sql = format!(
        "
        COPY (
            SELECT 
                ticker,
                timestamp,
                open_price,
                high_price,
                low_price,
                close_price,
                volume,
                volume_weighted_average_price,
                transactions
            FROM ({})
            ORDER BY timestamp, ticker
        ) TO '{}' (FORMAT PARQUET)
        ",
        s3_paths_str,
        temp_file
    );

    debug!("Executing export SQL: {}", export_sql);

    conn.execute(&export_sql, [])
        .map_err(|e| format!("Failed to execute parquet export: {}", e))?;

    // Read the parquet file into memory
    let parquet_data = std::fs::read(&temp_file)
        .map_err(|e| format!("Failed to read parquet file: {}", e))?;
    
    // Clean up temp file
    if let Err(e) = std::fs::remove_file(&temp_file) {
        info!("Failed to clean up temp file {}: {}", temp_file, e);
    }

    info!("Query exported {} bytes of parquet data", parquet_data.len());
    Ok(parquet_data)
}

async fn sync(State(state): State<AppState>, payload: Json<DailySync>) -> impl IntoResponse {
    info!("name: {}", payload.date);
    let url = format!(
        "{}/v2/aggs/grouped/locale/us/market/stocks/{}",
        state.polygon.base, payload.date
    );

    info!("url: {}", url);
    let response = match state
        .client
        .get(url)
        .header("accept", "application/json")
        .query(&[("adjusted", "true"), ("apiKey", state.polygon.key.as_str())])
        .send()
        .await
    {
        Ok(resp) => resp,
        Err(err) => {
            info!("Failed to send request: {}", err);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                "Failed to send API request",
            )
                .into_response();
        }
    };

    let raw_text = match response.error_for_status() {
        Ok(response) => match response.text().await {
            Ok(text) => text,
            Err(err) => {
                info!("Failed to read response text: {}", err);
                return (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    "Failed to read API response",
                )
                    .into_response();
            }
        },
        Err(err) => {
            info!("API request failed: {}", err);
            return (StatusCode::INTERNAL_SERVER_ERROR, "API request failed").into_response();
        }
    };

    let json_value: serde_json::Value = match serde_json::from_str(&raw_text) {
        Ok(value) => value,
        Err(err) => {
            info!("Failed to parse JSON response: {}", err);
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                "Invalid JSON response from API",
            )
                .into_response();
        }
    };

    let results_array = match json_value.get("results") {
        Some(results) => results,
        None => {
            info!("No results field found in response");
            return (StatusCode::OK, "No market data available for this date").into_response();
        }
    };

    let bars: Vec<BarResult> = match serde_json::from_value(results_array.clone()) {
        Ok(bars) => bars,
        Err(err) => {
            info!("Failed to parse results into BarResult structs: {}", err);
            return (StatusCode::OK, raw_text).into_response();
        }
    };

    let tickers: Vec<String> = bars.iter().map(|b| b.ticker.clone()).collect();
    let volumes: Vec<Option<u64>> = bars.iter().map(|b| b.v.map(|v| v as u64)).collect();
    let vw_prices: Vec<Option<f64>> = bars.iter().map(|b| b.vw).collect();
    let open_prices: Vec<Option<f64>> = bars.iter().map(|b| b.o).collect();
    let close_prices: Vec<Option<f64>> = bars.iter().map(|b| b.c).collect();
    let high_prices: Vec<Option<f64>> = bars.iter().map(|b| b.h).collect();
    let low_prices: Vec<Option<f64>> = bars.iter().map(|b| b.l).collect();
    let timestamps: Vec<i64> = bars.iter().map(|b| b.t).collect();
    let num_transactions: Vec<Option<u64>> = bars.iter().map(|b| b.n.map(|n| n as u64)).collect();

    let df_result = df! {
        "ticker" => tickers,
        "timestamp" => timestamps,
        "open_price" => open_prices,
        "high_price" => high_prices,
        "low_price" => low_prices,
        "close_price" => close_prices,
        "volume" => volumes,
        "volume_weighted_average_price" => vw_prices,
        "transactions" => num_transactions,
    };

    match df_result {
        Ok(df) => {
            info!(
                "Created DataFrame with {} rows and {} columns",
                df.height(),
                df.width()
            );
            debug!("DataFrame schema: {:?}", df.schema());

            match upload_dataframe_to_s3(&df, &state, &payload.date).await {
                Ok(s3_key) => {
                    info!("Successfully uploaded DataFrame to S3 at key: {}", s3_key);
                    let response_msg = format!(
                        "DataFrame created with {} rows and uploaded to S3: {}",
                        df.height(),
                        s3_key
                    );
                    (StatusCode::OK, response_msg).into_response()
                }
                Err(err) => {
                    info!("Failed to upload to S3: {}", err);
                    let json_output = df.to_string();
                    (
                        StatusCode::OK,
                        format!(
                            "DataFrame created but S3 upload failed: {}\n\n{}",
                            err, json_output
                        ),
                    )
                        .into_response()
                }
            }
        }
        Err(err) => {
            info!("Failed to create DataFrame: {}", err);
            (StatusCode::OK, raw_text).into_response()
        }
    }
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/equity", get(fetch))
        .route("/equity", post(sync))
}
