use tracing::{debug, info};

use aws_sdk_s3::primitives::ByteStream;
use chrono::NaiveDate;
use duckdb::{Connection, Result as DuckResult};
use polars::prelude::ParquetWriter;
use polars::prelude::*;

use std::io::Cursor;

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
    queryCount: u64,
    request_id: String,
    resultsCount: u64,
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

async fn fetch(State(_state): State<AppState>) -> Response {
    info!("hello");
    (StatusCode::OK).into_response()
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
            return (
                StatusCode::NO_CONTENT,
                "No market data available for this date",
            )
                .into_response();
        }
    };

    let bars: Vec<BarResult> = match serde_json::from_value(results_array.clone()) {
        Ok(bars) => bars,
        Err(err) => {
            info!("Failed to parse results into BarResult structs: {}", err);
            return (StatusCode::BAD_GATEWAY, raw_text).into_response();
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
        "volume" => volumes,
        "vwap" => vw_prices,
        "open" => open_prices,
        "close" => close_prices,
        "high" => high_prices,
        "low" => low_prices,
        "timestamp" => timestamps,
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
                        StatusCode::BAD_GATEWAY,
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
            (StatusCode::INTERNAL_SERVER_ERROR, raw_text).into_response()
        }
    }
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/equity", get(fetch))
        .route("/equity", post(sync))
}
