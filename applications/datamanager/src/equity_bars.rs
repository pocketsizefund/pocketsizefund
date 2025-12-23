use crate::state::State;
use crate::storage::{query_equity_bars_parquet_from_s3, write_equity_bars_dataframe_to_s3};
use axum::{
    body::Body,
    extract::{Json, Query, State as AxumState},
    http::{header, StatusCode},
    response::{IntoResponse, Response},
};
use chrono::{DateTime, Utc};
use polars::prelude::*;
use serde::Deserialize;
use tracing::{debug, info};

#[derive(Deserialize)]
pub struct DailySync {
    pub date: DateTime<Utc>,
}

#[derive(Deserialize)]
pub struct QueryParameters {
    tickers: Option<String>,
    start_timestamp: Option<DateTime<Utc>>,
    end_timestamp: Option<DateTime<Utc>>,
}

#[derive(Deserialize, Debug)]
struct BarResult {
    #[serde(rename = "T")]
    ticker: String,
    c: Option<u64>,
    h: Option<u64>,
    l: Option<u64>,
    n: Option<u64>,
    o: Option<u64>,
    t: u64,
    v: Option<u64>,
    vw: Option<u64>,
}

#[derive(Deserialize, Debug)]
#[allow(dead_code)]
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

pub async fn query(
    AxumState(state): AxumState<State>,
    Query(parameters): Query<QueryParameters>,
) -> impl IntoResponse {
    info!("Querying equity data from S3 partitioned files");

    let tickers: Option<Vec<String>> = match &parameters.tickers {
        Some(tickers_str) if !tickers_str.is_empty() => {
            let vec: Vec<String> = tickers_str
                .split(',')
                .map(|s| s.trim().to_uppercase())
                .collect();
            if vec.is_empty() {
                None
            } else {
                Some(vec)
            }
        }
        _ => None,
    };

    match query_equity_bars_parquet_from_s3(
        &state,
        tickers,
        parameters.start_timestamp,
        parameters.end_timestamp,
    )
    .await
    {
        Ok(parquet_data) => {
            let mut response = Response::new(Body::from(parquet_data));
            response.headers_mut().insert(
                header::CONTENT_TYPE,
                "application/octet-stream".parse().unwrap(),
            );
            response.headers_mut().insert(
                "Content-Disposition",
                "attachment; filename=\"equity_data.parquet\""
                    .parse()
                    .unwrap(),
            );
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

pub async fn sync(
    AxumState(state): AxumState<State>,
    Json(payload): Json<DailySync>,
) -> impl IntoResponse {
    info!("Sync date: {}", payload.date);
    let date = payload.date.format("%Y-%m-%d").to_string();
    let url = format!(
        "{}/v2/aggs/grouped/locale/us/market/stocks/{}",
        state.polygon.base, date
    );

    info!("url: {}", url);
    let response = match state
        .http_client
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

    let text_content = match response.error_for_status() {
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

    let json_content: serde_json::Value = match serde_json::from_str(&text_content) {
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

    let results = match json_content.get("results") {
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

    let bars: Vec<BarResult> = match serde_json::from_value(results.clone()) {
        Ok(bars) => bars,
        Err(err) => {
            info!("Failed to parse results into BarResult structs: {}", err);
            return (StatusCode::BAD_GATEWAY, text_content).into_response();
        }
    };

    let tickers: Vec<String> = bars.iter().map(|b| b.ticker.clone()).collect();
    let volumes: Vec<Option<u64>> = bars.iter().map(|b| b.v).collect();
    let volume_weighted_average_prices: Vec<Option<f64>> =
        bars.iter().map(|b| b.vw.map(|vw| vw as f64)).collect();
    let open_prices: Vec<Option<f64>> = bars.iter().map(|b| b.o.map(|o| o as f64)).collect();
    let close_prices: Vec<Option<f64>> = bars.iter().map(|b| b.c.map(|c| c as f64)).collect();
    let high_prices: Vec<Option<f64>> = bars.iter().map(|b| b.h.map(|h| h as f64)).collect();
    let low_prices: Vec<Option<f64>> = bars.iter().map(|b| b.l.map(|l| l as f64)).collect();
    let timestamps: Vec<i64> = bars.iter().map(|b| b.t as i64).collect();
    let transactions: Vec<Option<u64>> = bars.iter().map(|b| b.n).collect();

    let bars_data = df! {
        "ticker" => tickers,
        "timestamp" => timestamps,
        "open_price" => open_prices,
        "high_price" => high_prices,
        "low_price" => low_prices,
        "close_price" => close_prices,
        "volume" => volumes,
        "volume_weighted_average_price" => volume_weighted_average_prices,
        "transactions" => transactions,
    };

    match bars_data {
        Ok(data) => {
            info!(
                "Created DataFrame with {} rows and {} columns",
                data.height(),
                data.width()
            );
            debug!("DataFrame schema: {:?}", data.schema());

            match write_equity_bars_dataframe_to_s3(&state, &data, &payload.date).await {
                Ok(s3_key) => {
                    info!("Successfully uploaded DataFrame to S3 at key: {}", s3_key);
                    let response_message = format!(
                        "DataFrame created with {} rows and uploaded to S3: {}",
                        data.height(),
                        s3_key
                    );
                    (StatusCode::OK, response_message).into_response()
                }
                Err(err) => {
                    info!("Failed to upload to S3: {}", err);
                    let json_output = data.to_string();
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
            (StatusCode::INTERNAL_SERVER_ERROR, text_content).into_response()
        }
    }
}
