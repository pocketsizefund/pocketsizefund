use crate::tickers::get_dow_jones_tickers;
use crate::Interface;
use actix_web::{post, web};
use chrono::Utc;
use cloudevents::{Event, EventBuilder, EventBuilderV10};
use log::info;
use pocketsizefund::data::Client as DataClient;
use serde_json::json;
use tracing;
use uuid::Uuid;

#[post("/data")]
pub async fn handler(_event: Event, data_client: web::Data<DataClient>) -> Event {
    info!("data handler called");

    let old_bars = data_client.load_equities_bars().await.unwrap();

    info!("old bars count: {}", old_bars.len());

    let most_recent_datetime = old_bars.iter().max_by_key(|bar| bar.timestamp).unwrap();

    let current_datetime = chrono::Utc::now();

    info!(
        "most recent datetime: {}, current datetime: {}",
        most_recent_datetime.timestamp, current_datetime
    );

    let dow_jones_tickers = get_dow_jones_tickers();

    let new_bars = data_client
        .fetch_equities_bars(
            dow_jones_tickers,
            most_recent_datetime.timestamp,
            current_datetime,
        )
        .await
        .unwrap();

    info!("new bars count: {}", new_bars.len());

    data_client.write_equities_bars(new_bars).await.unwrap();

    EventBuilderV10::new()
        .id(Uuid::new_v4().to_string())
        .ty("data.equities.bars.updated")
        .source("psf.platform.datacollector")
        .data(
            "application/cloudevents+json",
            json!({
                "status": "success".to_string(),
            }),
        )
        .extension("timestamp", Utc::now().to_rfc3339().to_string())
        .build()
        .unwrap_or_else(|e| {
            tracing::error!("Failed to build event: {}", e);
            Event::default()
        })
}
