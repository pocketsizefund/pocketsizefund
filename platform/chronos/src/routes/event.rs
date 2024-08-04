use crate::AppState;
use actix_web::post;
use actix_web::web::Data;
use apca::api::v2::clock::Get;
use cloudevents::{Event, EventBuilder, EventBuilderV10};
use log;
use serde::{Deserialize, Serialize};
use serde_json::json;
use tracing::error;
use uuid::Uuid;

#[derive(Debug, Serialize, Deserialize)]
struct ResponseMessage {
    message: String,
    status: String,
}

#[post("/market-status")]
async fn market_status_check(_event: Event, app_state: Data<AppState>) -> Event {
    let clock_result = app_state.alpaca_client.issue::<Get>(&()).await;
    let message = match clock_result {
        Ok(clock) => match clock.open {
            true => ResponseMessage {
                message: "market is open".to_string(),
                status: "success".to_string(),
            },
            false => ResponseMessage {
                message: "market is closed".to_string(),
                status: "success".to_string(),
            },
        },
        Err(err) => {
            error!("Failed to check market status: {}", err);
            ResponseMessage {
                message: format!("failed to check market status: {}", err),
                status: "error".to_string(),
            }
        }
    };

    build_response_event(&message)
}

fn build_response_event(response: &ResponseMessage) -> Event {
    EventBuilderV10::new()
        .id(Uuid::new_v4().to_string())
        .ty("chronos.webhook.response")
        .source("platform:chronos")
        .data("application/cloudevents+json", json!(response))
        .extension("timestamp", chrono::Utc::now().to_rfc3339())
        .build()
        .map_or_else(
            |e| {
                log::error!("Failed to build event: {}", e);
                Event::default()
            },
            |event| event,
        )
}
