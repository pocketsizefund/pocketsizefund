use actix_web::post;
use cloudevents::Event;
use pocketsizefund::trade::Market;
use pocketsizefund::events::build_response_event;
use serde_json::json;

#[post("/market-status")]
pub async fn market_status_check(_event: Event) -> Event {
    let mut market = Market::default();

    market.check_current_status().await;

    tracing::info!(
        "{}",
        serde_json::json!({
            "market_status": market.status,
            "next_open": market.next_open,
            "next_close": market.next_close
        })
    );

    let event = build_response_event(
        "chronos".to_string(),
        vec!["market".to_string(), "status".to_string(), "updated".to_string()],
        Some(json!({
            "status": market.status.to_string(),
            "next_open": market.next_open,
            "next_close": market.next_close
        }).to_string()),
    );

    event
}
