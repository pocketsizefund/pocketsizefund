use actix_web::post;
use cloudevents::Event;
use pocketsizefund::events::Market;

#[post("/")]
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
    market.to_event().await
}
