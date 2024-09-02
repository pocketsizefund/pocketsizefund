use actix_web::post;
use cloudevents::Event;
use pocketsizefund::events::Market;

#[post("/")]
pub async fn market_status_check(_event: Event) -> Event {
    let mut market = Market::default();
    market.check_current_status().await;
    market.to_event().await
}
