use actix_web::post;
use cloudevents::Event;
use pocketsizefund::events::Market;

#[post("/")]
pub async fn handler(event: Event) -> Event {
    event
}
