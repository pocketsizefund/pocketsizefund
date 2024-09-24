use actix_web::post;
use cloudevents::Event;

#[post("/")]
pub async fn handler(event: Event) -> Event {
    event
}
