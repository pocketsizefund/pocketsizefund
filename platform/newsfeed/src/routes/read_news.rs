use actix_web::post;
use cloudevents::{Event, EventBuilder, EventBuilderV10};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use url::Url;

#[derive(Debug, Deserialize)]
struct CloudEvent<T> {
    specversion: String,
    ty: String,
    source: String,
    id: String,
    data: T,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct NewsSource {
    source_url: String,
}

impl NewsSource {
    pub fn new(source_url: String) -> Self {
        let source_url = Url::parse(&source_url)
            .expect("Failed to parse source url")
            .to_string();
        Self { source_url }
    }
}

#[post("/news")]
pub async fn event_handler(request: Event) -> Event {
    let event: CloudEvent<Value> = serde_json::from_str(&request.to_string()).unwrap();
    let data: NewsSource = serde_json::from_value(event.data).unwrap();
    tracing::info!("Received news source: {:#?}", data);
    //let message = match request.data() {
    //    Some(data) => data.to_string(),
    //    None => {
    //        tracing::error!("No data provided");
    //        return newsreader_error_event().await;
    //    }
    //};
    //
    //tracing::info!("{}", message.as_str());
    //
    //let news_source: NewsSource = match serde_json::from_str(&message) {
    //    Ok(data) => data,
    //    Err(e) => {
    //        tracing::error!("Failed to parse data: {}", e);
    //        return newsreader_error_event().await;
    //    }
    //};
    //
    //tracing::info!("Received news source: {:#?}", news_source);
    //let message: Option<NewsSource> = match request.data() {
    //    Some(data) => {
    //        let data: Option<NewsSource> = match serde_json::from_str(&data.to_string()) {
    //            Ok(data) => data,
    //            Err(e) => {
    //                tracing::error!("Failed to parse data: {}", e);
    //                None
    //            }
    //        };
    //        data
    //    }
    //    None => {
    //        tracing::error!("No data provided");
    //        None
    //    }
    //};

    //tracing::info!("Received news source: {:#?}", message);
    request
}

pub async fn newsreader_error_event() -> Event {
    let event = match EventBuilderV10::new()
        .id(uuid::Uuid::new_v4().to_string())
        .ty("psf.newsfeed.error")
        .source("platform:newsfeed")
        .data(
            "application/json+error",
            json!({"message": "Newsreader Error"}),
        )
        .build()
    {
        Ok(event) => event,
        Err(e) => {
            tracing::error!("Failed to build error event: {}", e);
            Event::default()
        }
    };
    event
}
