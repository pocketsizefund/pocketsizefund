//use actix_web::{post, web};
//use apca::api::v2::orders;
//use cloudevents::{Event, EventBuilder, EventBuilderV10};
//use positionmanager::config::AlpacaConfig;
//use serde::{Deserialize, Serialize};
//use serde_json::json;
//
////#[post("/baseline_buy")]
////async fn event_handler(request: Event, config: web::Data<AlpacaConfig>) -> Event {
////    let event = match request.data() {
////        Some(data) => data.to_string(),
////        None => {
////            tracing::error!("No data provided");
////            return event;
////        }
////    };
////
////    let client = orders::ListReq {
////        status: orders::Status::Closed,
////        limit: Some(100),
////        _non_exhaustive: (),
////        symbols: vec!["JNJ".to_string()],
////        nested: false,
////    };
////
////    match client.issue_order(&order_request).await {
////        Ok(result) => {
////            tracing::info!("order placed successfully");
////            tracing::info!("order ID: {}", order.id);
////        }
////    }
////}
//
////pub async fn buy_order_submitted_event() -> Event {
////    Ok(result) => EventBuilderV10::new()
////            .id("baseline-buy-event".to_string())
////            .ty("psf.positionmanager.baseline-buy")
////            .source("platform:positionmanager")
////            .data("application/json", result)
////            .build()
////            .source("platform:positionmanager")
////            .data("application/json", e)
////            .build()
////            .unwrap(),
////}
