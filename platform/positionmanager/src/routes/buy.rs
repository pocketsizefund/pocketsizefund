use crate::config::AlpacaConfig;
use actix_web::{post, web};
use apca::api::v2::order::{Order, Side, TimeInForce, Type};
use cloudevents::{Event, EventBuilder, EventBuilderV10};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct OrderRequestEvent {
    pub symbol: String,
    pub qty: f64,
    pub side: Side,
    #[serde(rename = "type")]
    pub _type: Type,
    pub time_in_force: TimeInForce,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OrderSubmttedEvent {
    pub id: String,
    pub symbol: String,
    pub qty: f64,
    pub side: Side,
    #[serde(rename = "type")]
    pub _type: Type,
    pub time_in_force: TimeInForce,
}

#[post("/buy")]
async fn event_handler(request: Event, config: web::Data<AlpacaConfig>) -> Event {
    //let event = match request.data() {
    //    Some(data) => match serde_json::from_str::<OrderRequestEvent>(data) {
    //        Ok(data) => data,
    //        Err(e) => {
    //            tracing::error!("Failed to parse data: {:?}", e);
    //            return request;
    //        }
    //    },
    //    None => {
    //        tracing::error!("No data provided");
    //        return request;
    //    }
    //};
    //
    tracing::info!("Received buy request: {:?}", request);

    request

    //let order_request = OrderReq {
    //    symbol: event.symbol,
    //    qty: event.qty,
    //    side: event.side,
    //    type: event._type,
    //    time_in_force: event.time_in_force,
    //    ..OrderReq::default()
    //}
    //
    //match client.issue::<orders::List>(&config.alpaca_api_key) {
    //    Ok(result) => EventBuilderV10::new()
    //        .id("baseline-buy-event".to_string())
    //        .ty("psf.positionmanager.baseline-buy")
    //        .source("platform:positionmanager")
    //        .data("application/json", result)
    //        .build()
    //        .source("platform:positionmanager")
    //        .data("application/json", e)
    //        .build()
    //        .unwrap(),
    //}
}
