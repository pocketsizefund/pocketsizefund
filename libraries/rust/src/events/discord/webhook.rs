/// Discord Webhook Events
// use crate::events;
// use bon::{bon, builder};
// use cloudevents::{Event, EventBuilder, EventBuilderV10};
// use serde;
// use serde::{Deserialize, Serialize};
// use serde_json::json;
//
// #[derive(Debug, Serialize, Deserialize, PartialEq)]
// pub struct DiscordMessage {
//     pub message: String,
// }
//
// /// Request to send a message to discord via webhook,
// /// generally to a notification channel
// #[builder]
// pub struct Request {
//     /// Meta data about the event
//     #[builder(default = events::Event::builder()
//         .source("psf.platform.discord")
//         .ty("psf.discord.message.request")
//         .build())
//     ]
//     pub meta: events::Meta,
//     /// Data to send to discord
//     #[builder]
//     pub data: Option<DiscordMessage>,
// }
//
// #[bon]
// impl Request {
//     /// Receive a discord message request event
//     #[builder]
//     pub fn receive(&mut self, data: DiscordMessage) -> Self {
//         let meta = self.meta.clone();
//         Self {
//             meta,
//             data: Some(data),
//         }
//     }
//
//     /// Publish an event to send a user-defined message to discord via webhook
//     #[builder]
//     pub fn send(self, message: String) -> Event {
//         EventBuilderV10::new()
//             .id(self.meta.id)
//             .ty(self.meta.ty)
//             .source(self.meta.source)
//             .data("application/json", json!({"message": message}))
//             .build()
//             .unwrap()
//     }
// }
//
// /// Response from discord upon sending a webhook
// pub struct Response {
//     meta: events::Meta,
// }
//
// impl Default for Response {
//     fn default() -> Self {
//         Self {
//             meta: events::Meta::builder()
//                 .source("psf.platform.discord")
//                 .ty("psf.discord.message.response")
//                 .build(),
//         }
//     }
// }
//
// /// Status of a message sent to discord via webhook
// #[derive(Debug, Serialize, Deserialize, PartialEq)]
// #[serde(rename_all = "lowercase")]
// pub enum Status {
//     /// Webhook was successful
//     Success,
//     /// Webhook failed
//     Failed,
//     /// Webhook status is unknown
//     Unknown,
// }
//
// impl Response {
//     /// Publish an event about the status of a message sent to discord via webhook
//     pub fn send(&self, status: Status) -> Event {
//         let meta = self.meta.clone();
//         let event = EventBuilderV10::new()
//             .id(meta.id)
//             .ty(meta.ty)
//             .source(meta.source)
//             .data("application/json", json!({"status": status}))
//             .build();
//
//         match event {
//             Ok(event) => event,
//             Err(e) => {
//                 tracing::error!("Failed to build response event: {:?}", e);
//                 Event::default()
//             }
//         }
//     }
// // }
// //
// // #[cfg(test)]
// // mod tests {
// //     use super::*;
// //
// //     #[test]
// //     fn test_request_builder() {
// //         let request = Request::builder().build();
// //         assert_eq!(request.meta.source, "psf.platform.discord");
// //         assert_eq!(request.meta.ty, "psf.discord.message.request");
// //     }
// //
// //     #[test]
// //     fn test_request_send() {
// //         let request = Request::builder().build();
// //         let sent_event = request.send().message("Hello world!".to_string());
// //         assert_eq!(sent_event.data.unwrap().message, "Hello world!".to_string());
// //     }
// //
// //     // #[test]
// //     // fn test_response_builder() {
// //     //     let response = Response::builder().build();
// //     //     assert_eq!(response.meta.source, "psf.platform.discord");
// //     //     assert_eq!(response.meta.ty, "psf.discord.message.response");
// //     // }
// // }
