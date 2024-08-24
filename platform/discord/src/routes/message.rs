use crate::config::DiscordWebhook;
use actix_web::{post, web};
use cloudevents::Event;
use pocketsizefund::events::discord;
use serde_json::json;

#[post("/message")]
pub async fn event_handler(event: Event) -> Event {
    tracing::info!("Received message: {:?}", event);
    // let sent_event = request.send().message("Hello world!".to_string());
    let discord_request = discord::webhook::Request::builder().build();

    let message = match event.data() {
        Some(data) => {
            tracing::info!("yo data: {:#?}", data.to_string());
            let message: discord::webhook::DiscordMessage =
                match serde_json::from_str(&data.to_string()) {
                    Ok(result) => result,
                    Err(e) => {
                        tracing::error!("Failed to parse {:#?}", e);
                        // TODO: failed/unknown webhook
                        return event;
                    }
                };
            discord::webhook::Request::builder()
                .build()
                .receive()
                .data(message)
                .call();
        }
        None => {
            tracing::error!("No content provided");
            return event;
        }
    };

    tracing::error!("content: {:#?}", message);

    // let payload = WebhookPayload::new(&content);
    //
    // let response = reqwest::Client::new()
    //     .post(webhook.url.clone())
    //     .json(&payload)
    //     .send()
    //     .await;
    //
    // let output = match response {
    //     Ok(response) => match response.status().as_u16() {
    //         204 => {
    //             tracing::info!("Message successfully sent");
    //             build_response_event(WebhookResponse {
    //                 message: "Message successfully sent".to_string(),
    //                 status: WebhookStatus::Success,
    //             })
    //         }
    //         _ => {
    //             tracing::error!("Message failed to send: {:#?}", response);
    //             build_response_event(WebhookResponse {
    //                 message: "Message failed to send".to_string(),
    //                 status: WebhookStatus::Failed,
    //             })
    //         }
    //     },
    //     Err(response) => {
    //         tracing::error!("Message failed to send: {:#?}", response);
    //         build_response_event(WebhookResponse {
    //             message: "Message failed to send".to_string(),
    //             status: WebhookStatus::Failed,
    //         })
    //     }
    // };

    event
}
