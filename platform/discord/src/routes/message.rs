use crate::config::DiscordWebhook;
use actix_web::{post, web};
use cloudevents::Event;
use pocketsizefund::events::build_response_event;
use serde::{Deserialize, Serialize};
use serde_json::json;

#[derive(Debug, Serialize, Deserialize)]
struct WebhookPayload {
    content: String,
}

impl WebhookPayload {
    fn new(content: &str) -> Self {
        Self {
            content: content.to_string(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
enum WebhookStatus {
    Success,
    Failed,
}

#[derive(Debug, Serialize, Deserialize)]
struct WebhookResponse {
    message: String,
    status: WebhookStatus,
}

#[post("/")]
async fn event_handler(event: Event, webhook: web::Data<DiscordWebhook>) -> Event {
    tracing::info!("Received message: {:?}", event);
    let content = match event.data() {
        Some(data) => data.to_string(),
        None => {
            tracing::error!("No content provided");
            return event;
        }
    };

    let payload = WebhookPayload::new(&content);

    let response = reqwest::Client::new()
        .post(webhook.url.clone())
        .json(&payload)
        .send()
        .await;

    match response {
        Ok(response) => match response.status().as_u16() {
            204 => {
                tracing::info!("Message successfully sent");
                build_response_event(
                    "discord".to_string(),
                    vec!["message".to_string(), "sent".to_string()],
                    Some(
                        json!({
                            "message": "Message send success".to_string(),
                            "status": WebhookStatus::Success,
                        })
                        .to_string(),
                    ),
                )
            }
            _ => {
                tracing::error!("Message failed to send: {:#?}", response);
                build_response_event(
                    "discord".to_string(),
                    vec!["message".to_string(), "failed".to_string()],
                    Some(
                        json!({
                            "message": "Message send failure".to_string(),
                            "status": WebhookStatus::Failed,
                        })
                        .to_string(),
                    ),
                )
            }
        },
        Err(response) => {
            tracing::error!("Message failed to send: {:#?}", response);
            build_response_event(
                "discord".to_string(),
                vec!["message".to_string(), "failed".to_string()],
                Some(
                    json!({
                        "message": "Message send failure".to_string(),
                        "status": WebhookStatus::Failed,
                    })
                    .to_string(),
                ),
            )
        }
    }
}
