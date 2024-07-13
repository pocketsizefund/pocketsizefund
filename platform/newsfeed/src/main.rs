use rdkafka::consumer::{Consumer, StreamConsumer};
use rdkafka::producer::{FutureProducer, FutureRecord};
use rdkafka::util::Timeout;
use rdkafka::ClientConfig;
use rdkafka::Message;
use regex::Regex;
use scraper::{Html, Selector};
use std::sync::Arc;
use std::time::Duration;
use url::Url;

use newsfeed::{ClaudeMessage, ClaudeModel, ClaudeResponse, Prompt, Role, SentimentResponse};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::env;
use tracing::{debug, error, info, instrument, warn};
use uuid::Uuid;

#[derive(Debug)]
struct UpstashConfig {
    bootstrap_server: String,
    username: String,
    password: String,
}

#[instrument]
async fn create_consumer(upstash: Arc<UpstashConfig>) -> StreamConsumer {
    ClientConfig::new()
        .set("bootstrap.servers", &upstash.bootstrap_server)
        .set("enable.partition.eof", "false")
        .set("security.protocol", "SASL_SSL")
        .set("sasl.mechanisms", "SCRAM-SHA-256")
        .set("sasl.username", &upstash.username)
        .set("sasl.password", &upstash.password)
        .set("group.id", format!("chat-{}", Uuid::new_v4()))
        .create()
        .expect("Failed to create client")
}

#[instrument]
async fn create_producer(upstash: Arc<UpstashConfig>) -> FutureProducer {
    ClientConfig::new()
        .set("bootstrap.servers", &upstash.bootstrap_server)
        .set("security.protocol", "SASL_SSL")
        .set("sasl.mechanisms", "SCRAM-SHA-256")
        .set("sasl.username", &upstash.username)
        .set("sasl.password", &upstash.password)
        .create()
        .expect("Failed to create producer")
}

#[derive(Debug, Serialize, Deserialize)]
struct SentimentData {
    confidence: i32,
}

async fn send_sentiment(
    producer: &FutureProducer,
    topic: &str,
    sentiment_data: &SentimentResponse,
) {
    let payload = serde_json::to_string(&sentiment_data).unwrap();
    let key = Uuid::new_v4().to_string();

    let record = FutureRecord::to(topic).payload(&payload).key(&key);

    match producer
        .send(record, Timeout::After(Duration::from_secs(5)))
        .await
    {
        Ok((partition, offset)) => {
            debug!(
                "Sent sentiment data to topic '{}', partition {}, offset {}",
                topic, partition, offset
            );
        }
        Err((e, _)) => {
            error!("Failed to send sentiment data: {:?}", e);
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
enum RecordNamespace {
    UpstashAvro,
}

#[derive(Debug, Serialize, Deserialize)]
struct RecordDatetime {
    name: String,
    #[serde(rename = "type")]
    field_type: String,
    doc: String,
    #[serde(rename = "default")]
    field_default: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct NewsSource {
    name: String,
    #[serde(rename = "type")]
    field_type: String,
    doc: String,
    field_default: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Fields(
    #[serde(rename = "record_date_time")] RecordDatetime,
    #[serde(rename = "news_source")] NewsSource,
);

#[derive(Debug, Serialize, Deserialize)]
struct NewsFeedSchema {
    #[serde(rename = "type")]
    field_type: String,
    name: String,
    namespace: String,
    fields: Fields,
}

pub async fn consume_message(consumer: StreamConsumer, producer: FutureProducer) {
    info!("Starting the consumer loop...");
    loop {
        match consumer.recv().await {
            Err(e) => warn!("Error while receiving message: {:?}", e),
            Ok(msg) => {
                let payload = match msg.payload_view::<str>() {
                    None => "",
                    Some(Ok(s)) => s,
                    Some(Err(e)) => {
                        warn!("Error while deserializing message payload: {:?}", e);
                        ""
                    }
                };

                let message: NewsFeedSchema = match serde_json::from_str(payload) {
                    Ok(result) => result,
                    Err(e) => {
                        warn!("Failed to deserialize message: {:?}", e);
                        continue;
                    }
                };

                let news_source_url: String = match Url::parse(&message.fields.1.doc) {
                    Ok(result) => {
                        let news_source_url = result.as_str().to_string();
                        debug!("Received news source: {}", news_source_url);
                        news_source_url
                    }
                    Err(e) => {
                        warn!("Failed to parse news source url: {:?}", e);
                        continue;
                    }
                };

                let pattern = match Regex::new(r"\s+") {
                    Ok(result) => result,
                    Err(e) => {
                        warn!("Failed to compile regex: {:?}", e);
                        continue;
                    }
                };

                let raw: String = match reqwest::get(news_source_url).await {
                    Ok(response) => match response.text().await {
                        Ok(result) => result,
                        Err(e) => {
                            error!("Failed to fetch news source text response: {:?}", e);
                            continue;
                        }
                    },
                    Err(e) => {
                        error!("Failed to fetch news source: {:?}", e);
                        continue;
                    }
                };

                let body_selector = Selector::parse("body").unwrap();
                let document = Html::parse_document(&raw);
                let body = document
                    .select(&body_selector)
                    .next()
                    .map(|element| element.text().collect::<Vec<_>>().join(" "))
                    .map(|body| pattern.replace_all(&body, " ").into_owned())
                    .unwrap();

                let earnings_report_sentiment_tool: serde_json::Value = json!({
                    "name": "earnings_report_sentiment",
                    "description": "Sentiment of an earnings report using well-structured JSON.",
                    "input_schema": {
                        "type": "object",
                        "properties": {
                            "confidence": {
                                "type": "integer",
                                "description": "How confident is the earnings report?"
                            },
                            "future_oriented_optimism": {
                                "type": "string",
                                "description": "How optimistic is the earnings report? Must be one of 'optimistic', 'pessimistic', or 'neutral'."
                            },
                        },
                        "required": [ "confidence", "feature_oriented_optimism" ]
                    }
                });

                let chat = ClaudeMessage {
                    model: ClaudeModel::Claude35Sonnet,
                    max_tokens: 1024,
                    system: Some("You are a financial news analyst, and you are extracting the sentiment from a quarterly earnings press release.".to_string()),
                    tools: vec![earnings_report_sentiment_tool],
                    tool_choice: Some(json!({"type": "tool", "name": "earnings_report_sentiment"})),
                    messages: vec![
                        Prompt {
                            role: Role::User,
                            content: body,
                        },
                    ],
                };

                let chat_response = match chat.generate().await {
                    Ok(response) => response,
                    Err(e) => {
                        error!("Failed to generate chat response: {:?}", e);
                        continue;
                    }
                };

                debug!("chat response: {:?}", chat_response);

                let topic = msg.topic();
                let partition = msg.partition();
                let offset = msg.offset();

                if let Err(e) = consumer.store_offset(topic, partition, offset) {
                    error!("Failed to store offset: {:?}", e);
                }

                let sentiment_data: ClaudeResponse = match serde_json::from_str(&chat_response) {
                    Ok(data) => data,
                    Err(e) => {
                        error!("Failed to parse sentiment data: {:?}", e);
                        continue;
                    }
                };

                send_sentiment(
                    &producer,
                    "news.earnings-report.sentiment.extracted",
                    &sentiment_data.content[0].input,
                )
                .await;
            }
        }
    }
}

#[tokio::main]
async fn main() {
    let subscriber = tracing_subscriber::fmt()
        .with_file(true)
        .with_line_number(true)
        .with_thread_ids(true)
        .finish();

    let _ = tracing::subscriber::set_global_default(subscriber)
        .map_err(|_err| eprintln!("Unable to set global default subscriber"));

    let upstash = Arc::new(UpstashConfig {
        bootstrap_server: env::var("UPSTASH_ENDPOINT")
            .expect("UPSTASH_BOOTSTRAP_SERVER is not set"),
        username: env::var("UPSTASH_USERNAME").expect("UPSTASH_USERNAME is not set"),
        password: env::var("UPSTASH_PASSWORD").expect("UPSTASH_PASSWORD is not set"),
    });

    info!("bootstrapping kafka server at {}", upstash.bootstrap_server);

    info!("Creating the consumer");
    let consumer = create_consumer(upstash.clone()).await;
    info!("Creating the producer");
    let producer = create_producer(upstash.clone()).await;

    info!("consumer created");

    let topics = vec!["news.earnings-report.submitted"];

    consumer
        .subscribe(&topics)
        .expect("Failed to subscribe to topics");

    info!("subscribed to topics: {}", topics.join(", "));

    consume_message(consumer, producer).await;
}
