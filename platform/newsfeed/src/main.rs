use apache_avro::from_avro_datum;
use apache_avro::Schema;
use rdkafka::consumer::{Consumer, StreamConsumer};
use rdkafka::ClientConfig;
use rdkafka::Message;
use regex::Regex;
use reqwest;
use scraper::{Html, Selector};

use serde::{Deserialize, Serialize};
use std::env;
use tracing_subscriber;
use tracing_subscriber::filter::EnvFilter;
use uuid::Uuid;
// use serde_avro::from_value;
use tracing::{debug, error, info, instrument, warn};
use url::Url;
// use tracing_subscriber::{self, EnvFilter};
//
use newsfeed::{ClaudeMessage, ClaudeModel, Prompt, Role};

#[derive(Debug)]
struct UpstashConfig {
    bootstrap_server: String,
    username: String,
    password: String,
}

#[instrument]
async fn create_consumer(upstash: UpstashConfig) -> StreamConsumer {
    ClientConfig::new()
        .set("bootstrap.servers", upstash.bootstrap_server)
        .set("enable.partition.eof", "false")
        .set("security.protocol", "SASL_SSL")
        .set("sasl.mechanisms", "SCRAM-SHA-256")
        .set("sasl.username", &upstash.username)
        .set("sasl.password", &upstash.password)
        .set("group.id", format!("chat-{}", Uuid::new_v4()))
        .create()
        .expect("Failed to create client")
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

// #[instrument]
pub async fn consume_message(consumer: StreamConsumer) {
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

                let message: NewsFeedSchema = match serde_json::from_str(&payload) {
                    Ok(result) => result,
                    Err(e) => {
                        warn!("Failed to deserialize message: {:?}", e);
                        continue;
                    }
                };

                let news_source_url: String = match Url::parse(&message.fields.1.doc) {
                    Ok(result) => {
                        let news_source_url = result.as_str().to_string();
                        info!("Received news source: {}", news_source_url);
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

                let chat = ClaudeMessage {
                    model: ClaudeModel::Claude35Sonnet,
                    max_tokens: 1024,
                    system: Some("You are a financial news analyst, and you are extracting the content from an html source with noise".to_string()),
                    messages: vec![
                        Prompt {
                            role: Role::User,
                            content: body,
                        },
                    ],
                };

                let chat_response = chat.generate().await;

                info!("chat response: {:?}", chat_response);

                let topic = msg.topic();
                let partition = msg.partition();
                let offset = msg.offset();

                if let Err(e) = consumer.store_offset(topic, partition, offset) {
                    error!("Failed to store offset: {:?}", e);
                }
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

    let upstash = UpstashConfig {
        bootstrap_server: env::var("UPSTASH_ENDPOINT")
            .expect("UPSTASH_BOOTSTRAP_SERVER is not set"),
        username: env::var("UPSTASH_USERNAME").expect("UPSTASH_USERNAME is not set"),
        password: env::var("UPSTASH_PASSWORD").expect("UPSTASH_PASSWORD is not set"),
    };

    info!("bootstrapping kafka server at {}", upstash.bootstrap_server);

    let consumer = create_consumer(upstash).await;

    info!("consumer created");

    // TODO paper needs to be configurable
    let topics = vec!["paper.pocketsizefund.news.earnings-report.submitted"];

    consumer
        .subscribe(&topics)
        .expect("Failed to subscribe to topics");

    info!("subscribed to topics: {}", topics.join(", "));

    consume_message(consumer).await;
}
