use rdkafka::consumer::{Consumer, StreamConsumer};
use rdkafka::ClientConfig;
use std::env;
use tracing_subscriber;
use tracing_subscriber::filter::EnvFilter;
use uuid::Uuid;

mod listener;


struct UpstashConfig {
    bootstrap_server: String,
    username: String,
    password: String,
}

fn create_consumer(upstash: UpstashConfig) -> StreamConsumer {
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

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    tracing::info!("Starting up the application");

    let upstash = UpstashConfig {
        bootstrap_server: env::var("UPSTASH_ENDPOINT")
            .expect("UPSTASH_BOOTSTRAP_SERVER is not set"),
        username: env::var("UPSTASH_USERNAME").expect("UPSTASH_USERNAME is not set"),
        password: env::var("UPSTASH_PASSWORD").expect("UPSTASH_PASSWORD is not set"),
    };

    let consumer = create_consumer(upstash);

    let topic = "paper.pocketsizefund.discord.test";

    consumer
        .subscribe(&[topic])
        .expect("Failed to subscribe to topics");

    listener::consume_message(consumer).await;
}
