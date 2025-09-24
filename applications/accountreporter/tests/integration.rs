use std::time::Duration;

use testcontainers_modules::kafka::Kafka;
use testcontainers_modules::testcontainers::runners::AsyncRunner;
use futures::StreamExt;

use anyhow::{Result};
use rdkafka::{
    Message,
    consumer::{Consumer, StreamConsumer},
    producer::{FutureProducer, FutureRecord},
    ClientConfig,
};

#[tokio::test]
async fn test_basic() -> Result<()> {
    // Start Kafka (it includes Zookeeper internally)
    let kafka = Kafka::default().start().await?;

    let bootstrap_servers = format!(
        "127.0.0.1:{}",
        kafka.get_host_port_ipv4(9093).await?
    );

    println!("Bootstrap servers: {}", bootstrap_servers);

    tokio::time::sleep(Duration::from_secs(5)).await;

    let topic = "test-topic";
    let expected_message = "Hello from Kafka!";

    let producer: FutureProducer = ClientConfig::new()
        .set("bootstrap.servers", &bootstrap_servers)
        .set("message.timeout.ms", "5000")
        .create()
        .expect("Failed to create Kafka FutureProducer");

    let record = FutureRecord::to(topic)
        .payload(expected_message.as_bytes())
        .key("test-key");

    let delivery_result = producer.send(record, Duration::from_secs(5))
        .await
        .expect("Failed to send message");

    println!("Message sent successfully: partition={}, offset={}",
             delivery_result.0, delivery_result.1);

    // Create consumer and read the message
    let consumer: StreamConsumer = ClientConfig::new()
        .set("group.id", "testcontainer-rs")
        .set("bootstrap.servers", &bootstrap_servers)
        .set("session.timeout.ms", "6000")
        .set("enable.auto.commit", "false")
        .set("auto.offset.reset", "earliest")
        .create()
        .expect("Failed to create Kafka StreamConsumer");

    consumer
        .subscribe(&[topic])
        .expect("Failed to subscribe to a topic");

    let mut message_stream = consumer.stream();

    // Wait for and consume the message
    let borrowed_message = tokio::time::timeout(
        Duration::from_secs(10),
        message_stream.next()
    )
    .await
    .expect("Timeout waiting for message")
    .expect("No message received");

    // Verify the message content
    let payload = borrowed_message
        .expect("Failed to borrow message")
        .payload()
        .expect("No payload in message");

    let received_message = std::str::from_utf8(payload)
        .expect("Failed to convert payload to string");

    assert_eq!(expected_message, received_message);

    Ok(())
}
