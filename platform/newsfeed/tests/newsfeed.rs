// use crate::Kafka;
use testcontainers::*;
use testcontainers::{core::ContainerPort::Tcp, core::WaitFor, runners::AsyncRunner, GenericImage};
use testcontainers_modules::kafka::Kafka;

#[tokio::test]
async fn test_kafka() {
    let container = GenericImage::new("confluentinc/confluent-local", "7.5.0")
        .with_exposed_port(Tcp(9090))
        // .with_wait_for(WaitFor::message_on_stdout("Ready to accept connections"))
        .start()
        .await
        .expect("Kafka started");
}
