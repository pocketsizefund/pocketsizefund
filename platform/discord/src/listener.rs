use rdkafka::consumer::{Consumer, StreamConsumer};
use rdkafka::Message;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct KafkaMessage {}

#[tracing::instrument(skip_all)]
pub async fn consume_message(con: StreamConsumer) {
    tracing::info!("Starting the consumer loop...");

    loop {
        match con.recv().await {
            Err(e) => tracing::warn!("Error while receiving message: {:?}", e),
            Ok(msg) => {
                let payload = msg.payload().unwrap_or_default();

                let message: KafkaMessage = match serde_json::from_slice(payload) {
                    Ok(result) => result,
                    Err(e) => {
                        tracing::error!("Failed to deserialize message: {:?}", e);
                        continue;
                    }
                };

                tracing::info!("Received message: {:?}", message);

                let topic = msg.topic();
                let partition = msg.partition();
                let offset = msg.offset();

                if let Err(e) = con.store_offset(topic, partition, offset) {
                    tracing::error!("Failed to store offset: {:?}", e);
                }
            }
        }
    }
}
