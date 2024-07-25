use rdkafka::config::ClientConfig;
use rdkafka::producer::{FutureProducer, FutureRecord};
use rdkafka::util::Timeout;

pub struct Publisher {
    producer: FutureProducer,
}

impl Publisher {
    pub fn new(bootstrap_server_url: &str) -> Self {
        let producer = ClientConfig::new()
            .set("bootstrap.servers", bootstrap_server_url)
            .set("queue.buffering.max.ms", "0")
            .create()
            .expect("failed creating producer");

        Self {
            producer,
        }
    }

    pub async fn send_event(&self, topic: String, payload: String) {
        self.producer.send(
            FutureRecord::<(), _>::to(&topic)
                .payload(&payload), Timeout::Never)
                .await
                .expect("failed sending message");
    }
}