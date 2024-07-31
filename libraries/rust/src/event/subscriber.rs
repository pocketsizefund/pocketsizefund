use rdkafka::config::ClientConfig;
use rdkafka::consumer::{BaseConsumer, Consumer};
use rdkafka::Message;
use std::error::Error;
use std::fmt;
use std::time::Duration;
use uuid::Uuid;

pub struct Subscriber {
    consumer: BaseConsumer,
}

impl Subscriber {
    pub fn new(bootstrap_server_url: &str, topic_names: &[String]) -> Result<Self, NoTopicsError> {
        if topic_names.is_empty() {
            return Err(NoTopicsError);
        }

        let consumer: BaseConsumer = ClientConfig::new()
            .set("bootstrap.servers", bootstrap_server_url)
            .set("enable.partition.eof", "false")
            .set("group.id", Uuid::new_v4().to_string())
            .create()
            .expect("failed creating subscriber");

        for topic_name in topic_names {
            consumer
                .subscribe(&[topic_name])
                .expect("error subscribing to topic");
        }

        Ok(Subscriber { consumer })
    }

    pub async fn receive_events(&self) -> Result<String, Box<dyn std::error::Error>> {
        let message = self.consumer.poll(Duration::from_secs(1));

        match message {
            None => Ok(String::from("")),
            Some(Ok(message)) => {
                if let Some(payload) = message.payload() {
                    Ok(String::from_utf8(payload.to_vec())?)
                } else {
                    Err(Box::new(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "invalid payload",
                    )))
                }
            }
            Some(Err(e)) => Err(Box::new(e)),
        }
    }
}

#[derive(Debug)]
pub struct NoTopicsError;

impl fmt::Display for NoTopicsError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "no topic names provided")
    }
}

impl Error for NoTopicsError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_subscriber_no_topics() {
        let subscriber = Subscriber::new("localhost:9092", &[]);
        assert!(subscriber.is_err());
    }
}
