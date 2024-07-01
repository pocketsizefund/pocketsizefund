use apache_avro::from_avro_datum;
use apache_avro::Schema;
use rdkafka::consumer::{Consumer, StreamConsumer};
use rdkafka::Message;
use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize)]
struct NewsFeedMessage {
    date_time: String,
    #[serde(default = "default_source_url")]
    source_url: String,
}

fn default_source_url() -> String {
    "notAvailable".to_string()
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
enum FieldType {
    Record,
    Error,
    Enum,
    Array,
    Map,
    Union,
    Fixed,
}

#[derive(Debug, Deserialize, Serialize)]
enum RecordName {
    SimpleMessage,
}

#[derive(Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
enum RecordNamespace {
    #[serde(rename = "com.upstash.avro")]
    UpstashAvro,
    #[serde(rename = "org.apache.avro")]
    ApacheAvro,
    // Add more namespaces as needed
}

#[derive(Debug, Deserialize, Serialize)]
struct NewsFeedAvroRecord {
    name: RecordName,
    #[serde(rename = "type")]
    field_type: FieldType,
    namespace: RecordNamespace,
    #[serde(skip_serializing_if = "Option::is_none")]
    default: Option<String>,
    fields: Vec<NewsFeedMessage>,
}

#[tracing::instrument(skip_all)]
pub async fn consume_message(con: StreamConsumer) {
    tracing::info!("Starting the consumer loop...");

    loop {
        match con.recv().await {
            Err(e) => tracing::warn!("Error while receiving message: {:?}", e),
            Ok(msg) => {
                let payload = msg.payload().unwrap_or_default();

                let schema = Schema::from(&NewsAvroRecord::default());

                let message: NewsFeedMessage = match from_avro_datum(&schema, payload) {
                    Ok(result) => result,
                    Err(e) => {
                        tracing::error!(
                            "Failed to deserialize message: {:?}\nmsg={:?}",
                            e,
                            payload
                        );
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
