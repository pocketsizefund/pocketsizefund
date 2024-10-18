use cloudevents::{Event, EventBuilder, EventBuilderV10};
use chrono::Utc;

pub fn build_response_event(
    service_name: String,
    metadata: Vec<String>,
    json_payload: Option<String>,
) -> Event {
    let tag = "psf.".to_string() + &service_name + "." + &metadata.join(".");

    let source = "platform:".to_string() + &service_name;

    let mut event_builder = EventBuilderV10::new()
        .id(uuid::Uuid::new_v4().to_string())
        .ty(tag)
        .source(source)
        .extension("timestamp", Utc::now().to_rfc3339().to_string());

    if let Some(payload) = json_payload {
        event_builder = event_builder.data(
            "application/cloudevents+json",
            payload,
        );
    };

    event_builder
        .build()
        .unwrap_or_else(|e| {
            tracing::error!("Failed to build event: {}", e);
            Event::default()
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use cloudevents::AttributesReader;
    use cloudevents::event::Data;

    #[test]
    fn test_build_response_event() {
        let service_name = "test".to_string();
        let metadata = vec!["test".to_string(), "test".to_string()];
        let json_payload = Some("test".to_string());

        let event = build_response_event(service_name, metadata, json_payload);

        assert_eq!(event.ty(), "psf.test.test.test");
        assert_eq!(event.source(), "platform:test");
        assert_eq!(event.datacontenttype().unwrap(), "application/cloudevents+json");
        if let Some(Data::String(data)) = event.data() {
            assert_eq!(data, "test");
        } else {
            panic!("Unexpected data type or value");
        }
    }
}