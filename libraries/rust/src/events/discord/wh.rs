use crate::events::Event;
use bon::{bon, builder};
use cloudevents::{EventBuilder, EventBuilderV10};
use serde;
use serde::{Deserialize, Serialize};
use serde_json::json;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Message {
    pub url: String,
}

// let event = Event::<String> {
//             id: Some("test-id".to_string()),
//             specversion: Some("1.0".to_string()),
//             ty: "test.event".to_string(),
//             source: "test.source".to_string(),
//             time: Some("2023-01-01T00:00:00Z".to_string()),
//             datacontenttype: Some("application/json".to_string()),
//             dataschema: Some(Url::parse("https://example.com/schema").unwrap()),
//             data: "test-data".to_string(),
//         };

impl Event<Message> {
    pub fn new(message: Message) -> Box<dyn cloudevents::EventBuilder> {
        EventBuilderV10::new()
            .id("hello".to_string())
            .ty("type".to_string())
            .source("source".to_string())
            .data(message.to_string())
            .build()
            .unwrap()

        // event.set_data_content_type("application/json");
        // event.set_data_schema(Url::parse("https://example.com/schema").unwrap());
        // event
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cloudevents::{Event, EventBuilder, EventBuilderV10};
    use serde_json::json;

    #[test]
    fn test_to_event() {
        let message = Message {
            url: "https://example.com/test".to_string(),
        };

        let event = message.to_event();

        assert_eq!(event.id, Some("test-id".to_string()));
        assert_eq!(event.specversion, Some("1.0".to_string()));
        assert_eq!(event.ty, "test.event".to_string());
        assert_eq!(event.source, "test.source".to_string());
        assert_eq!(event.time, Some("2023-01-01T00:00:00Z".to_string()));
        assert_eq!(event.datacontenttype, Some("application/json".to_string()));
        // assert_eq!(
        //     event.dataschema,
        //     // Some(Url::parse("https://example.com/schema").unwrap())
        // );
        assert_eq!(
            event.data,
            json!({"url": "https://example.com/test"}).to_string()
        );
    }
}
