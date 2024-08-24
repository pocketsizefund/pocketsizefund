/// Event SDK
use bon::builder;
use chrono::{DateTime, Utc};
use cloudevents::{EventBuilder, EventBuilderV10};
use serde::{Deserialize, Serialize};
use url::Url;

/// Metadata for an event.
/// This is to ensure capatability with the CloudEvents spec.
/// See: `https://github.com/cloudevents/spec?tab=readme-ov-file`
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Event<T> {
    /// Identifies the event. Producers MUST ensure that
    /// `source` + `id` is unique for each distinct event.
    /// Consumers MAY assume that Events with identical `source`
    /// and `id` are duplicates.
    // #[builder(default = uuid::Uuid::new_v4().to_string())]
    pub id: Option<String>,
    /// The version of the CloudEvents specification
    /// which the event uses. This enables the interpretation
    /// of the context. Compliant event producers MUST
    /// use a value of 1.0 when referring to this version
    /// of the specification.
    /// Currently, this attribute will only have the
    /// 'major' and 'minor' version numbers included in it.
    /// This allows for 'patch' changes to the specification
    /// to be made without changing this property's value in
    /// the serialization. Note: for 'release candidate' releases
    /// a suffix might be used for testing purposes.
    // #[builder(default = "1.0")]
    pub specversion: Option<String>,
    /// This attribute contains a value describing
    /// the type of event related to the originating occurrence.
    /// Often this attribute is used for routing, observability,
    /// policy enforcement, etc. The format of this is producer
    /// defined and might include information such as the
    /// version of the type - see Versioning of CloudEvents
    /// in the Primer for more information.
    /// SHOULD be prefixed with a reverse-DNS name.
    /// The prefixed domain dictates the organization
    /// which defines the semantics of this event type.
    pub ty: String,
    /// Identifies the context in which an event happened.
    /// Often this will include information such as
    /// the type of the event source, the organization publishing
    /// the event or the process that produced the event.
    /// The exact syntax and semantics behind the data encoded
    /// in the URI is defined by the event producer.
    /// Producers MUST ensure that source + id is unique for each distinct event.
    /// An application MAY assign a unique source to each distinct producer,
    /// which makes it easy to produce unique IDs since no other producer
    /// will have the same source. The application MAY use
    /// UUIDs, URNs, DNS authorities or an application-specific scheme
    /// to create unique source identifiers.
    /// A source MAY include more than one producer.
    /// In that case the producers MUST collaborate to ensure
    /// that source + id is unique for each distinct event.
    pub source: String,
    /// Timestamp of when the occurrence happened.
    /// If the time of the occurrence cannot be determined
    /// then this attribute MAY be set to some other time
    /// (such as the current time) by the CloudEvents producer,
    /// however all producers for the same source MUST be consistent
    /// in this respect. In other words, either they all use
    /// the actual time of the occurrence or they all use
    /// the same algorithm to determine the value used.
    /// If present, MUST adhere to the format specified in RFC 3339
    // #[builder(default = chrono::Utc::now().to_rfc3339())]
    pub time: Option<String>,
    pub datacontenttype: Option<String>,
    pub dataschema: Option<Url>,
    pub data: T,
}

impl<T: Serialize> Into<cloudevents::Event> for Event<T> {
    fn into(self) -> cloudevents::Event {
        let time = match self.time {
            Some(time) => time,
            None => Utc::now().to_rfc3339(),
        };

        EventBuilderV10::new()
            .id(self.id.unwrap_or_else(|| uuid::Uuid::new_v4().to_string()))
            .source(self.source)
            .ty(self.ty)
            .data("application/json", serde_json::to_value(self.data).unwrap())
            .time(time)
            .build()
            .unwrap()

        // match datacontenttype {
        //     Some(datacontenttype) => event_builder.datacontenttype(datacontenttype),
        //     None => event_builder.datacontenttype("application/json"),
        // }
        // ty: self.ty,
        // source: self.source,
        // specversion: self.specversion,
        // datacontenttype: self.datacontenttype,
        // dataschema: self.dataschema,
        // data: self.data,
        // time: self.time,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_serialize_simple_string_event() {
        let event = Event::<String> {
            id: Some("test-id".to_string()),
            specversion: Some("1.0".to_string()),
            ty: "test.event".to_string(),
            source: "test.source".to_string(),
            time: Some("2023-01-01T00:00:00Z".to_string()),
            datacontenttype: Some("application/json".to_string()),
            dataschema: Some(Url::parse("https://example.com/schema").unwrap()),
            data: "test-data".to_string(),
        };

        // let event_json = serde_json::to_string(&event).unwrap();
        let expected = json!({
            "id": "test-id",
            "specversion": "1.0",
            "ty": "test.event",
            "source": "test.source",
            "time": "2023-01-01T00:00:00Z",
            "datacontenttype": "application/json",
            "dataschema": "https://example.com/schema",
            "data": "test-data"
        });

        assert_eq!(event.id, Some(expected["id"]));
    }

    // #[test]
    // fn test_default_event_time_is_valid_rfc3339() {
    //     let event_meta = Meta::builder()
    //         .ty("test.event")
    //         .source("test.source")
    //         .build();
    //
    //     assert!(chrono::DateTime::parse_from_rfc3339(event_meta.time.as_str()).is_ok());
    // }
}
