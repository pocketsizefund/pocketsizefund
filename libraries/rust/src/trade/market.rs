use apca::api::v2::clock::Get;
use apca::{ApiInfo, Client};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use tracing::error;

#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ClosedReason {
    AfterHours,
    BeforeHours,
    Weekend,
    Holiday,
    Unknown,
}

impl ToString for ClosedReason {
    fn to_string(&self) -> String {
        match self {
            ClosedReason::AfterHours => "after_hours".to_string(),
            ClosedReason::BeforeHours => "before_hours".to_string(),
            ClosedReason::Weekend => "weekend".to_string(),
            ClosedReason::Holiday => "holiday".to_string(),
            ClosedReason::Unknown => "unknown".to_string(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum Status {
    Open,
    ExtendedHours,
    Closed(ClosedReason),
    Unknown,
}

impl ToString for Status {
    fn to_string(&self) -> String {
        match self {
            Status::Open => "open".to_string(),
            Status::ExtendedHours => "extended_hours".to_string(),
            Status::Closed(reason) => format!("closed_{}", reason.to_string()),
            Status::Unknown => "unknown".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Market {
    client: Client,
    pub updated_at: Option<DateTime<Utc>>,
    pub status: Status,
    pub current: Option<DateTime<Utc>>,
    pub next_open: Option<DateTime<Utc>>,
    pub next_close: Option<DateTime<Utc>>,
}

impl Default for Market {
    fn default() -> Self {
        let api_info = match ApiInfo::from_env() {
            Ok(api_info) => api_info,
            Err(e) => {
                panic!("Failed to get API info: {:?}", e);
            }
        };
        let client = Client::new(api_info);

        Self {
            client,
            updated_at: None,
            status: Status::Unknown,
            current: None,
            next_open: None,
            next_close: None,
        }
    }
}

impl Market {
    pub async fn check_current_status(&mut self) {
        let clock_result = self.client.issue::<Get>(&()).await;

        match clock_result {
            Ok(clock) => {
                self.updated_at = Some(clock.current);
                self.next_open = Some(clock.next_open);
                self.next_close = Some(clock.next_close);
                match clock.open {
                    true => self.status = Status::Open,
                    false => self.status = Status::Closed(ClosedReason::Unknown),
                }
            }
            Err(err) => {
                error!("Failed to check market status: {}", err);
                self.status = Status::Unknown;
            }
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_closed_reason_to_string() {
        assert_eq!(ClosedReason::AfterHours.to_string(), "after_hours");
        assert_eq!(ClosedReason::BeforeHours.to_string(), "before_hours");
        assert_eq!(ClosedReason::Weekend.to_string(), "weekend");
        assert_eq!(ClosedReason::Holiday.to_string(), "holiday");
        assert_eq!(ClosedReason::Unknown.to_string(), "unknown");
    }

    #[test]
    fn test_status_to_string() {
        assert_eq!(Status::Open.to_string(), "open");
        assert_eq!(Status::ExtendedHours.to_string(), "extended_hours");
        assert_eq!(
            Status::Closed(ClosedReason::AfterHours).to_string(),
            "closed_after_hours"
        );
        assert_eq!(Status::Unknown.to_string(), "unknown");
    }

    #[test]
    fn test_market_default() {
        let market = Market {
            client: Client::new(
                ApiInfo::from_parts(
                    "https://paper-api.alpaca.markets".to_string(),
                    "test_key_id".to_string(),
                    "test_secret".to_string(),
                )
                .unwrap(),
            ),
            updated_at: None,
            status: Status::Open,
            current: None,
            next_open: None,
            next_close: None,
        };

        assert!(market.updated_at.is_none());
        assert_eq!(market.status, Status::Open);
    }

    #[test]
    fn test_closed_reason_serialization() {
        let reason = ClosedReason::AfterHours;
        let serialized = serde_json::to_string(&reason).unwrap();
        assert_eq!(serialized, "\"after_hours\"");
    }

    #[test]
    fn test_closed_reason_deserialization() {
        let serialized = "\"after_hours\"";
        let deserialized: ClosedReason = serde_json::from_str(serialized).unwrap();
        assert_eq!(deserialized, ClosedReason::AfterHours);
    }

    #[test]
    fn test_status_serialization() {
        let status = Status::Closed(ClosedReason::Weekend);
        let serialized = serde_json::to_string(&status).unwrap();
        assert_eq!(serialized, "{\"closed\":\"weekend\"}");
    }

    #[test]
    fn test_status_deserialization() {
        let serialized = "{\"closed\":\"weekend\"}";
        let deserialized: Status = serde_json::from_str(serialized).unwrap();
        assert_eq!(deserialized, Status::Closed(ClosedReason::Weekend));
    }

    #[test]
    fn test_status_partial_eq() {
        assert_eq!(Status::Open, Status::Open);
        assert_ne!(Status::Open, Status::Closed(ClosedReason::AfterHours));
        assert_eq!(
            Status::Closed(ClosedReason::Weekend),
            Status::Closed(ClosedReason::Weekend)
        );
        assert_ne!(
            Status::Closed(ClosedReason::Weekend),
            Status::Closed(ClosedReason::Holiday)
        );
    }

    #[test]
    fn test_closed_reason_partial_eq() {
        assert_eq!(ClosedReason::AfterHours, ClosedReason::AfterHours);
        assert_ne!(ClosedReason::AfterHours, ClosedReason::BeforeHours);
        assert_eq!(ClosedReason::Weekend, ClosedReason::Weekend);
        assert_ne!(ClosedReason::Weekend, ClosedReason::Holiday);
    }

    #[test]
    fn test_market_debug() {
        let market = Market {
            client: Client::new(
                ApiInfo::from_parts(
                    "https://paper-api.alpaca.markets".to_string(),
                    "test_key_id".to_string(),
                    "test_secret".to_string(),
                )
                .unwrap(),
            ),
            updated_at: None,
            status: Status::Open,
            current: None,
            next_open: None,
            next_close: None,
        };

        let debug_output = format!("{:?}", market);
        assert!(debug_output.contains("Market"));
        assert!(debug_output.contains("client"));
        assert!(debug_output.contains("updated_at"));
        assert!(debug_output.contains("status"));
    }

    #[test]
    fn test_status_debug() {
        let status = Status::Closed(ClosedReason::Holiday);
        let debug_output = format!("{:?}", status);
        assert!(debug_output.contains("Closed"));
        assert!(debug_output.contains("Holiday"));
    }

    #[test]
    fn test_closed_reason_debug() {
        let reason = ClosedReason::BeforeHours;
        let debug_output = format!("{:?}", reason);
        assert!(debug_output.contains("BeforeHours"));
    }
}
