use serde;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
enum MarketStatus {
    Open,
    ExtendedHours,
    Closed,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub struct Market {
    after_hours: bool,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{from_value, json, Value};

    #[test]
    fn test_when_afterhours() {
        let input: Value = json!({
        "afterHours": true,
        "currencies": {
            "crypto": "open",
            "fx": "open"
        },
        "earlyHours": false,
        "exchanges": {
            "nasdaq": "extended-hours",
            "nyse": "extended-hours",
            "otc": "closed"
        },
        "market": "extended-hours",
        "serverTime": "2020-11-10T17:37:37-05:00"
        });

        let _input: Market = from_value(input).unwrap();
    }
}
