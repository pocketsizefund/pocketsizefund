use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Ticker {
    symbol: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{from_value, json, Value};

    #[test]
    fn test_ticker() {
        let input: Value = json!({
            "symbol": "AAPL"
        });

        let input: Ticker = from_value(input).unwrap();

        assert_eq!(
            input,
            Ticker {
                symbol: "AAPL".to_string()
            }
        );
    }
}
