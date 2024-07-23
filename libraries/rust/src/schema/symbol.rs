use crate::schema::ticker::Symbol;

use serde::{Deserialize, Serialize};
#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Ticker {
    // Symbol of company
    symbol: Symbol,
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{from_value, json};

    #[test]
    fn test_open_high_low_close_volume_daily_data() {
        let expected = json!({
            "afterHours": 322.1,
            "close": 325.12,
            "from": "2023-01-09",
            "high": 326.2,
            "low": 322.3,
            "open": 324.66,
            "preMarket": 324.5,
            "status": "OK",
            "symbol": "AAPL",
            "volume": 26122646
        });
    }
}
