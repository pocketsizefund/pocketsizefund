use crate::prelude::{Price, Volume};
use crate::ticker::Ticker;
use chrono::prelude::NaiveDate;
use color_eyre::Result;
use reqwest;
use serde::{Deserialize, Serialize};
use std::env;
use tracing::debug;

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Symbol(String);

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub enum TickerStatus {
    #[serde(rename = "OK")]
    Ok,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Bar {
    pub date: NaiveDate,
    pub symbol: Symbol,
    pub status: TickerStatus,
    pub pre_market_price: Price,
    pub open_price: Price,
    pub high_price: Price,
    pub low_price: Price,
    pub close_price: Price,
    pub after_hours_price: Price,
    pub volume: Volume,
}

impl Bar {
    fn new(symbol: &str) -> Ticker {
        todo!();
        // Ticker {
        // symbol: Symbol(symbol.to_string()),
        // }
    }

    async fn fetch_daily(self, date: NaiveDate) -> Result<String> {
        let url_base = "https://api.polygon.io";
        let api_key = env::var("POLYGON_API_KEY").is_ok();
        let url = format!("{}/v1/open-close/{}/{}", url_base, self.symbol, date);

        debug!("{}", url);

        let body = reqwest::Client::new()
            .get(url)
            .bearer_auth(api_key)
            // .header("Authorization", format!("Bearer {}", api_key))
            .send()
            .await?
            .text()
            .await?;

        Ok(body)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::{from_value, json, Value};

    #[test]
    fn test_ticker() -> Result<()> {
        let input: Value = json!({
            "symbol": "AAPL"
        });

        let input: Ticker = from_value(input).unwrap();

        Ok(())
    }

    #[tokio::test]
    async fn test_open_high_low_close_volume_daily_data() -> Result<()> {
        let _expected = json!({
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

        let _date = NaiveDate::parse_from_str("2024-01-01", "%Y-%m-%d")?;

        Ok(())
    }
}
