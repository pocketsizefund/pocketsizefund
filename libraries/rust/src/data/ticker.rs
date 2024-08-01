
use serde::{Deserialize, Serialize};
use anyhow::{anyhow, Result};


#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Symbol(String);

#[derive(Debug, Serialize, Deserialize)]
pub struct Ticker {
    active: bool,
    cik: String,
    composite_figi: String,
    currency_name: String,
    last_updated_utc: String,
    locale: String,
    market: String,
    name: String,
    primary_exchange: String,
    share_class_figi: String,
    #[serde(rename = "ticker")]
    symbol: Symbol,
    #[serde(rename = "type")]
    ticker_type: String,
}

impl Ticker {
    fn client() -> reqwest::Client {
        reqwest::Client::new()
    }

    fn base_url() -> String {
        "https://api.polygon.io".to_string()
    }

    pub async fn fetch(symbol: Symbol) -> Result<Self> {
        let url = format!("{}/v3/reference/tickers/{}", Self::base_url(), symbol.0);
        let response = Self::client().get(&url).send().await?;
        let body = response.text().await?;
        let ticker: Ticker = serde_json::from_str(&body).map_err(|e| anyhow!("JSON deserialization error: {}", e))?;
        Ok(ticker)
    }
}



#[derive(Debug, Serialize, Deserialize)]
struct TickerResponse {
    count: u32,
    next_url: String,
    request_id: String,
    results: Vec<Ticker>,
    status: String,
}



#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn test_serialize_ticker_response() {
        let expected_result = json!({
        "count": 1,
        "next_url": "https://api.polygon.io/v3/reference/tickers",
        "request_id": "e70013d92930de90e089dc8fa098888e",
        "results": [
        {
            "active": true,
            "cik": "0001090872",
            "composite_figi": "BBG000BWQYZ5",
            "currency_name": "usd",
            "last_updated_utc": "2021-04-25T00:00:00Z",
            "locale": "us",
            "market": "stocks",
            "name": "Agilent Technologies Inc.",
            "primary_exchange": "XNYS",
            "share_class_figi": "BBG001SCTQY4",
            "ticker": "A",
            "type": "CS"
        }
        ],
        "status": "OK"
    });

    let response: TickerResponse = serde_json::from_value(expected_result).expect("failed to serialize TickerResponse");
    assert_eq!(response.count, 1);
    assert_eq!(response.results[0].active, true);
    assert_eq!(response.results[0].symbol, Symbol("A".to_string()));
    }

    fn test_base_url() {
        assert_eq!(Ticker::base_url(), "https://api.polygon.io");
    }
}