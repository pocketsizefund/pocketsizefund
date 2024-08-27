use crate::schema::prelude::{Price, Volume};
use crate::schema::symbol::Ticker;
use chrono::prelude::NaiveDate;
use color_eyre::Result;
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
