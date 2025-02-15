use async_trait::async_trait;
use chrono::{DateTime, Duration, Utc};
use reqwest::Client as HTTPClient;
use reqwest::Url;
use reqwest::{Error as ReqwestError, RequestBuilder, Response};
use serde::Deserialize;
use std::collections::HashMap;
use thiserror::Error as ThisError;
use url::ParseError;

#[derive(Deserialize, Debug)]
struct ConstituentItem {
    #[serde(rename = "Code")]
    code: String,
}

#[derive(Deserialize, Debug)]
struct DarqubeIndexConstituentsResponse {
    #[serde(flatten)]
    items: HashMap<String, ConstituentItem>,
}

#[derive(Deserialize, Debug)]
struct AlpacaAsset {
    tradable: bool,
    fractionable: bool,
    shortable: bool,
    symbol: String,
}

#[derive(Deserialize, Debug)]
struct AlpacaPortfolio {
    #[serde(rename = "timestamp")]
    timestamps: Vec<i64>,
    #[serde(rename = "equity")]
    equity_values: Vec<f64>,
}

#[derive(Deserialize, Debug)]
pub struct PortfolioPerformance {
    pub timestamps: Vec<DateTime<Utc>>,
    pub equity_values: Vec<f64>,
}

#[derive(Deserialize, Debug, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum Side {
    Long,
    Short,
}

#[derive(Deserialize, Debug)]
pub struct PortfolioPosition {
    #[serde(rename = "symbol")]
    pub ticker: String,
    pub side: Side,
    #[serde(rename = "qty")]
    pub quantity: f64,
}

pub struct Order {
    pub ticker: String,
    pub side: Side,
    pub quantity: f64,
}

#[derive(Deserialize, Debug)]
struct AlpacaOrder {
    #[serde(rename = "symbol")]
    ticker: String,
    position_intent: String,
}

pub struct PatternDayTraderCheck {
    pub ticker: String,
    pub is_pdt_violated: bool,
}

#[derive(ThisError, Debug)]
pub enum Error {
    #[error("Parse URL error: {0}")]
    ParseURLError(#[from] ParseError),
    #[error("Request error: {0}")]
    ReqwestError(#[from] ReqwestError),
    #[error("Other error: {0}")]
    OtherError(String),
}

#[async_trait]
pub trait Interface: Send + Sync {
    async fn get_available_tickers(&self) -> Result<Vec<String>, Error>;
    async fn get_portfolio_performance(
        &self,
        end_at: DateTime<Utc>,
    ) -> Result<PortfolioPerformance, Error>;
    async fn get_portfolio_positions(&self) -> Result<Vec<PortfolioPosition>, Error>;
    async fn check_orders_pattern_day_trade_restrictions(
        &self,
        orders: Vec<Order>,
    ) -> Result<Vec<PatternDayTraderCheck>, Error>;
    async fn execute_orders(&self, orders: Vec<Order>) -> Result<(), Error>;
}

#[derive(Clone)]
pub struct Client {
    alpaca_base_url: String,
    alpaca_api_key_id: String,
    alpaca_api_secret_key: String,
    darqube_base_url: String,
    darqube_api_key: String,
    http_client: HTTPClient,
}

impl Client {
    pub fn new(
        alpaca_api_key_id: String,
        alpaca_api_secret_key: String,
        darqube_api_key: String,
        is_production: bool,
    ) -> Self {
        let alpaca_base_url = if is_production {
            "https://api.alpaca.markets".to_string()
        } else {
            "https://paper-api.alpaca.markets".to_string()
        };

        let darqube_base_url = "https://api.darqube.com".to_string();

        Client {
            alpaca_base_url,
            alpaca_api_key_id,
            alpaca_api_secret_key,
            darqube_base_url,
            darqube_api_key,
            http_client: HTTPClient::new(),
        }
    }

    async fn send_alpaca_api_request(
        &self,
        url_path: &str,
        query_parameters: Option<HashMap<&str, &str>>,
        json_body: Option<serde_json::Value>,
    ) -> Result<Response, Error> {
        let mut alpaca_url = Url::parse(&self.alpaca_base_url)?.join(url_path)?;

        match query_parameters {
            Some(query_parameters) => {
                for (key, value) in query_parameters.iter() {
                    alpaca_url.query_pairs_mut().append_pair(key, value);
                }
            }
            None => {}
        }

        let mut request_builder: RequestBuilder;
        match json_body {
            Some(json_body) => {
                request_builder = self.http_client.post(alpaca_url);
                request_builder = request_builder.json(&json_body);
                request_builder = request_builder.header("content-type", "application/json");
            }
            None => {
                request_builder = self.http_client.get(alpaca_url);
            }
        }

        let response = request_builder
            .header("APCA-API-KEY-ID", &self.alpaca_api_key_id)
            .header("APCA-API-SECRET-KEY", &self.alpaca_api_secret_key)
            .header("accept", "application/json")
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(Error::OtherError(format!(
                "Alpaca API request failed with status: {}",
                response.status()
            )));
        }

        Ok(response)
    }
}

#[async_trait]
impl Interface for Client {
    async fn get_available_tickers(&self) -> Result<Vec<String>, Error> {
        // "GSPC" is the S&P 500 Index
        // "DJI" is the Dow Jones Industrial Average
        let darqube_url_path = "data-api/fundamentals/indexes/index_constituents/GSPC";

        let darqube_index_constituents_url =
            Url::parse(&self.darqube_base_url)?.join(darqube_url_path)?;

        let response = self
            .http_client
            .get(darqube_index_constituents_url)
            .query(&[("token", &self.darqube_api_key)])
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(Error::OtherError(format!(
                "Alpaca API request failed with status: {}",
                response.status()
            )));
        }

        let darqube_response: DarqubeIndexConstituentsResponse = response.json().await?;

        let constituents = darqube_response
            .items
            .values()
            .map(|item| item.code.clone())
            .collect::<Vec<String>>();

        let response = self
            .send_alpaca_api_request(
                "v2/assets",
                Some(
                    [("status", "ACTIVE"), ("asset_class", "US_EQUITY")]
                        .iter()
                        .cloned()
                        .collect(),
                ),
                None,
            )
            .await?;

        let alpaca_response: Vec<AlpacaAsset> = response.json().await?;

        let mut tickers: Vec<String> = Vec::new();

        for asset in alpaca_response {
            if asset.tradable
                && asset.fractionable
                && asset.shortable
                && constituents.contains(&asset.symbol)
                && !asset.symbol.contains(".")
            {
                tickers.push(asset.symbol);
            }
        }

        Ok(tickers)
    }

    async fn get_portfolio_performance(
        &self,
        end_at: DateTime<Utc>,
    ) -> Result<PortfolioPerformance, Error> {
        let alpaca_portfolio_response = self
            .send_alpaca_api_request(
                "v2/account/portfolio/history",
                Some(
                    [
                        ("period", "1Y"),
                        ("timeframe", "1D"),
                        ("intraday_reporting", "market_hours"),
                        ("pnl_reset", "per_day"),
                        ("end", end_at.to_rfc3339().as_str()),
                    ]
                    .iter()
                    .cloned()
                    .collect(),
                ),
                None,
            )
            .await?;

        let alpaca_portfolio: AlpacaPortfolio = alpaca_portfolio_response.json().await?;

        let timestamps: Vec<DateTime<Utc>> = alpaca_portfolio
            .timestamps
            .iter()
            .map(|timestamp| DateTime::from_timestamp(*timestamp, 0).expect("Invalid timestamp"))
            .collect();

        let mut combined_values: Vec<(DateTime<Utc>, f64)> = timestamps
            .into_iter()
            .zip(alpaca_portfolio.equity_values.into_iter())
            .collect();

        combined_values.sort_by(|a, b| b.0.cmp(&a.0));

        let (timestamps, equity_values): (Vec<_>, Vec<_>) = combined_values.into_iter().unzip();

        let portfolio = PortfolioPerformance {
            timestamps,
            equity_values,
        };

        Ok(portfolio)
    }

    async fn get_portfolio_positions(&self) -> Result<Vec<PortfolioPosition>, Error> {
        let alpaca_portfolio_response = self
            .send_alpaca_api_request("v2/positions", None, None)
            .await?;

        let portfolio_positions: Vec<PortfolioPosition> = alpaca_portfolio_response.json().await?;

        Ok(portfolio_positions)
    }

    async fn check_orders_pattern_day_trade_restrictions(
        &self,
        orders: Vec<Order>,
    ) -> Result<Vec<PatternDayTraderCheck>, Error> {
        let after_timestamp = Utc::now() - Duration::hours(24);

        let symbols: Vec<String> = orders.iter().map(|order| order.ticker.clone()).collect();

        let alpaca_orders_response = self
            .send_alpaca_api_request(
                "v2/orders",
                Some(
                    [
                        ("status", "closed"),
                        ("after", after_timestamp.to_rfc3339().as_str()),
                        ("direction", "desc"),
                        ("symbols", symbols.join(",").as_str()),
                    ]
                    .iter()
                    .cloned()
                    .collect(),
                ),
                None,
            )
            .await?;

        let prior_orders: Vec<AlpacaOrder> = alpaca_orders_response.json().await?;

        let mut results: Vec<PatternDayTraderCheck> = Vec::new();

        for order in orders {
            let mut is_pdt_violated = false;

            for prior_order in prior_orders.iter() {
                if prior_order.ticker == order.ticker {
                    if prior_order.position_intent == "buy_to_open" && order.side == Side::Short {
                        is_pdt_violated = true;
                    } else if prior_order.position_intent == "sell_to_open"
                        && order.side == Side::Long
                    {
                        is_pdt_violated = true;
                    }
                }
            }

            results.push(PatternDayTraderCheck {
                ticker: order.ticker.clone(),
                is_pdt_violated,
            });
        }

        Ok(results)
    }

    async fn execute_orders(&self, orders: Vec<Order>) -> Result<(), Error> {
        for order in orders {
            self.send_alpaca_api_request(
                "v2/orders",
                None,
                Some(serde_json::json!({
                    "symbol": order.ticker,
                    "side": match order.side {
                        Side::Long => "buy",
                        Side::Short => "sell",
                    },
                    "type": "market",
                    "time_in_force": "day",
                    "qty": order.quantity
                })),
            )
            .await?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockito;
    use serde_json::json;

    #[test]
    fn test_new() {
        let client = Client::new(
            "alpaca_api_key_id".to_string(),
            "alpaca_api_secret_key".to_string(),
            "darqube_api_key".to_string(),
            false,
        );

        assert_eq!(
            client.alpaca_base_url,
            "https://paper-api.alpaca.markets".to_string()
        );
        assert_eq!(client.alpaca_api_key_id, "alpaca_api_key_id".to_string());
        assert_eq!(
            client.alpaca_api_secret_key,
            "alpaca_api_secret_key".to_string()
        );
        assert_eq!(
            client.darqube_base_url,
            "https://api.darqube.com".to_string()
        );
        assert_eq!(client.darqube_api_key, "darqube_api_key".to_string());
    }

    #[tokio::test]
    async fn test_get_available_tickers() {
        let mut mock_server = tokio::task::spawn_blocking(|| mockito::Server::new())
            .await
            .unwrap();

        let base_url = mock_server.url().to_string();

        let client = Client {
            alpaca_base_url: base_url.clone(),
            alpaca_api_key_id: "alpaca_api_key_id".to_string(),
            alpaca_api_secret_key: "alpaca_api_secret_key".to_string(),
            darqube_base_url: base_url.clone(),
            darqube_api_key: "darqube_api_key".to_string(),
            http_client: HTTPClient::new(),
        };

        let mock_darqube_response = json!(
            {
                "0": {
                    "Code": "AAPL"
                }
            }
        );

        mock_server
            .mock(
                "GET",
                "/data-api/fundamentals/indexes/index_constituents/GSPC",
            )
            .match_query(mockito::Matcher::AllOf(vec![mockito::Matcher::UrlEncoded(
                "token".into(),
                "darqube_api_key".into(),
            )]))
            .with_status(200)
            .with_header("content-type", "application/json")
            .with_body(mock_darqube_response.to_string())
            .create();

        let mock_alpaca_response = json!(
            [
                {
                    "tradable": true,
                    "fractionable": true,
                    "shortable": true,
                    "symbol": "AAPL"
                }
            ]
        );

        mock_server
            .mock("GET", "/v2/assets")
            .match_header("APCA-API-KEY-ID", "alpaca_api_key_id")
            .match_header("APCA-API-SECRET-KEY", "alpaca_api_secret_key")
            .match_header("accept", "application/json")
            .match_query(mockito::Matcher::AllOf(vec![
                mockito::Matcher::UrlEncoded("status".into(), "ACTIVE".into()),
                mockito::Matcher::UrlEncoded("asset_class".into(), "US_EQUITY".into()),
            ]))
            .with_status(200)
            .with_body(mock_alpaca_response.to_string())
            .create();

        match client.get_available_tickers().await {
            Ok(result) => {
                assert_eq!(result, vec!["AAPL".to_string()]);
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[tokio::test]
    async fn test_get_portfolio_performance() {
        let mut mock_server = tokio::task::spawn_blocking(|| mockito::Server::new())
            .await
            .unwrap();

        let base_url = mock_server.url().to_string();

        let client = Client {
            alpaca_base_url: base_url.clone(),
            alpaca_api_key_id: "alpaca_api_key_id".to_string(),
            alpaca_api_secret_key: "alpaca_api_secret_key".to_string(),
            darqube_base_url: base_url.clone(),
            darqube_api_key: "darqube_api_key".to_string(),
            http_client: HTTPClient::new(),
        };

        let mock_alpaca_response = json!(
            {
                "timestamp": [1614556800],
                "equity": [1000.0]
            }
        );

        mock_server
            .mock("GET", "/v2/account/portfolio/history")
            .match_query(mockito::Matcher::AllOf(vec![
                mockito::Matcher::UrlEncoded("period".into(), "1Y".into()),
                mockito::Matcher::UrlEncoded("timeframe".into(), "1D".into()),
                mockito::Matcher::UrlEncoded("intraday_reporting".into(), "market_hours".into()),
                mockito::Matcher::UrlEncoded("pnl_reset".into(), "per_day".into()),
                mockito::Matcher::UrlEncoded(
                    "end".into(),
                    DateTime::from_timestamp(1614556800, 0)
                        .expect("Invalid timestamp")
                        .to_rfc3339(),
                ),
            ]))
            .match_header("APCA-API-KEY-ID", "alpaca_api_key_id")
            .match_header("APCA-API-SECRET-KEY", "alpaca_api_secret_key")
            .match_header("accept", "application/json")
            .with_status(200)
            .with_body(mock_alpaca_response.to_string())
            .create();

        match client
            .get_portfolio_performance(DateTime::from_timestamp(1614556800, 0).unwrap())
            .await
        {
            Ok(result) => {
                assert_eq!(result.timestamps.len(), 1);
                assert_eq!(result.equity_values.len(), 1);
                assert_eq!(
                    result.timestamps[0],
                    DateTime::from_timestamp(1614556800, 0).unwrap()
                );
                assert_eq!(result.equity_values[0], 1000.0);
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[tokio::test]
    async fn test_get_portfolio_positions() {
        let mut mock_server = tokio::task::spawn_blocking(|| mockito::Server::new())
            .await
            .unwrap();

        let base_url = mock_server.url().to_string();

        let client = Client {
            alpaca_base_url: base_url.clone(),
            alpaca_api_key_id: "alpaca_api_key_id".to_string(),
            alpaca_api_secret_key: "alpaca_api_secret_key".to_string(),
            darqube_base_url: base_url.clone(),
            darqube_api_key: "darqube_api_key".to_string(),
            http_client: HTTPClient::new(),
        };

        let mock_alpaca_response = json!(
            [
                {
                    "symbol": "AAPL",
                    "side": "long",
                    "qty": 1.0
                }
            ]
        );

        mock_server
            .mock("GET", "/v2/positions")
            .match_header("APCA-API-KEY-ID", "alpaca_api_key_id")
            .match_header("APCA-API-SECRET-KEY", "alpaca_api_secret_key")
            .match_header("accept", "application/json")
            .with_status(200)
            .with_body(mock_alpaca_response.to_string())
            .create();

        match client.get_portfolio_positions().await {
            Ok(result) => {
                assert_eq!(result.len(), 1);
                assert_eq!(result[0].ticker, "AAPL".to_string());
                assert_eq!(result[0].side, Side::Long);
                assert_eq!(result[0].quantity, 1.0);
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[tokio::test]
    async fn test_check_orders_pattern_day_trade_restrictions() {
        let mut mock_server = tokio::task::spawn_blocking(|| mockito::Server::new())
            .await
            .unwrap();

        let base_url = mock_server.url().to_string();

        let client = Client {
            alpaca_base_url: base_url.clone(),
            alpaca_api_key_id: "alpaca_api_key_id".to_string(),
            alpaca_api_secret_key: "alpaca_api_secret_key".to_string(),
            darqube_base_url: base_url.clone(),
            darqube_api_key: "darqube_api_key".to_string(),
            http_client: HTTPClient::new(),
        };

        let mock_alpaca_response = json!(
            [
                {
                    "symbol": "AAPL",
                    "position_intent": "buy_to_open"
                }
            ]
        );

        mock_server
            .mock("GET", "/v2/orders")
            .match_header("APCA-API-KEY-ID", "alpaca_api_key_id")
            .match_header("APCA-API-SECRET-KEY", "alpaca_api_secret_key")
            .match_header("accept", "application/json")
            .match_query(mockito::Matcher::AllOf(vec![
                mockito::Matcher::UrlEncoded("status".into(), "closed".into()),
                mockito::Matcher::UrlEncoded("direction".into(), "desc".into()),
                mockito::Matcher::UrlEncoded("symbols".into(), "AAPL".into()),
            ]))
            .with_status(200)
            .with_body(mock_alpaca_response.to_string())
            .create();

        let orders = vec![Order {
            ticker: "AAPL".to_string(),
            side: Side::Short,
            quantity: 1.0,
        }];

        match client
            .check_orders_pattern_day_trade_restrictions(orders)
            .await
        {
            Ok(result) => {
                assert_eq!(result.len(), 1);
                assert_eq!(result[0].ticker, "AAPL".to_string());
                assert_eq!(result[0].is_pdt_violated, true);
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[tokio::test]
    async fn test_execute_orders() {
        let mut mock_server = tokio::task::spawn_blocking(|| mockito::Server::new())
            .await
            .unwrap();

        let base_url = mock_server.url().to_string();

        let client = Client {
            alpaca_base_url: base_url.clone(),
            alpaca_api_key_id: "alpaca_api_key_id".to_string(),
            alpaca_api_secret_key: "alpaca_api_secret_key".to_string(),
            darqube_base_url: base_url.clone(),
            darqube_api_key: "darqube_api_key".to_string(),
            http_client: HTTPClient::new(),
        };

        mock_server
            .mock("POST", "/v2/orders")
            .match_header("APCA-API-KEY-ID", "alpaca_api_key_id")
            .match_header("APCA-API-SECRET-KEY", "alpaca_api_secret_key")
            .match_header("accept", "application/json")
            .match_header("content-type", "application/json")
            .match_body(mockito::Matcher::Json(json!({
                "symbol": "AAPL",
                "side": "buy",
                "type": "market",
                "time_in_force": "day",
                "qty": 1.0
            })))
            .with_status(200)
            .create();

        let orders = vec![Order {
            ticker: "AAPL".to_string(),
            side: Side::Long,
            quantity: 1.0,
        }];

        match client.execute_orders(orders).await {
            Ok(_) => {
                assert_eq!(true, true);
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }
}
