use async_trait::async_trait;
use chrono::{DateTime, Utc};
use reqwest::Client as HTTPClient;
use reqwest::Url;
use serde::Deserialize;
use std::collections::HashMap;

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
pub struct Portfolio {
    pub timestamps: Vec<DateTime<Utc>>,
    pub equity_values: Vec<f64>,
}

#[async_trait]
pub trait Interface: Send + Sync {
    async fn get_available_tickers(&self) -> Result<Vec<String>, Box<dyn std::error::Error>>;
    async fn execute_baseline_buy(&self, ticker: String) -> Result<(), Box<dyn std::error::Error>>;
    async fn get_portfolio(
        &self,
        end_at: DateTime<Utc>,
    ) -> Result<Portfolio, Box<dyn std::error::Error>>;
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
}

#[async_trait]
impl Interface for Client {
    async fn get_available_tickers(&self) -> Result<Vec<String>, Box<dyn std::error::Error>> {
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
            return Err(
                format!("Darqube request failed with status: {}", response.status()).into(),
            );
        }

        let darqube_response: DarqubeIndexConstituentsResponse = response.json().await?;

        let constituents = darqube_response
            .items
            .values()
            .map(|item| item.code.clone())
            .collect::<Vec<String>>();

        let alpaca_assets_url = Url::parse(&self.alpaca_base_url)?.join("v2/assets")?;

        let response = self
            .http_client
            .get(alpaca_assets_url)
            .header("APCA-API-KEY-ID", &self.alpaca_api_key_id)
            .header("APCA-API-SECRET-KEY", &self.alpaca_api_secret_key)
            .header("accept", "application/json")
            .query(&[("status", "ACTIVE"), ("asset_class", "US_EQUITY")])
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(format!("Alpaca request failed with status: {}", response.status()).into());
        }

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

    async fn execute_baseline_buy(&self, ticker: String) -> Result<(), Box<dyn std::error::Error>> {
        let alpaca_order_url = Url::parse(&self.alpaca_base_url)?.join("v2/orders")?;

        let alpaca_order_response = self
            .http_client
            .post(alpaca_order_url)
            .header("APCA-API-KEY-ID", &self.alpaca_api_key_id)
            .header("APCA-API-SECRET-KEY", &self.alpaca_api_secret_key)
            .header("accept", "application/json")
            .header("content-type", "application/json")
            .json(&serde_json::json!({
                "symbol": ticker,
                "side": "buy",
                "type": "market",
                "time_in_force": "day",
                "notional": "1"
            }))
            .send()
            .await?;

        if !alpaca_order_response.status().is_success() {
            return Err(format!(
                "Alpaca request failed with status: {}",
                alpaca_order_response.status()
            )
            .into());
        }

        Ok(())
    }

    async fn get_portfolio(
        &self,
        end_at: DateTime<Utc>,
    ) -> Result<Portfolio, Box<dyn std::error::Error>> {
        let mut alpaca_portfolio_url =
            Url::parse(&self.alpaca_base_url)?.join("v2/account/portfolio/history")?;

        alpaca_portfolio_url
            .query_pairs_mut()
            .append_pair("period", "1Y")
            .append_pair("timeframe", "1D")
            .append_pair("intraday_reporting", "market_hours")
            .append_pair("pnl_reset", "per_day")
            .append_pair("end", end_at.to_rfc3339().as_str());

        let alpaca_portfolio_response = self
            .http_client
            .get(alpaca_portfolio_url)
            .header("APCA-API-KEY-ID", &self.alpaca_api_key_id)
            .header("APCA-API-SECRET-KEY", &self.alpaca_api_secret_key)
            .header("accept", "application/json")
            .header("content-type", "application/json")
            .send()
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

        let portfolio = Portfolio {
            timestamps,
            equity_values,
        };

        Ok(portfolio)
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
    async fn test_execute_baseline_buy() {
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
                "notional": "1"
            })))
            .with_status(200)
            .create();

        match client.execute_baseline_buy("AAPL".to_string()).await {
            Ok(_) => {
                assert_eq!(true, true);
            }
            Err(e) => {
                panic!("Error: {:?}", e);
            }
        }
    }

    #[tokio::test]
    async fn test_get_portfolio() {
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
            .match_header("content-type", "application/json")
            .with_status(200)
            .with_body(mock_alpaca_response.to_string())
            .create();

        match client
            .get_portfolio(DateTime::from_timestamp(1614556800, 0).unwrap())
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
}
