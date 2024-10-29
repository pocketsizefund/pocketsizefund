use async_trait::async_trait;
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

#[async_trait]
pub trait Interface: Send + Sync {
    async fn get_available_tickers(&self) -> Result<Vec<String>, Box<dyn std::error::Error>>;
    async fn execute_baseline_buy(&self, ticker: String) -> Result<(), Box<dyn std::error::Error>>;
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
}
