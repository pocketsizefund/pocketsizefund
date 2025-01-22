use async_trait::async_trait;
use aws_credential_types::Credentials;
use aws_sdk_s3::client::Client as S3Client;
use aws_sdk_s3::error::SdkError;
use aws_sdk_s3::operation::get_object::GetObjectError;
use aws_sdk_s3::primitives::{ByteStream, ByteStreamError};
use aws_sdk_s3::Config;
use aws_types::region::Region;
use chrono::{DateTime, Utc};
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;
use mockall::automock;
use reqwest::header::{HeaderMap, HeaderValue, InvalidHeaderValue, ACCEPT};
use reqwest::{Client as HTTPClient, Error as ReqwestError, Url};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use serde_json;
use serde_json::Error as SerdeError;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::io::{Error as IOError, Read, Write};
use thiserror::Error as ThisError;
use url::ParseError;

#[derive(Deserialize)]
struct BarsResponse {
    bars: Vec<Bar>,
    next_page_token: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Bar {
    pub ticker: Option<String>,
    #[serde(rename = "t")]
    pub timestamp: DateTime<Utc>,
    #[serde(rename = "o")]
    pub open: f32,
    #[serde(rename = "h")]
    pub high: f32,
    #[serde(rename = "l")]
    pub low: f32,
    #[serde(rename = "c")]
    pub close: f32,
    #[serde(rename = "v")]
    pub volume: u32,
    #[serde(rename = "n")]
    pub number_of_trades: u32,
    #[serde(rename = "vw")]
    pub volume_weighted_average_price: f32,
}

impl PartialEq for Bar {
    fn eq(&self, other: &Self) -> bool {
        self.ticker == other.ticker && self.timestamp == other.timestamp
    }
}

impl Eq for Bar {}

impl Hash for Bar {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ticker.hash(state);
        self.timestamp.hash(state);
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Prediction {
    pub ticker: String,
    pub timestamp: DateTime<Utc>,
    pub timestamps: Vec<DateTime<Utc>>,
    pub prices: Vec<f32>,
}

impl PartialEq for Prediction {
    fn eq(&self, other: &Self) -> bool {
        self.ticker == other.ticker && self.timestamp == other.timestamp
    }
}

impl Eq for Prediction {}

impl Hash for Prediction {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ticker.hash(state);
        self.timestamp.hash(state);
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Portfolio {
    pub timestamp: DateTime<Utc>,
    pub ticker: String,
    pub predicted_mean: f64,
    pub allocation: f64,
    pub investment_amount: f64,
}

impl Eq for Portfolio {}

impl PartialEq for Portfolio {
    fn eq(&self, other: &Self) -> bool {
        self.ticker == other.ticker && self.timestamp == other.timestamp
    }
}

impl Hash for Portfolio {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.timestamp.hash(state);
        self.ticker.hash(state);
    }
}

#[derive(ThisError, Debug)]
pub enum Error {
    #[error("Parse URL error: {0}")]
    ParseURLError(#[from] ParseError),
    #[error("Invalid header value: {0}")]
    InvalidHeaderValueError(#[from] InvalidHeaderValue),
    #[error("Request error: {0}")]
    ReqwestError(#[from] ReqwestError),
    #[error("IO error: {0}")]
    IOError(#[from] IOError),
    #[error("JSON error: {0}")]
    SerdeError(#[from] SerdeError),
    #[error("Byte stream error: {0}")]
    ByteStreamError(#[from] ByteStreamError),
    #[error("Get S3 object error: {0}")]
    SDKError(#[from] SdkError<GetObjectError>),
    #[error("Other error: {0}")]
    OtherError(String),
}

#[automock]
#[async_trait]
pub trait Interface: Send + Sync {
    async fn fetch_equities_bars(
        &self,
        tickers: Vec<String>,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Result<Vec<Bar>, Error>;
    async fn write_equities_bars(&self, equities_bars: Vec<Bar>) -> Result<(), Error>;
    async fn load_equities_bars(&self) -> Result<Vec<Bar>, Error>;
    async fn write_predictions(&self, predictions: Vec<Prediction>) -> Result<(), Error>;
    async fn load_predictions(&self) -> Result<Vec<Prediction>, Error>;
    async fn write_portfolios(&self, portfolio: Vec<Portfolio>) -> Result<(), Error>;
    async fn load_portfolios(&self) -> Result<Vec<Portfolio>, Error>;
}

#[derive(Clone)]
pub struct Client {
    alpaca_base_url: String,
    alpaca_api_key_id: String,
    alpaca_api_secret_key: String,
    http_client: HTTPClient,
    s3_client: S3Client,
    s3_data_bucket_name: String,
}

impl Client {
    pub fn new(
        alpaca_api_key_id: String,
        alpaca_api_secret_key: String,
        aws_access_key_id: String,
        aws_secret_access_key: String,
        s3_data_bucket_name: String,
    ) -> Self {
        let creds = Credentials::from_keys(aws_access_key_id, aws_secret_access_key, None);

        let conf = Config::builder()
            .credentials_provider(creds)
            .region(Region::new("us-east-1"))
            .build();

        let s3_client = S3Client::from_conf(conf);

        Client {
            alpaca_base_url: "https://data.alpaca.markets/".to_string(),
            alpaca_api_key_id,
            alpaca_api_secret_key,
            http_client: HTTPClient::new(),
            s3_client,
            s3_data_bucket_name,
        }
    }

    fn _build_equities_bars_url(
        &self,
        ticker: &String,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
        page_token: Option<&str>,
    ) -> Result<Url, Error> {
        let path = format!("v2/stocks/{}/bars", ticker);

        let start_str = start.format("%Y-%m-%dT%H:%M:%SZ").to_string();
        let end_str = end.format("%Y-%m-%dT%H:%M:%SZ").to_string();

        let mut alpaca_url = Url::parse(&self.alpaca_base_url)?.join(&path)?;

        {
            let mut query_pairs = alpaca_url.query_pairs_mut();

            query_pairs
                .append_pair("timeframe", "1D")
                .append_pair("start", &start_str)
                .append_pair("end", &end_str)
                .append_pair("limit", "10000")
                .append_pair("adjustment", "all")
                .append_pair("feed", "sip")
                .append_pair("sort", "asc");

            if let Some(token) = page_token {
                query_pairs.append_pair("page_token", token);
            }
        }

        Ok(alpaca_url)
    }
}

const EQUITIES_BARS_PATH: &str = "equities/bar/all.gz";

const PREDICTIONS_PATH: &str = "predictions/all.gz";

const PORTFOLIOS_PATH: &str = "portfolio/all.gz";

#[async_trait]
impl Interface for Client {
    async fn fetch_equities_bars(
        &self,
        tickers: Vec<String>,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Result<Vec<Bar>, Error> {
        let mut equities_bars: Vec<Bar> = Vec::new();

        let mut headers = HeaderMap::new();
        headers.insert(ACCEPT, HeaderValue::from_str("application/json")?);
        headers.insert(
            "APCA-API-KEY-ID",
            HeaderValue::from_str(&self.alpaca_api_key_id)?,
        );
        headers.insert(
            "APCA-API-SECRET-KEY",
            HeaderValue::from_str(&self.alpaca_api_secret_key)?,
        );

        for ticker in tickers {
            let mut page_token: Option<String> = None;
            loop {
                let alpaca_url =
                    self._build_equities_bars_url(&ticker, start, end, page_token.as_deref())?;

                let response = self
                    .http_client
                    .get(alpaca_url)
                    .headers(headers.clone())
                    .send()
                    .await?;

                if !response.status().is_success() {
                    return Err(Error::OtherError(format!(
                        "Alpaca API request failed with status: {}",
                        response.status()
                    )));
                }

                let bar_response: BarsResponse = response.json().await?;

                let bars_with_ticker = bar_response
                    .bars
                    .into_iter()
                    .map(|mut bar| {
                        bar.ticker = Some(ticker.clone());
                        bar
                    })
                    .collect::<Vec<Bar>>();

                equities_bars.extend(bars_with_ticker);

                match bar_response.next_page_token {
                    Some(token) => page_token = Some(token),
                    None => break,
                }
            }
        }

        Ok(equities_bars)
    }

    async fn write_equities_bars(&self, equities_bars: Vec<Bar>) -> Result<(), Error> {
        let mut combined_equities_bars: HashSet<Bar> = HashSet::new();

        let original_equities_bars: Vec<Bar> = load_objects(
            &self.s3_client,
            self.s3_data_bucket_name.clone(),
            EQUITIES_BARS_PATH.to_string(),
        )
        .await?;

        combined_equities_bars.extend(original_equities_bars.into_iter());

        combined_equities_bars.extend(equities_bars.into_iter());

        let result = write_objects(
            &self.s3_client,
            self.s3_data_bucket_name.clone(),
            EQUITIES_BARS_PATH.to_string(),
            &combined_equities_bars,
        )
        .await?;

        Ok(result)
    }

    async fn load_equities_bars(&self) -> Result<Vec<Bar>, Error> {
        let equities_bars = load_objects(
            &self.s3_client,
            self.s3_data_bucket_name.clone(),
            EQUITIES_BARS_PATH.to_string(),
        )
        .await?;

        Ok(equities_bars)
    }

    async fn write_predictions(&self, predictions: Vec<Prediction>) -> Result<(), Error> {
        let mut combined_predictions: HashSet<Prediction> = HashSet::new();

        let original_predictions: Vec<Prediction> = load_objects(
            &self.s3_client,
            self.s3_data_bucket_name.clone(),
            PREDICTIONS_PATH.to_string(),
        )
        .await?;

        combined_predictions.extend(original_predictions.into_iter());

        combined_predictions.extend(predictions.into_iter());

        let result = write_objects(
            &self.s3_client,
            self.s3_data_bucket_name.clone(),
            PREDICTIONS_PATH.to_string(),
            &combined_predictions,
        )
        .await?;

        Ok(result)
    }

    async fn load_predictions(&self) -> Result<Vec<Prediction>, Error> {
        let predictions = load_objects(
            &self.s3_client,
            self.s3_data_bucket_name.clone(),
            PREDICTIONS_PATH.to_string(),
        )
        .await?;

        Ok(predictions)
    }

    async fn write_portfolios(&self, portfolio: Vec<Portfolio>) -> Result<(), Error> {
        let mut combined_portfolios: HashSet<Portfolio> = HashSet::new();

        let original_portfolios: Vec<Portfolio> = load_objects(
            &self.s3_client,
            self.s3_data_bucket_name.clone(),
            PORTFOLIOS_PATH.to_string(),
        )
        .await?;

        combined_portfolios.extend(original_portfolios.into_iter());

        combined_portfolios.extend(portfolio.into_iter());

        let result = write_objects(
            &self.s3_client,
            self.s3_data_bucket_name.clone(),
            PORTFOLIOS_PATH.to_string(),
            &combined_portfolios,
        )
        .await?;

        Ok(result)
    }

    async fn load_portfolios(&self) -> Result<Vec<Portfolio>, Error> {
        let portfolios = load_objects(
            &self.s3_client,
            self.s3_data_bucket_name.clone(),
            PREDICTIONS_PATH.to_string(),
        )
        .await?;

        Ok(portfolios)
    }
}

async fn write_objects<T: Serialize>(
    s3_client: &S3Client,
    bucket: String,
    key: String,
    object: &T,
) -> Result<(), Error> {
    let objects_json = serde_json::to_vec(&object)?;

    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());

    encoder.write_all(&objects_json)?;

    let compressed_data = encoder.finish()?;

    let objects_bytes = ByteStream::from(compressed_data);

    let output = s3_client
        .put_object()
        .bucket(bucket)
        .key(key)
        .body(objects_bytes)
        .send();

    let result = output.await;

    match result {
        Ok(_) => Ok(()),
        Err(e) => Err(Error::OtherError(format!("Error writing objects: {e}"))),
    }
}

async fn load_objects<T: DeserializeOwned>(
    s3_client: &S3Client,
    bucket: String,
    key: String,
) -> Result<Vec<T>, Error> {
    let get_object_response = s3_client
        .get_object()
        .bucket(bucket)
        .key(key)
        .send()
        .await?;

    let compressed_data = get_object_response.body.collect().await?;

    let compressed_bytes = compressed_data.into_bytes();

    let mut decoder = GzDecoder::new(&compressed_bytes[..]);

    let mut decompressed_data = Vec::new();

    decoder.read_to_end(&mut decompressed_data)?;

    let predictions = serde_json::from_slice(&decompressed_data)?;

    Ok(predictions)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::TimeZone;
    use regex;
    use serde_json::json;

    #[test]
    fn test_build_equities_bars_url() {
        let client = Client {
            alpaca_base_url: "https://paper-api.alpaca.markets".to_string(),
            alpaca_api_key_id: "your_api_key".to_string(),
            alpaca_api_secret_key: "your_secret_key".to_string(),
            http_client: reqwest::Client::new(),
            s3_client: S3Client::from_conf(
                Config::builder().region(Region::new("us-east-1")).build(),
            ),
            s3_data_bucket_name: "your_bucket_name".to_string(),
        };

        let ticker = "AAPL".to_string();
        let start: DateTime<Utc> = Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap();
        let end: DateTime<Utc> = Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap();

        let page_token = Some("next_page_token");

        let result = client
            ._build_equities_bars_url(&ticker, start, end, page_token)
            .unwrap();

        assert_eq!(result.scheme(), "https");
        assert_eq!(result.host_str(), Some("paper-api.alpaca.markets"));
        assert_eq!(result.path(), "/v2/stocks/AAPL/bars");

        let query_pairs: std::collections::HashMap<_, _> =
            result.query_pairs().into_owned().collect();
        assert_eq!(query_pairs.get("timeframe"), Some(&"1D".to_string()));
        assert_eq!(
            query_pairs.get("start"),
            Some(&"1977-05-25T00:00:00Z".to_string())
        );
        assert_eq!(
            query_pairs.get("end"),
            Some(&"1977-05-26T00:00:00Z".to_string())
        );
        assert_eq!(query_pairs.get("limit"), Some(&"10000".to_string()));
        assert_eq!(query_pairs.get("adjustment"), Some(&"all".to_string()));
        assert_eq!(query_pairs.get("feed"), Some(&"sip".to_string()));
        assert_eq!(query_pairs.get("sort"), Some(&"asc".to_string()));
        assert_eq!(
            query_pairs.get("page_token"),
            Some(&"next_page_token".to_string())
        );
    }

    #[tokio::test]
    async fn test_fetch_equities_bars() {
        let mut mock_server = tokio::task::spawn_blocking(|| mockito::Server::new())
            .await
            .unwrap();

        let base_url = mock_server.url().to_string();

        let client = Client {
            alpaca_base_url: base_url,
            alpaca_api_key_id: "your_api_key".to_string(),
            alpaca_api_secret_key: "your_secret_key".to_string(),
            http_client: reqwest::Client::new(),
            s3_client: S3Client::from_conf(
                Config::builder().region(Region::new("us-east-1")).build(),
            ),
            s3_data_bucket_name: "your_bucket_name".to_string(),
        };

        let tickers = vec!["AAPL".to_string(), "GOOGL".to_string()];
        let start: DateTime<Utc> = Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap();
        let end: DateTime<Utc> = Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap();

        let mock_response = json!({
            "bars": [
                {
                    "t": "1977-05-25T00:00:00Z",
                    "o": 100.0,
                    "h": 105.0,
                    "l": 99.0,
                    "c": 102.0,
                    "v": 1000000,
                    "n": 100,
                    "vw": 102.0,
                }
            ],
            "next_token": null
        });

        for ticker in &tickers {
            let path = format!("/v2/stocks/{}/bars", ticker);
            mock_server
                .mock(
                    "GET",
                    mockito::Matcher::Regex(format!(r"^{}.*", regex::escape(&path))),
                )
                .match_query(mockito::Matcher::AllOf(vec![
                    mockito::Matcher::UrlEncoded("timeframe".into(), "1D".into()),
                    mockito::Matcher::UrlEncoded("start".into(), "1977-05-25T00:00:00Z".into()),
                    mockito::Matcher::UrlEncoded("end".into(), "1977-05-26T00:00:00Z".into()),
                    mockito::Matcher::UrlEncoded("limit".into(), "10000".into()),
                    mockito::Matcher::UrlEncoded("adjustment".into(), "all".into()),
                    mockito::Matcher::UrlEncoded("feed".into(), "sip".into()),
                    mockito::Matcher::UrlEncoded("sort".into(), "asc".into()),
                ]))
                .match_header("APCA-API-KEY-ID", "your_api_key")
                .match_header("APCA-API-SECRET-KEY", "your_secret_key")
                .with_status(200)
                .with_header("content-type", "application/json")
                .with_body(mock_response.to_string())
                .create();
        }

        let result = client
            .fetch_equities_bars(tickers, start, end)
            .await
            .unwrap();

        assert_eq!(result.len(), 2);
        for bar in result {
            assert!(bar.ticker.is_some());
            assert_eq!(bar.timestamp, start);
            assert_eq!(bar.open, 100.0);
            assert_eq!(bar.high, 105.0);
            assert_eq!(bar.low, 99.0);
            assert_eq!(bar.close, 102.0);
            assert_eq!(bar.volume, 1000000);
            assert_eq!(bar.number_of_trades, 100);
            assert_eq!(bar.volume_weighted_average_price, 102.0);
        }
    }
}
