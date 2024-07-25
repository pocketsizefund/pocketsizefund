use chrono::{DateTime, Utc};
use reqwest::Url;
use reqwest::header::{HeaderMap, HeaderValue, ACCEPT};
use reqwest::Client as HTTPClient;
use aws_sdk_s3::client::Client as S3Client;
use aws_credential_types::Credentials;
use aws_types::region::Region;
use aws_sdk_s3::Config;
use serde::{Serialize, Deserialize};
use serde_json;
use aws_sdk_s3::primitives::ByteStream;
use flate2::write::GzEncoder;
use flate2::read::GzDecoder;
use flate2::Compression;
use std::io::{Write, Read};

#[derive(Deserialize)]
struct BarsResponse {
    bars: Vec<Bar>,
    #[serde(rename = "next_page_token")]
    next_token: Option<String>,
}

#[derive(Serialize, Deserialize, Debug)]
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

pub struct Client {
    alpaca_base_url: String,
    alpaca_api_key_id: String,
    alpaca_api_secret_key: String,
    http_client: HTTPClient,
    s3_client: S3Client,
    s3_data_bucket_name: String,
}

const EQUITY_BARS_PATH: &str = "equity/bars";

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
            alpaca_base_url: "https://data.alpaca.markets/v2/".to_string(),
            alpaca_api_key_id,
            alpaca_api_secret_key,
            http_client: HTTPClient::new(),
            s3_client,
            s3_data_bucket_name,
        }
    }

    pub async fn fetch_equities_bars_by_date_range(
        &self,
        tickers: Vec<String>,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
    ) -> Result<Vec<Bar>, Box<dyn std::error::Error>> {
        let mut equities_bars: Vec<Bar> = Vec::new();

        let mut headers = HeaderMap::new();
        headers.insert(ACCEPT, HeaderValue::from_str("application/json")?);
        headers.insert("APCA-API-KEY-ID", HeaderValue::from_str(&self.alpaca_api_key_id)?);
        headers.insert("APCA-API-SECRET-KEY", HeaderValue::from_str(&self.alpaca_api_secret_key)?);

        for ticker in tickers {
            loop {
                let mut page_token: Option<String> = None;

                let alpaca_url = self.build_equities_bars_url(&ticker, start, end, page_token.as_deref())?;

                let response = self.http_client.get(alpaca_url)
                    .headers(headers.clone())
                    .send()
                    .await?;
        
                if !response.status().is_success() {
                    return Err(format!("api request failed with status: {}", response.status()).into());
                }

                let bar_response: BarsResponse = response.json().await?;
        
                let bars_with_ticker = bar_response.bars.into_iter()
                    .map(|mut bar| {
                        bar.ticker = Some(ticker.clone());
                        bar
                    })
                    .collect::<Vec<Bar>>();
        
                equities_bars.extend(bars_with_ticker);
        
                match bar_response.next_token {
                    Some(token) => page_token = Some(token),
                    None => break,
                }
            }
        }

        Ok(equities_bars)
    }

    fn build_equities_bars_url(
        &self,
        ticker: &String,
        start: DateTime<Utc>,
        end: DateTime<Utc>,
        page_token: Option<&str>,
    ) -> Result<Url, Box<dyn std::error::Error>> {
        let path = format!("stocks/{}/bars", ticker);

        let start_str = start.format("%Y-%m-%dT%H:%M:%SZ").to_string();
        let end_str = end.format("%Y-%m-%dT%H:%M:%SZ").to_string();

        let mut alpaca_url = Url::parse(&self.alpaca_base_url)?
            .join(&path)?;

        {
            let mut query_pairs = alpaca_url.query_pairs_mut();
            
            query_pairs.append_pair("timeframe", "1D")
                .append_pair("start", &start_str)
                .append_pair("end", &end_str)
                .append_pair("limit", "10000")
                .append_pair("adjustment", "all")
                .append_pair("feed", "sip")
                .append_pair("sort", "asc");

            if let Some(token) = page_token {
                query_pairs.append_pair("page_token", &token);
            }
        }

        Ok(alpaca_url)
    }

    pub async fn write_equities_bars_to_storage(&self, equities_bars: Vec<Bar> ) -> Result<(), Box<dyn std::error::Error>> {
        let equities_bars_json = serde_json::to_vec(&equities_bars).unwrap();

        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());

        encoder.write_all(&equities_bars_json)?;

        let compressed_data = encoder.finish()?;

        let equities_bars_bytes = ByteStream::from(compressed_data);

        let key = format!("{}/all.gz", EQUITY_BARS_PATH);

        let output = self.s3_client
            .put_object()
            .bucket(&self.s3_data_bucket_name)
            .key(key)
            .body(equities_bars_bytes)
            .send();

        let result = output.await;
    
        match result {
            Ok(_) => Ok(()),
            Err(e) => Err(Box::new(e) as Box<dyn std::error::Error>),
        }
    }

    pub async fn read_equities_bars_from_storage(&self) -> Result<Vec<Bar>, Box<dyn std::error::Error>> {
        let key = format!("{}/all.gz", EQUITY_BARS_PATH);

        let output = self.s3_client
            .get_object()
            .bucket(&self.s3_data_bucket_name)
            .key(key)
            .send()
            .await?;
        
        let compressed_data = output.body.collect().await?;

        let compressed_bytes = compressed_data.into_bytes();

        let mut decoder = GzDecoder::new(&compressed_bytes[..]);

        let mut decompressed_data = Vec::new();

        decoder.read_to_end(&mut decompressed_data)?;

        let equities_bars: Vec<Bar> = serde_json::from_slice(&decompressed_data)?;

        Ok(equities_bars)
    }
}
