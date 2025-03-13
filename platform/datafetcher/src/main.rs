use actix_web::middleware::Logger;
use actix_web::{post, web, App, HttpResponse, HttpServer};
use async_trait::async_trait;
use chrono::{DateTime, Duration, Utc};
use cloudevents::Event;
use log::info;
use mockall::automock;
use mockall::mock;
use pocketsizefund::data::{
    Bar, Client as DataClient, Error as DataError, Interface as DataInterface, Object,
    Type as DataType,
};
use pocketsizefund::events::build_response_event;
use pocketsizefund::trade::{
    Client as TradeClient, Error as TradeError, Interface as TradeInterface, Order,
    PatternDayTraderCheck, PortfolioPerformance, PortfolioPosition,
};
use reqwest::header::{HeaderMap, HeaderValue, InvalidHeaderValue, ACCEPT};
use reqwest::{Client as HTTPClient, Error as ReqwestError, Url};
use serde::Deserialize;
use serde_json::json;
use std::env;
use std::io;
use std::num::ParseIntError;
use std::sync::Arc;
use thiserror::Error as ThisError;
use url::ParseError;

#[derive(ThisError, Debug)]
pub enum Error {
    #[error("Parse URL error: {0}")]
    ParseURLError(#[from] ParseError),
    #[error("Invalid header value: {0}")]
    InvalidHeaderValue(#[from] InvalidHeaderValue),
    #[error("Reqwest error: {0}")]
    ReqwestError(#[from] ReqwestError),
    #[error("Other error: {0}")]
    OtherError(String),
}

#[post("/health")]
async fn health_handler() -> HttpResponse {
    HttpResponse::Ok().body("OK")
}

#[derive(Deserialize)]
struct BarsResponse {
    bars: Vec<Bar>,
    next_page_token: Option<String>,
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
}

#[derive(Clone)]
struct Client {
    http_client: HTTPClient,
    alpaca_base_url: String,
    alpaca_api_key_id: String,
    alpaca_api_secret_key: String,
}

impl Client {
    fn new(
        alpaca_base_url: String,
        alpaca_api_key_id: String,
        alpaca_api_secret_key: String,
    ) -> Self {
        Client {
            http_client: HTTPClient::new(),
            alpaca_base_url,
            alpaca_api_key_id,
            alpaca_api_secret_key,
        }
    }
}

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
            let path = format!("v2/stocks/{}/bars", ticker);

            let start_str = start.format("%Y-%m-%dT%H:%M:%SZ").to_string();
            let end_str = end.format("%Y-%m-%dT%H:%M:%SZ").to_string();

            let mut equities_bars_url = Url::parse(&self.alpaca_base_url)?.join(&path)?;

            loop {
                {
                    let mut query_pairs = equities_bars_url.query_pairs_mut();

                    query_pairs
                        .append_pair("timeframe", "1D")
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

                let response = self
                    .http_client
                    .get(&path)
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
}

#[post("/data")]
async fn data_handler(
    fetch_client: web::Data<Arc<dyn Interface>>,
    data_client: web::Data<Arc<dyn DataInterface>>,
    trade_client: web::Data<Arc<dyn TradeInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let old_objects = data_client.load(DataType::Bar).await.unwrap_or_else(|e| {
        info!("Failed to load old bars: {}", e);
        Vec::new()
    });

    let old_bars = extract_bars(old_objects);

    let most_recent_datetime = old_bars
        .iter()
        .max_by_key(|bar| bar.timestamp)
        .map(|bar| bar.timestamp)
        .unwrap_or_else(|| {
            info!("No maximum timestamp found in old bars, using fallback value.");
            Utc::now() - Duration::days(365 * 10)
        });

    let current_datetime = chrono::Utc::now();

    let available_tickers = trade_client
        .get_available_tickers()
        .await
        .unwrap_or_else(|e| {
            info!("Failed to get available tickers: {}", e);
            Vec::new()
        });

    let new_bars = fetch_client
        .fetch_equities_bars(available_tickers, most_recent_datetime, current_datetime)
        .await
        .unwrap_or_else(|e| {
            info!("Failed to fetch new bars: {}", e);
            Vec::new()
        });

    Ok(build_response_event(
        "datafetcher".to_string(),
        vec![
            "equities".to_string(),
            "bars".to_string(),
            "new".to_string(),
        ],
        Some(
            json!({
                "status": "success".to_string(),
                "equities_bars": new_bars,
            })
            .to_string(),
        ),
    ))
}

fn extract_bars(objects: Vec<Object>) -> Vec<Bar> {
    objects
        .into_iter()
        .filter_map(|obj| {
            if let Object::Bar(bar) = obj {
                Some(bar)
            } else {
                None
            }
        })
        .collect()
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let server_port_environment_variable = env::var("SERVER_PORT").unwrap_or("8080".to_string());

    let server_port = server_port_environment_variable
        .parse::<u16>()
        .map_err(|e: ParseIntError| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    let data_client = DataClient::new(
        env::var("AWS_ACCESS_KEY_ID").expect("AWS access key ID"),
        env::var("AWS_SECRET_ACCESS_KEY").expect("AWS secret access key"),
        env::var("S3_DATA_BUCKET_NAME").expect("S3 data bucket name"),
    );

    let data_client: Arc<dyn DataInterface> = Arc::new(data_client);

    let data_client = web::Data::new(data_client);

    let trade_client = TradeClient::new(
        env::var("ALPACA_API_KEY").expect("Alpaca API key"),
        env::var("ALPACA_API_SECRET").expect("Alpaca API secret"),
        env::var("DARQUBE_API_KEY").expect("Darqube API key"),
        env::var("ENVIRONMENT")
            .expect("Environment")
            .eq("production"),
    );

    let trade_client: Arc<dyn TradeInterface> = Arc::new(trade_client);

    let trade_client = web::Data::new(trade_client);

    let fetch_client = Client::new(
        env::var("ALPACA_BASE_URL").expect("Alpaca base URL"),
        env::var("ALPACA_API_KEY").expect("Alpaca API key"),
        env::var("ALPACA_API_SECRET").expect("Alpaca API secret"),
    );

    let fetch_client: Arc<dyn Interface> = Arc::new(fetch_client);

    let fetch_client = web::Data::new(fetch_client);

    HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .app_data(fetch_client.clone())
            .app_data(data_client.clone())
            .app_data(trade_client.clone())
            .service(health_handler)
            .service(data_handler)
    })
    .bind(("0.0.0.0", server_port))?
    .run()
    .await
}

mock! {
    pub InterfaceMock {}

    #[async_trait::async_trait]
    impl Interface for InterfaceMock {
        async fn fetch_equities_bars(
            &self,
            tickers: Vec<String>,
            start: DateTime<Utc>,
            end: DateTime<Utc>,
        ) -> Result<Vec<Bar>, Error>;
    }
}

mock! {
    pub DataInterfaceMock {}

    #[async_trait::async_trait]
    impl DataInterface for DataInterfaceMock {
        async fn store(
            &self,
            objects: Vec<Object>,
        ) -> Result<(), DataError>;
        async fn load(&self, object_type: DataType) -> Result<Vec<Object>, DataError>;
    }
}

mock! {
    pub TradeInterfaceMock {}

    #[async_trait::async_trait]
    impl TradeInterface for TradeInterfaceMock {
        async fn get_available_tickers(&self) -> Result<Vec<String>, TradeError>;
        async fn get_portfolio_performance(&self, current_time: DateTime<Utc>) -> Result<PortfolioPerformance, TradeError>;
        async fn get_portfolio_positions(&self) -> Result<Vec<PortfolioPosition>, TradeError>;
        async fn check_orders_pattern_day_trade_restrictions(
            &self,
            orders: Vec<Order>,
        ) -> Result<Vec<PatternDayTraderCheck>, TradeError>;
        async fn execute_orders(&self, orders: Vec<Order>) -> Result<(), TradeError>;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::{http::header::ContentType, test, App};
    use chrono::{TimeZone, Utc};

    #[actix_web::test]
    async fn test_health_handler() {
        let app = test::init_service(App::new().service(health_handler)).await;

        let request = test::TestRequest::post()
            .uri("/health")
            .insert_header(ContentType::plaintext())
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }

    #[actix_web::test]
    async fn test_data_handler() {
        let mut mock_fetch_client: MockInterfaceMock = MockInterfaceMock::new();

        mock_fetch_client
            .expect_fetch_equities_bars()
            .returning(|_, _, _| {
                Ok(vec![Bar {
                    ticker: Some("AAPL".to_string()),
                    timestamp: Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap(),
                    open_price: 150.5,
                    high_price: 152.3,
                    low_price: 149.8,
                    close_price: 151.0,
                    volume: 1_200_000,
                    number_of_trades: 5_000,
                    volume_weighted_average_price: 150.9,
                }])
            });

        let mut mock_data_client: MockDataInterfaceMock = MockDataInterfaceMock::new();

        mock_data_client.expect_load().returning(|_| {
            Ok(vec![Object::Bar(Bar {
                ticker: Some("AAPL".to_string()),
                timestamp: Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap(),
                open_price: 150.0,
                high_price: 152.5,
                low_price: 149.5,
                close_price: 151.5,
                volume: 1_000_000,
                number_of_trades: 5_000,
                volume_weighted_average_price: 151.2,
            })])
        });

        mock_data_client.expect_store().returning(|_| Ok(()));

        let mut mock_trade_client: MockTradeInterfaceMock = MockTradeInterfaceMock::new();

        mock_trade_client
            .expect_get_available_tickers()
            .returning(|| Ok(vec!["AAPL".to_string()]));

        let mock_fetch_client: Arc<dyn Interface> = Arc::new(mock_fetch_client);

        let mock_fetch_client = web::Data::new(mock_fetch_client);

        let mock_data_client: Arc<dyn DataInterface> = Arc::new(mock_data_client);

        let mock_data_client = web::Data::new(mock_data_client);

        let mock_trade_client: Arc<dyn TradeInterface> = Arc::new(mock_trade_client);

        let mock_trade_client = web::Data::new(mock_trade_client);

        let app = test::init_service(
            App::new()
                .app_data(mock_fetch_client.clone())
                .app_data(mock_data_client.clone())
                .app_data(mock_trade_client.clone())
                .service(data_handler),
        )
        .await;

        let request = test::TestRequest::post()
            .uri("/data")
            .insert_header(ContentType::plaintext())
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }
}
