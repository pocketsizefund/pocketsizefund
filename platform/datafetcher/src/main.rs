use actix_web::middleware::Logger;
use actix_web::{post, web, App, HttpResponse, HttpServer};
use chrono::{DateTime, Duration, Utc};
use cloudevents::Event;
use log::info;
use mockall::mock;
use pocketsizefund::data::{Bar, Client as DataClient, Interface as DataInterface, Prediction};
use pocketsizefund::events::build_response_event;
use pocketsizefund::trade::{
    Client as TradeClient, Interface as TradeInterface, Order, PatternDayTraderCheck,
    PortfolioPerformance, PortfolioPosition,
};
use serde_json::json;
use std::env;
use std::io;
use std::num::ParseIntError;
use std::sync::Arc;

#[post("/health")]
async fn health_handler() -> HttpResponse {
    HttpResponse::Ok().body("OK")
}

#[post("/data")]
async fn data_handler(
    data_client: web::Data<Arc<dyn DataInterface>>,
    trade_client: web::Data<Arc<dyn TradeInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let old_bars = data_client.load_equities_bars().await.unwrap_or_else(|e| {
        info!("Failed to load old bars: {}", e);
        Vec::new()
    });

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

    let new_bars = data_client
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

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let server_port_environment_variable = env::var("SERVER_PORT").unwrap_or("8080".to_string());

    let server_port = server_port_environment_variable
        .parse::<u16>()
        .map_err(|e: ParseIntError| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    let data_client = DataClient::new(
        env::var("ALPACA_API_KEY").expect("Alpaca API key"),
        env::var("ALPACA_API_SECRET").expect("Alpaca API secret"),
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
        env::var("IS_PRODUCTION")
            .expect("Production flag not found")
            .parse()
            .expect("Production flag not a boolean"),
    );

    let trade_client: Arc<dyn TradeInterface> = Arc::new(trade_client);

    let trade_client = web::Data::new(trade_client);

    HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
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
    pub DataInterfaceMock {}

    #[async_trait::async_trait]
    impl DataInterface for DataInterfaceMock {
        async fn fetch_equities_bars(
            &self,
            tickers: Vec<String>,
            start: DateTime<Utc>,
            end: DateTime<Utc>,
        ) -> Result<Vec<Bar>, Box<dyn std::error::Error>>;
        async fn write_equities_bars(
            &self,
            equities_bars: Vec<Bar>,
        ) -> Result<(), Box<dyn std::error::Error>>;
        async fn load_equities_bars(&self) -> Result<Vec<Bar>, Box<dyn std::error::Error>>;
        async fn write_predictions(
            &self,
            predictions: Vec<Prediction>,
        ) -> Result<(), Box<dyn std::error::Error>>;
        async fn load_predictions(&self) -> Result<Vec<Prediction>, Box<dyn std::error::Error>>;
    }
}

mock! {
    pub TradeInterfaceMock {}

    #[async_trait::async_trait]
    impl TradeInterface for TradeInterfaceMock {
        async fn get_available_tickers(&self) -> Result<Vec<String>, Box<dyn std::error::Error>>;
        async fn execute_baseline_buy(&self, ticker: String) -> Result<(), Box<dyn std::error::Error>>;
        async fn get_portfolio_performance(&self, current_time: DateTime<Utc>) -> Result<PortfolioPerformance, Box<dyn std::error::Error>>;
        async fn get_portfolio_positions(&self) -> Result<Vec<PortfolioPosition>, Box<dyn std::error::Error>>;
        async fn check_orders_pattern_day_trade_restrictions(
            &self,
            orders: Vec<Order>,
        ) -> Result<Vec<PatternDayTraderCheck>, Box<dyn std::error::Error>>;
        async fn execute_orders(&self, orders: Vec<Order>) -> Result<(), Box<dyn std::error::Error>>;
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
        let mut mock_data_client: MockDataInterfaceMock = MockDataInterfaceMock::new();

        mock_data_client.expect_load_equities_bars().returning(|| {
            Ok(vec![Bar {
                ticker: Some("AAPL".to_string()),
                timestamp: Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap(),
                open: 150.0,
                high: 152.5,
                low: 149.5,
                close: 151.5,
                volume: 1_000_000,
                number_of_trades: 5_000,
                volume_weighted_average_price: 151.2,
            }])
        });

        mock_data_client
            .expect_fetch_equities_bars()
            .returning(|_, _, _| {
                Ok(vec![Bar {
                    ticker: Some("AAPL".to_string()),
                    timestamp: Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap(),
                    open: 150.5,
                    high: 152.3,
                    low: 149.8,
                    close: 151.0,
                    volume: 1_200_000,
                    number_of_trades: 5_000,
                    volume_weighted_average_price: 150.9,
                }])
            });

        mock_data_client
            .expect_write_equities_bars()
            .returning(|_| Ok(()));

        let mut mock_trade_client: MockTradeInterfaceMock = MockTradeInterfaceMock::new();

        mock_trade_client
            .expect_get_available_tickers()
            .returning(|| Ok(vec!["AAPL".to_string()]));

        let mock_data_client: Arc<dyn DataInterface> = Arc::new(mock_data_client);

        let mock_data_client = web::Data::new(mock_data_client);

        let mock_trade_client: Arc<dyn TradeInterface> = Arc::new(mock_trade_client);

        let mock_trade_client = web::Data::new(mock_trade_client);

        let app = test::init_service(
            App::new()
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
