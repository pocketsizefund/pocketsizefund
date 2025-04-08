pub mod calculators;
use crate::calculators::calculate_metrics;
use actix_web::middleware::Logger;
use actix_web::{main, post, HttpResponse};
use actix_web::{web, App, HttpServer};
use chrono::{DateTime, Utc};
use cloudevents::event::Data;
use cloudevents::Event;
use mockall::mock;
use pocketsizefund::data::Bar;
use pocketsizefund::events::build_response_event;
use pocketsizefund::trade::{
    Client as TradeClient, Error as TradeError, Interface as TradeInterface, Order,
    PatternDayTraderCheck, PortfolioPerformance, PortfolioPosition,
};
use reqwest::{Client as HTTPClient, Response};
use serde::Deserialize;
use serde_json::json;
use std::env;
use std::io;
use std::num::ParseIntError;
use std::sync::Arc;
use url::Url;

#[post("/health")]
async fn health_handler() -> HttpResponse {
    HttpResponse::Ok().body("OK")
}

#[derive(Deserialize)]
struct Payload {
    end_at: DateTime<Utc>,
}

#[post("/metrics")]
async fn metrics_handler(
    trade_client: web::Data<Arc<dyn TradeInterface>>,
    data_provider_url: web::Data<Arc<Url>>,
    event: web::Json<Event>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut end_at = Utc::now();
    if let Some(Data::Json(json)) = event.data() {
        let payload: Payload = match serde_json::from_value(json.clone()) {
            Ok(val) => val,
            Err(error) => {
                return Err(error.into());
            }
        };

        end_at = payload.end_at;
    }

    let start_at = Utc::now()
        .checked_sub_signed(chrono::Duration::days(365))
        .expect("Failed to calculate start at");

    let portfolio_performance: PortfolioPerformance =
        trade_client.get_portfolio_performance(end_at).await?;

    let client = HTTPClient::new();
    let data_provider_response: Response = client
        .post(data_provider_url.as_ref().to_string())
        .json(&json!({
            "start_at": start_at,
            "end_at": end_at,
        }))
        .send()
        .await?;

    let equity_bars: Vec<Bar> = data_provider_response.json().await?;

    let filtered_equity_bars: Vec<Bar> = equity_bars
        .into_iter()
        .filter(|bar| bar.ticker.as_deref() == Some("SPY"))
        .collect();

    filtered_equity_bars
        .clone()
        .sort_by(|a, b| b.timestamp.cmp(&a.timestamp));

    let portfolio_oldest = portfolio_performance
        .timestamps
        .first()
        .expect("Portfolio timestamps are empty");
    let bars_oldest = filtered_equity_bars
        .first()
        .expect("Equity bars are empty")
        .timestamp;

    let oldest_timestamp = if *portfolio_oldest < bars_oldest {
        *portfolio_oldest
    } else {
        bars_oldest
    };

    let portfolio_metrics = calculate_metrics(
        portfolio_performance.timestamps,
        portfolio_performance.equity_values,
        oldest_timestamp,
    );

    let equity_timestamps: Vec<DateTime<Utc>> = filtered_equity_bars
        .iter()
        .map(|bar| bar.timestamp)
        .collect();
    let equity_closes: Vec<f64> = filtered_equity_bars
        .iter()
        .map(|bar| bar.close_price as f64)
        .collect();

    let benchmark_metrics = calculate_metrics(equity_timestamps, equity_closes, oldest_timestamp);

    let event = build_response_event(
        "metricsreporter".to_string(),
        vec!["metrics".to_string(), "generated".to_string()],
        Some(
            json!({
                "portfolio_metrics": portfolio_metrics,
                "benchmark_metrics": benchmark_metrics,
                "status": "success".to_string(),
            })
            .to_string(),
        ),
    );

    Ok(event)
}

#[main]
async fn main() -> std::io::Result<()> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let server_port_environment_variable = env::var("SERVER_PORT").unwrap_or("8080".to_string());

    let server_port = server_port_environment_variable
        .parse::<u16>()
        .map_err(|e: ParseIntError| io::Error::new(io::ErrorKind::InvalidInput, e))?;

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

    let data_provider_url = format!(
        "http://dataprovider.{}.svc.cluster.local:8080/data",
        env::var("ENVIRONMENT").expect("Environment not found"),
    );

    let data_provider_url =
        Arc::new(Url::parse(&data_provider_url).expect("Data provider URL is invalid"));

    let data_provider_url = web::Data::new(data_provider_url);

    HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .app_data(trade_client.clone())
            .app_data(data_provider_url.clone())
            .service(health_handler)
            .service(metrics_handler)
    })
    .bind(("0.0.0.0", server_port))?
    .run()
    .await
}

mock! {
    pub TradeInterfaceMock {}

    #[async_trait::async_trait]
    impl TradeInterface for TradeInterfaceMock {
        async fn get_available_tickers(&self) -> Result<Vec<String>, TradeError>;
        async fn get_portfolio_performance(&self, end_at: DateTime<Utc>) -> Result<PortfolioPerformance, TradeError>;
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
    use serde_json::Value;

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
    async fn test_metrics_handler() {
        let mut mock_trade_client: MockTradeInterfaceMock = MockTradeInterfaceMock::new();

        mock_trade_client
            .expect_get_portfolio_performance()
            .returning(|_| {
                Ok(PortfolioPerformance {
                    timestamps: vec![
                        Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap(),
                        Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap(),
                        Utc.with_ymd_and_hms(1977, 5, 27, 0, 0, 0).unwrap(),
                    ],
                    equity_values: vec![100.0, 150.0, 200.0],
                })
            });

        let mock_trade_client: Arc<dyn TradeInterface> = Arc::new(mock_trade_client);

        let mock_trade_client = web::Data::new(mock_trade_client);

        let mut mock_data_provider_server = tokio::task::spawn_blocking(|| mockito::Server::new())
            .await
            .unwrap();

        mock_data_provider_server
            .mock("POST", "/data")
            .with_body(
                json!(vec![
                    Bar {
                        ticker: Some("SPY".to_string()),
                        timestamp: Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap(),
                        open_price: 150.0,
                        high_price: 152.5,
                        low_price: 149.5,
                        close_price: 151.5,
                        volume: 1_000_000,
                        number_of_trades: 5_000,
                        volume_weighted_average_price: 151.2,
                    },
                    Bar {
                        ticker: Some("SPY".to_string()),
                        timestamp: Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap(),
                        open_price: 151.0,
                        high_price: 155.5,
                        low_price: 149.5,
                        close_price: 152.5,
                        volume: 1_000_000,
                        number_of_trades: 5_000,
                        volume_weighted_average_price: 151.2,
                    },
                    Bar {
                        ticker: Some("SPY".to_string()),
                        timestamp: Utc.with_ymd_and_hms(1977, 5, 27, 0, 0, 0).unwrap(),
                        open_price: 152.0,
                        high_price: 159.5,
                        low_price: 149.5,
                        close_price: 151.5,
                        volume: 1_000_000,
                        number_of_trades: 5_000,
                        volume_weighted_average_price: 151.2,
                    },
                ])
                .to_string(),
            )
            .with_status(200)
            .create();

        let base_url = mock_data_provider_server.url().to_string() + "/data";

        let base_url = Arc::new(Url::parse(&base_url).unwrap());

        let base_url = web::Data::new(base_url);

        let app = test::init_service(
            App::new()
                .service(metrics_handler)
                .app_data(mock_trade_client.clone())
                .app_data(base_url),
        )
        .await;

        env::set_var("ALPACA_API_KEY", "VALUE");
        env::set_var("ALPACA_API_SECRET", "VALUE");
        env::set_var("DARQUBE_API_KEY", "VALUE");
        env::set_var("ENVIRONMENT", "development");

        let request = test::TestRequest::post()
            .uri("/metrics")
            .insert_header(ContentType::json())
            .set_json(&json!({
                "specversion": "1.0",
                "type": "metrics.get",
                "source": "pocketsizefund.dashboard",
                "id": "1234",
                "time": "1997-05-25T20:00:00Z",
                "data": {
                    "start_at": "1997-05-25T20:00:00Z",
                    "end_at": "1997-05-27T20:00:00Z"
                }
            }))
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());

        let body_bytes = test::read_body(response).await;
        let body_text = String::from_utf8(body_bytes.to_vec()).unwrap();
        let json_body: Value = serde_json::from_str(&body_text).unwrap();

        assert_eq!(
            json_body["benchmark_metrics"]["daily_mean_annualized"],
            38178.0
        );
    }
}
