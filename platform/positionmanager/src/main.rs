use actix_web::middleware::Logger;
use actix_web::{post, web, App, HttpResponse, HttpServer};
use chrono::{DateTime, Utc};
use cloudevents::event::Data;
use cloudevents::Event;
use mockall::mock;
use pocketsizefund::data::Prediction;
use pocketsizefund::events::build_response_event;
use pocketsizefund::trade::{
    Client as TradeClient, Error as TradeError, Interface as TradeInterface, Order,
    PatternDayTraderCheck, PortfolioPerformance, PortfolioPosition,
};
use rand::seq::SliceRandom;
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
    written_at: DateTime<Utc>,
}

#[post("/trade")]
async fn trade_handler(
    event: web::Json<Event>,
    trade_client: web::Data<Arc<dyn TradeInterface>>,
    data_provider_url: web::Data<Arc<Url>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut start_at = Utc::now().to_rfc3339();
    if let Some(Data::Json(json)) = event.data() {
        let payload: Payload = match serde_json::from_value(json.clone()) {
            Ok(val) => val,
            Err(error) => {
                return Err(error.into());
            }
        };

        start_at = payload.written_at.to_rfc3339();
    }

    let end_at = Utc::now().to_rfc3339();

    let client = HTTPClient::new();
    let data_provider_response: Response = client
        .post(data_provider_url.as_ref().to_string())
        .json(&json!({
            "start_at": start_at,
            "end_at": end_at
        }))
        .send()
        .await?;

    let predictions: Vec<Prediction> = data_provider_response.json().await?;

    let prediction = predictions.choose(&mut rand::thread_rng()).unwrap();

    trade_client
        .execute_baseline_buy(prediction.ticker.clone())
        .await?;

    let event = build_response_event(
        "positionmanager".to_string(),
        vec!["baselinebuy".to_string(), "executed".to_string()],
        Some(
            json!({
                "ticker": prediction.ticker,
            })
            .to_string(),
        ),
    );

    Ok(event)
}

#[actix_web::main]
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
        env::var("IS_PRODUCTION")
            .expect("Production flag not found")
            .parse()
            .expect("Production flag not a boolean"),
    );

    let trade_client = Arc::new(trade_client);

    let trade_client = web::Data::new(trade_client);

    let data_provider_url = format!(
        "http://data-provider.{}.svc.cluster.local:8080/predictions",
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
            .service(trade_handler)
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
        async fn execute_baseline_buy(&self, ticker: String) -> Result<(), TradeError>;
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
    use chrono::TimeZone;

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
    async fn test_trade_handler() {
        let mut mock_trade_client: MockTradeInterfaceMock = MockTradeInterfaceMock::new();

        mock_trade_client
            .expect_execute_baseline_buy()
            .times(1)
            .returning(|_| Ok(()));

        let mock_trade_client: Arc<dyn TradeInterface> = Arc::new(mock_trade_client);

        let mock_trade_client = web::Data::new(mock_trade_client);

        let mut mock_server = tokio::task::spawn_blocking(|| mockito::Server::new())
            .await
            .unwrap();

        mock_server
            .mock("POST", "/predictions")
            .with_body(
                json!(vec![Prediction {
                    ticker: "AAPL".to_string(),
                    timestamp: Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap(),
                    timestamps: vec![Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap()],
                    prices: vec![100.0],
                },])
                .to_string(),
            )
            .with_status(200)
            .create();

        let base_url = mock_server.url().to_string() + "/predictions";

        let base_url = Arc::new(Url::parse(&base_url).unwrap());

        let base_url = web::Data::new(base_url);

        let app = test::init_service(
            App::new()
                .service(trade_handler)
                .app_data(mock_trade_client.clone())
                .app_data(base_url),
        )
        .await;

        env::set_var("ALPACA_API_KEY", "VALUE");
        env::set_var("ALPACA_API_SECRET", "VALUE");
        env::set_var("DARQUBE_API_KEY", "VALUE");
        env::set_var("IS_PRODUCTION", "false");
        env::set_var("ENVIRONMENT", "development");

        let request = test::TestRequest::post()
            .uri("/trade")
            .insert_header(ContentType::json())
            .set_json(&json!({
                "specversion": "1.0",
                "type": "equities.predictions.updated",
                "source": "pocketiszefund.datacollector",
                "id": "1234",
                "time": "1997-05-25T20:00:00Z",
                "data": {
                    "written_at": "1997-05-25T20:00:00Z"
                }
            }))
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }
}
