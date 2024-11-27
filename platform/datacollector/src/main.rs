use actix_web::middleware::Logger;
use actix_web::{post, web, App, HttpResponse, HttpServer};
use chrono::{DateTime, Utc};
use cloudevents::{Data, Event};
use log::info;
use mockall::mock;
use pocketsizefund::data::{Bar, Client as DataClient, Interface as DataInterface, Prediction};
use pocketsizefund::events::build_response_event;
use pocketsizefund::trade::{
    Client as TradeClient, Interface as TradeInterface, Order, PatternDayTraderCheck,
    PortfolioPerformance, PortfolioPosition,
};
use serde::Deserialize;
use serde_json::json;
use std::env;
use std::io;
use std::num::ParseIntError;
use std::sync::Arc;

#[post("/health")]
async fn health_handler() -> HttpResponse {
    HttpResponse::Ok().body("OK")
}

#[derive(Deserialize)]
struct EquitiesBarsPayload {
    equities_bars: Vec<Bar>,
}

#[post("/data")]
async fn data_handler(
    event: web::Json<Event>,
    data_client: web::Data<Arc<dyn DataInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut equities_bars: Vec<Bar> = Vec::new();

    if let Some(Data::Json(json)) = event.data() {
        let payload: EquitiesBarsPayload = match serde_json::from_value(json.clone()) {
            Ok(val) => val,
            Err(error) => {
                return Err(error.into());
            }
        };

        equities_bars = payload.equities_bars;
    }

    match data_client.write_equities_bars(equities_bars).await {
        Ok(_) => {
            info!("New bars written successfully");
            Ok(build_response_event(
                "datapcollector".to_string(),
                vec![
                    "equities".to_string(),
                    "bars".to_string(),
                    "updated".to_string(),
                ],
                Some(
                    json!({
                        "status": "success".to_string(),
                    })
                    .to_string(),
                ),
            ))
        }
        Err(e) => {
            info!("Failed to write new bars: {}", e);
            Err(e)
        }
    }
}

#[derive(Deserialize)]
struct PredictionsPayload {
    predictions: Vec<Prediction>,
}

#[post("/predictions")]
async fn predictions_handler(
    event: web::Json<Event>,
    data_client: web::Data<Arc<dyn DataInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut predictions: Vec<Prediction> = Vec::new();

    if let Some(Data::Json(json)) = event.data() {
        let payload: PredictionsPayload = match serde_json::from_value(json.clone()) {
            Ok(val) => val,
            Err(error) => {
                return Err(error.into());
            }
        };

        predictions = payload.predictions;
    }

    match data_client.write_predictions(predictions).await {
        Ok(_) => Ok(build_response_event(
            "datacollector".to_string(),
            vec![
                "equities".to_string(),
                "predictions".to_string(),
                "updated".to_string(),
            ],
            Some(
                json!({
                    "status": "success".to_string(),
                    "written_at": Utc::now().to_rfc3339().to_string(),
                })
                .to_string(),
            ),
        )),
        Err(e) => {
            info!("Failed to write predictions: {}", e);
            Err(e)
        }
    }
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
            .service(predictions_handler)
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

        mock_data_client
            .expect_write_predictions()
            .returning(|_| Ok(()));

        mock_data_client
            .expect_write_equities_bars()
            .returning(|_| Ok(()));

        let mut mock_trade_client: MockTradeInterfaceMock = MockTradeInterfaceMock::new();

        mock_trade_client
            .expect_get_available_tickers()
            .returning(|| Ok(vec!["AAPL".to_string()]));

        let mock_data_client: Arc<dyn DataInterface> = Arc::new(mock_data_client);

        let mock_data_client = web::Data::new(mock_data_client);

        let app = test::init_service(
            App::new()
                .app_data(mock_data_client.clone())
                .service(data_handler),
        )
        .await;

        let request = test::TestRequest::post()
            .uri("/data")
            .insert_header(ContentType::json())
            .set_json(&json!({
                "specversion": "1.0",
                "type": "equities.bars.new",
                "source": "datafetcher",
                "id": "1234",
                "time": "1997-05-25T20:00:00Z",
                "data": {
                    "equities_bars": []
                }
            }))
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }

    #[actix_web::test]
    async fn test_predictions_handler() {
        let mut mock_data_client = MockDataInterfaceMock::new();

        mock_data_client
            .expect_write_predictions()
            .returning(|_| Ok(()));

        let mock_data_client: Arc<dyn DataInterface> = Arc::new(mock_data_client);

        let mock_data_client = web::Data::new(mock_data_client);

        let app = test::init_service(
            App::new()
                .app_data(mock_data_client.clone())
                .service(predictions_handler),
        )
        .await;

        let request = test::TestRequest::post()
            .uri("/predictions")
            .insert_header(ContentType::json())
            .set_json(&json!({
                "specversion": "1.0",
                "type": "equities.predictions.generated",
                "source": "pricemodel",
                "id": "1234",
                "time": "1997-05-25T20:00:00Z",
                "data": {
                    "predictions": []
                }
            }))
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }
}
