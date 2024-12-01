use actix_web::middleware::Logger;
use actix_web::{post, web, App, HttpResponse, HttpServer};
use chrono::{DateTime, Utc};
use cloudevents::{Data, Event};
use mockall::mock;
use pocketsizefund::data::{Bar, Client as DataClient, Error, Interface, Prediction};
use pocketsizefund::events::build_response_event;
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
struct Payload {
    start_at: DateTime<Utc>,
    end_at: DateTime<Utc>,
}

#[post("/data")]
async fn data_handler(
    event: web::Json<Event>,
    data_client: web::Data<Arc<dyn Interface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut filtered_bars: Vec<Bar> = Vec::new();

    if let Some(Data::Json(json)) = event.data() {
        let payload: Payload = match serde_json::from_value(json.clone()) {
            Ok(val) => val,
            Err(error) => {
                return Err(error.into());
            }
        };

        let old_bars = data_client.load_equities_bars().await.unwrap_or_else(|e| {
            tracing::error!("Failed to load old bars: {}", e);
            Vec::new()
        });

        filtered_bars = old_bars
            .into_iter()
            .filter(|bar| bar.timestamp >= payload.start_at && bar.timestamp <= payload.end_at)
            .collect();
    }

    let event = build_response_event(
        "dataprovider".to_string(),
        vec![
            "equities".to_string(),
            "bars".to_string(),
            "read".to_string(),
        ],
        Some(
            json!({
                "status": "success".to_string(),
                "data": filtered_bars,
            })
            .to_string(),
        ),
    );

    Ok(event)
}

#[post("/predictions")]
async fn predictions_handler(
    event: web::Json<Event>,
    data_client: web::Data<Arc<dyn Interface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut filtered_predictions: Vec<Prediction> = Vec::new();

    if let Some(Data::Json(json)) = event.data() {
        let payload: Payload = match serde_json::from_value(json.clone()) {
            Ok(val) => val,
            Err(error) => {
                return Err(error.into());
            }
        };

        let old_predictions = data_client.load_predictions().await.unwrap_or_else(|e| {
            tracing::error!("Failed to load old predictions: {}", e);
            Vec::new()
        });

        filtered_predictions = old_predictions
            .into_iter()
            .filter(|prediction| {
                prediction.timestamp >= payload.start_at && prediction.timestamp <= payload.end_at
            })
            .collect();
    }

    let event = build_response_event(
        "dataprovider".to_string(),
        vec![
            "equities".to_string(),
            "predictions".to_string(),
            "read".to_string(),
        ],
        Some(
            json!({
                "status": "success".to_string(),
                "data": filtered_predictions,
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

    let data_client = DataClient::new(
        env::var("ALPACA_API_KEY").expect("Alpaca API key not found"),
        env::var("ALPACA_API_SECRET").expect("Alpaca API secret not found"),
        env::var("AWS_ACCESS_KEY_ID").expect("AWS access key ID not found"),
        env::var("AWS_SECRET_ACCESS_KEY").expect("AWS secret access key not found"),
        env::var("S3_DATA_BUCKET_NAME").expect("S3 data bucket name not found"),
    );

    let data_client: Arc<dyn Interface> = Arc::new(data_client);

    let data_client = web::Data::new(data_client);

    HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .app_data(data_client.clone())
            .service(health_handler)
            .service(data_handler)
            .service(predictions_handler)
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
        async fn write_equities_bars(
            &self,
            equities_bars: Vec<Bar>,
        ) -> Result<(), Error>;
        async fn load_equities_bars(&self) -> Result<Vec<Bar>, Error>;
        async fn write_predictions(
            &self,
            predictions: Vec<Prediction>,
        ) -> Result<(), Error>;
        async fn load_predictions(&self) -> Result<Vec<Prediction>, Error>;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::{http::header::ContentType, test, App};
    use chrono::{TimeZone, Utc};
    use pocketsizefund::data::Prediction;

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
        let mut mock_client = MockInterfaceMock::new();

        let mock_output = vec![
            Bar {
                ticker: Some("AAPL".to_string()),
                timestamp: Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap(),
                open: 150.0,
                high: 152.5,
                low: 149.5,
                close: 151.5,
                volume: 1_000_000,
                number_of_trades: 5_000,
                volume_weighted_average_price: 151.2,
            },
            Bar {
                ticker: Some("AAPL".to_string()),
                timestamp: Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap(),
                open: 150.5,
                high: 152.3,
                low: 149.8,
                close: 151.0,
                volume: 1_200_000,
                number_of_trades: 5_000,
                volume_weighted_average_price: 150.9,
            },
        ];

        mock_client
            .expect_load_equities_bars()
            .returning(move || Ok(mock_output.clone()));

        env::set_var("ALPACA_API_KEY", "VALUE");
        env::set_var("ALPACA_API_SECRET", "VALUE");
        env::set_var("AWS_ACCESS_KEY_ID", "VALUE");
        env::set_var("AWS_SECRET_ACCESS_KEY", "VALUE");
        env::set_var("S3_DATA_BUCKET_NAME", "VALUE");

        let mock_client: Arc<dyn Interface> = Arc::new(mock_client);

        let mock_client = web::Data::new(mock_client);

        let app = test::init_service(
            App::new()
                .app_data(mock_client.clone())
                .service(data_handler),
        )
        .await;

        let request = test::TestRequest::post()
            .uri("/data")
            .set_json(&json!({
                "specversion": "1.0",
                "type": "equities.bars.get",
                "source": "pocketsizefund.pricemodel",
                "id": "1234",
                "time": "1997-05-25T20:00:00Z",
                "data": {
                    "start_at": "1977-05-24T00:00:00Z",
                    "end_at": "1977-05-25T00:00:00Z",
                }
            }))
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }

    #[actix_web::test]
    async fn test_predictions_handler() {
        let mut mock_client = MockInterfaceMock::new();

        let mock_output = vec![
            Prediction {
                ticker: "AAPL".to_string(),
                timestamp: Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap(),
                timestamps: vec![Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap()],
                prices: vec![150.0],
            },
            Prediction {
                ticker: "AAPL".to_string(),
                timestamp: Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap(),
                timestamps: vec![Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap()],
                prices: vec![150.5],
            },
        ];

        mock_client
            .expect_load_predictions()
            .returning(move || Ok(mock_output.clone()));

        mock_client.expect_write_predictions().returning(|_| Ok(()));

        env::set_var("ALPACA_API_KEY", "VALUE");
        env::set_var("ALPACA_API_SECRET", "VALUE");
        env::set_var("AWS_ACCESS_KEY_ID", "VALUE");
        env::set_var("AWS_SECRET_ACCESS_KEY", "VALUE");
        env::set_var("S3_DATA_BUCKET_NAME", "VALUE");

        let mock_client: Arc<dyn Interface> = Arc::new(mock_client);

        let mock_client = web::Data::new(mock_client);

        let app = test::init_service(
            App::new()
                .app_data(mock_client.clone())
                .service(predictions_handler),
        )
        .await;

        let request = test::TestRequest::post()
            .uri("/predictions")
            .set_json(&json!({
                "specversion": "1.0",
                "type": "baseline",
                "source": "positionmanager",
                "id": "1234",
                "time": "1997-05-25T20:00:00Z",
                "data": {
                    "start_at": "1977-05-24T00:00:00Z",
                    "end_at": "1977-05-25T00:00:00Z",
                }
            }))
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }
}
