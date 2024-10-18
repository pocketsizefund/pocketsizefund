use actix_web::{post, web, App, HttpResponse, HttpServer};
use pocketsizefund::data::{Client as DataClient, Interface, Bar, Prediction};
use pocketsizefund::events::build_response_event;
use std::env;
use serde_json::json;
use serde::Deserialize;
use chrono::{DateTime, Utc, Duration};
use mockall::mock;
use std::sync::Arc;
use actix_web::middleware::Logger;
use log::info;
use std::io;
use std::num::ParseIntError;

mod tickers;

use tickers::get_dow_jones_tickers;


#[derive(Deserialize)]
struct Payload {
    predictions: Vec<Prediction>,
}

#[post("/health")]
async fn health_handler() -> HttpResponse {
    HttpResponse::Ok().body("OK")
}

#[post("/data")]
async fn data_handler(data_client: web::Data<Arc<dyn Interface>>) -> Result<cloudevents::Event, Box<dyn std::error::Error>> {
    let old_bars = data_client.load_equities_bars().await.unwrap_or_else(|e| {
        info!("Failed to load old bars: {}", e);
        Vec::new()
    });

    let most_recent_datetime = old_bars.iter()
        .max_by_key(|bar| bar.timestamp)
        .map(|bar| bar.timestamp)
        .unwrap_or_else(|| {
            info!("No maximum timestamp found in old bars, using fallback value.");
            Utc::now() - Duration::days(365 * 10)
        });
    
    let current_datetime = chrono::Utc::now();

    let dow_jones_tickers = get_dow_jones_tickers();

    let new_bars = data_client.fetch_equities_bars(
            dow_jones_tickers,
            most_recent_datetime,
            current_datetime,
        )
        .await.unwrap_or_else(|e| {
            info!("Failed to fetch new bars: {}", e);
            Vec::new()
        });

    match data_client.write_equities_bars(new_bars).await {
        Ok(_) => {
            info!("New bars written successfully");
            Ok(build_response_event(
                "dataprovider".to_string(),
                vec!["equities".to_string(), "bars".to_string(), "updated".to_string()],
                Some(json!({
                    "status": "success".to_string(),
                }).to_string()),
            ))
        },
        Err(e) => {
            info!("Failed to write new bars: {}", e);
            Err(e)
        }
    }
}

#[post("/predictions")]
async fn predictions_handler(
    body: web::Bytes, 
    data_client: web::Data<Arc<dyn Interface>>,
) -> HttpResponse {
    let payload: Payload = match serde_json::from_slice(&body) {
        Ok(val) => val,
        Err(_) => return HttpResponse::BadRequest().body("Invalid JSON or incorrect predictions format"),
    };

    match data_client.write_predictions(payload.predictions).await {
        Ok(_) => {
            build_response_event(
                "datacollector".to_string(), 
                vec!("equities".to_string(), "predictions".to_string(), "write".to_string()),
                Some(json!({
                    "status": "success".to_string(),
                }).to_string(),
            ));
        
            HttpResponse::Ok().body("Predictions written successfully")
        },
        Err(e) => {
            info!("Failed to write predictions: {}", e);
            HttpResponse::InternalServerError().body("Failed to write predictions")
        }
    }
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let server_port_environment_variable = env::var("SERVER_PORT")
        .unwrap_or("8080".to_string());

    let server_port = server_port_environment_variable.parse::<u16>()
        .map_err(|e: ParseIntError| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    let data_client = DataClient::new(
        env::var("ALPACA_API_KEY").expect("Alpaca API key"),
        env::var("ALPACA_API_SECRET").expect("Alpaca API secret"),
        env::var("AWS_ACCESS_KEY_ID").expect("AWS access key ID"),
        env::var("AWS_SECRET_ACCESS_KEY").expect("AWS secret access key"),
        env::var("S3_DATA_BUCKET_NAME").expect("S3 data bucket name"),
    );

    let data_client: Arc<dyn Interface> = Arc::new(data_client);

    let data_client = web::Data::new(data_client);

    HttpServer::new(move || App::new()
        .wrap(Logger::default())
        .app_data(data_client.clone())
        .service(health_handler)
        .service(data_handler)
        .service(predictions_handler))
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

#[cfg(test)]
mod tests {
    use super::*;
    use actix_web::{http::header::ContentType, test, App};
    use pocketsizefund::data::Bar;
    use chrono::{Utc, TimeZone};

    #[actix_web::test]
    async fn test_health_handler() {
        let app = test::init_service(App::new()
            .service(health_handler))
            .await;

        let req = test::TestRequest::post()
            .uri("/health")
            .insert_header(ContentType::plaintext())
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert!(resp.status().is_success());
    }

    #[actix_web::test]
    async fn test_data_handler() {
        let mut mock_client = MockInterfaceMock::new();
        
        mock_client.expect_load_equities_bars()
            .returning(|| Ok(vec![Bar {
                ticker: Some("AAPL".to_string()), 
                timestamp:  Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap(),
                open: 150.0,
                high: 152.5,
                low: 149.5,
                close: 151.5,
                volume: 1_000_000,
                number_of_trades: 5_000,
                volume_weighted_average_price: 151.2,
            }]));

        mock_client.expect_fetch_equities_bars()
            .returning(|_, _, _| Ok(vec![Bar {
                ticker: Some("AAPL".to_string()), 
                timestamp:  Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap(),
                open: 150.5,
                high: 152.3,
                low: 149.8,
                close: 151.0,
                volume: 1_200_000,
                number_of_trades: 5_000,
                volume_weighted_average_price: 150.9,
            }]));

        mock_client.expect_write_equities_bars()
            .returning(|_| Ok(()));

        env::set_var("ALPACA_API_KEY", "VALUE");
        env::set_var("ALPACA_API_SECRET", "VALUE");
        env::set_var("AWS_ACCESS_KEY_ID", "VALUE");
        env::set_var("AWS_SECRET_ACCESS_KEY", "VALUE");
        env::set_var("S3_DATA_BUCKET_NAME", "VALUE");

        let mock_client: Arc<dyn Interface> = Arc::new(mock_client);

        let mock_client = web::Data::new(mock_client);

        let app = test::init_service(App::new()
            .app_data(mock_client.clone())
            .service(data_handler))
            .await;

        let req = test::TestRequest::post()
            .uri("/data")
            .insert_header(ContentType::plaintext())
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert!(resp.status().is_success());
    }

    #[actix_web::test]
    async fn test_predictions_handler() {
        let mut mock_client = MockInterfaceMock::new();
        
        mock_client.expect_write_predictions()
            .returning(|_| Ok(()));

        let mock_client: Arc<dyn Interface> = Arc::new(mock_client);

        let mock_client = web::Data::new(mock_client);

        let app = test::init_service(App::new()
            .app_data(mock_client.clone())
            .service(predictions_handler))
            .await;

        let req = test::TestRequest::post()
            .uri("/predictions")
            .insert_header(ContentType::json())
            .set_payload(r#"{"predictions": []}"#)
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert!(resp.status().is_success());
    }
}
