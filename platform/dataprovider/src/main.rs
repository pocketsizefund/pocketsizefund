use actix_web::middleware::Logger;
use actix_web::{post, web, App, HttpResponse, HttpServer};
use chrono::{DateTime, Utc};
use cloudevents::{Event, EventBuilder, EventBuilderV10};
use dataprovider::{Bar, Client as DataClient, Interface};
use mockall::mock;
use serde::Deserialize;
use serde_json::json;
use std::env;
use std::io;
use std::num::ParseIntError;
use std::sync::Arc;
use uuid::Uuid;

#[post("/health")]
async fn health_handler() -> HttpResponse {
    HttpResponse::Ok().body("OK")
}

#[derive(Deserialize, Debug)]
struct Data {
    start_at: DateTime<Utc>,
    end_at: DateTime<Utc>,
}

#[derive(Deserialize, Debug)]
struct Payload {
    data: Data,
}

#[post("/data")]
async fn data_handler(
    payload: web::Json<Payload>,
    data_client: web::Data<Arc<dyn Interface>>,
) -> HttpResponse {
    tracing::info!("payload: {:?}", payload);
    // let payload: Payload = match serde_json::from_slice(&body) {
    //     Ok(val) => val,
    //     Err(_) => {
    //         tracing::error!("invalid JSON or incorrect date format");
    //         return HttpResponse::BadRequest().body("Invalid JSON or incorrect date format");
    //     }
    // };

    let old_bars = data_client.load_equities_bars().await.unwrap_or_else(|e| {
        tracing::error!("Failed to load old bars: {}", e);
        Vec::new()
    });

    let filtered_bars: Vec<Bar> = old_bars
        .into_iter()
        .filter(|bar| {
            bar.timestamp >= payload.data.start_at && bar.timestamp <= payload.data.end_at
        })
        .collect();

    EventBuilderV10::new()
        .id(Uuid::new_v4().to_string())
        .ty("data.equities.bars.read")
        .source("psf.platform.dataprovider")
        .data(
            "application/cloudevents+json",
            json!({
                "status": "success".to_string(),
            }),
        )
        .extension("timestamp", Utc::now().to_rfc3339().to_string())
        .build()
        .unwrap_or_else(|e| {
            tracing::error!("Failed to build event: {}", e);
            Event::default()
        });

    HttpResponse::Ok().json(filtered_bars)
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
        ) -> Result<Vec<Bar>, Box<dyn std::error::Error>>;
        async fn write_equities_bars(
            &self,
            equities_bars: Vec<Bar>,
        ) -> Result<(), Box<dyn std::error::Error>>;
        async fn load_equities_bars(&self) -> Result<Vec<Bar>, Box<dyn std::error::Error>>;
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

        let payload = json!({
            "data": {
                "start_at": "1977-05-24T00:00:00Z",
                "end_at": "1977-05-25T00:00:00Z",
            }
        });

        let req = test::TestRequest::post()
            .uri("/data")
            .set_json(&payload)
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert!(resp.status().is_success());
    }
}
