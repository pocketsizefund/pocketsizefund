use actix_web::{post, web, App, HttpResponse, HttpServer};
use pocketsizefund::data::{Client as DataClient, Interface, Bar};
use std::env;
use cloudevents::{Event, EventBuilder, EventBuilderV10};
use serde_json::json;
use uuid::Uuid;
use chrono::{DateTime, Utc};
use mockall::mock;
use std::sync::Arc;

mod tickers;

use tickers::get_dow_jones_tickers;

#[post("/health")]
async fn health_handler() -> HttpResponse {
    HttpResponse::Ok().body("OK")
}

#[post("/")]
async fn data_handler(data_client: web::Data<Arc<dyn Interface>>) -> Event {
    let old_bars = data_client.load_equities_bars().await.unwrap();

    let most_recent_datetime = old_bars.iter().max_by_key(|bar| bar.timestamp).unwrap();

    let current_datetime = chrono::Utc::now();

    let dow_jones_tickers = get_dow_jones_tickers();

    let new_bars = data_client
        .fetch_equities_bars(
            dow_jones_tickers,
            most_recent_datetime.timestamp,
            current_datetime,
        )
        .await
        .unwrap();

    data_client.write_equities_bars(new_bars).await.unwrap();

    EventBuilderV10::new()
        .id(Uuid::new_v4().to_string())
        .ty("data.equities.bars.updated")
        .source("psf.platform.datacollector")
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
        })
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let server_port_environment_variable = env::var("SERVER_PORT").unwrap();

    let server_port = server_port_environment_variable.parse::<u16>().unwrap();

    let data_client = DataClient::new(
        env::var("ALPACA_API_KEY").unwrap(),
        env::var("ALPACA_API_SECRET").unwrap(),
        env::var("AWS_ACCESS_KEY_ID").unwrap(),
        env::var("AWS_SECRET_ACCESS_KEY").unwrap(),
        env::var("S3_DATA_BUCKET_NAME").unwrap(),
    );

    let data_client = web::Data::new(data_client);

    HttpServer::new(move || App::new()
        .app_data(data_client.clone())
        .service(health_handler)
        .service(data_handler))
        .bind(("127.0.0.1", server_port))?
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
            .uri("/")
            .insert_header(ContentType::plaintext())
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert!(resp.status().is_success());
    }
}
