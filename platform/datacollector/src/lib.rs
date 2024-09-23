pub mod routes;
use actix_web::dev::Server;
use actix_web::middleware::Logger;
use actix_web::{web, App, HttpServer};
use anyhow::{anyhow, Result};
use chrono::DateTime;
use chrono::Utc;
use pocketsizefund::data::{Bar, Client as DataClient, Interface};
use std::env;

use std::net::TcpListener;

pub mod tickers;

pub fn run(listener: TcpListener) -> Result<Server> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let data_client = DataClient::new(
        env::var("ALPACA_API_KEY").map_err(|e| anyhow!("Failed to get ALPACA_API_KEY: {}", e))?,
        env::var("ALPACA_API_SECRET")
            .map_err(|e| anyhow!("Failed to get ALPACA_API_SECRET: {}", e))?,
        env::var("AWS_ACCESS_KEY_ID")
            .map_err(|e| anyhow!("Failed to get AWS_ACCESS_KEY_ID: {}", e))?,
        env::var("AWS_SECRET_ACCESS_KEY")
            .map_err(|e| anyhow!("Failed to get AWS_SECRET_ACCESS_KEY: {}", e))?,
        env::var("S3_DATA_BUCKET_NAME")
            .map_err(|e| anyhow!("Failed to get S3_DATA_BUCKET_NAME: {}", e))?,
    );

    let data_client: web::Data<DataClient> = web::Data::new(data_client);

    let server = HttpServer::new(move || {
        App::new()
            .app_data(data_client.clone())
            .wrap(Logger::default())
            .service(routes::health::handler)
            .service(routes::tickers::handler)
    })
    .listen(listener)?
    .run();

    Ok(server)
}

use mockall::mock;

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
    use pocketsizefund::data::Bar;
    use std::sync::Arc;

    #[actix_web::test]
    async fn test_health_handler() {
        let app = test::init_service(App::new().service(routes::health::handler)).await;

        let req = test::TestRequest::get()
            .uri("/health")
            .insert_header(ContentType::plaintext())
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert!(resp.status().is_success());
    }

    #[actix_web::test]
    async fn test_data_handler() {
        let mut mock_client = MockInterfaceMock::new();

        mock_client.expect_load_equities_bars().returning(|| {
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

        mock_client
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

        mock_client
            .expect_write_equities_bars()
            .returning(|_| Ok(()));

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
                .service(routes::tickers::handler),
        )
        .await;

        let req = test::TestRequest::post()
            .uri("/data")
            .insert_header(ContentType::plaintext())
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert!(resp.status().is_success());
    }
}
