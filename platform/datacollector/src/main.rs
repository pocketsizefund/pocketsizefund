use actix_web::{get, App, HttpResponse, HttpServer, Responder};
use pocketsizefund::data::Client as DataClient;
use std::env;

mod tickers;

use tickers::get_dow_jones_tickers;

#[get("/health")]
async fn health_handler() -> impl Responder {
    HttpResponse::Ok().body("OK")
}

#[get("/data")]
async fn data_handler() -> impl Responder {
    let data_client = DataClient::new(
        std::env::var("ALPACA_API_KEY_ID").unwrap(),
        std::env::var("ALPACA_API_SECRET_KEY").unwrap(),
        std::env::var("AWS_ACCESS_KEY_ID").unwrap(),
        std::env::var("AWS_SECRET_ACCESS_KEY").unwrap(),
        std::env::var("S3_DATA_BUCKET_NAME").unwrap(),
    );

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

    HttpResponse::Ok().body("OK")
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let server_port_environment_variable = env::var("SERVER_PORT").unwrap();

    let server_port = server_port_environment_variable.parse::<u16>().unwrap();

    HttpServer::new(|| App::new().service(health_handler).service(data_handler))
        .bind(("127.0.0.1", server_port))?
        .run()
        .await
}

#[cfg(test)]
mod tests {
    use actix_web::{http::header::ContentType, test, App};

    use super::*;

    #[actix_web::test]
    async fn test_health_handler() {
        let app = test::init_service(App::new().service(health_handler)).await;

        let req = test::TestRequest::default()
            .uri("/health")
            .insert_header(ContentType::plaintext())
            .to_request();

        let resp = test::call_service(&app, req).await;

        assert!(resp.status().is_success());
    }
}
