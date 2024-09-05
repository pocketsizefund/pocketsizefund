use actix_web::web::Query;
use actix_web::{get, App, HttpRequest, HttpResponse, HttpServer, Responder};
use chrono::DateTime;
use pocketsizefund::data::client::Bar;
use pocketsizefund::data::Client as DataClient;
use std::collections::HashMap;
use std::env;

#[get("/health")]
async fn health_handler() -> impl Responder {
    HttpResponse::Ok().body("OK")
}

#[get("/data")]
async fn data_handler(request: HttpRequest) -> impl Responder {
    let request_url = request.full_url();

    let query_string = request_url.query().unwrap();

    let query_parameters = Query::<HashMap<String, String>>::from_query(query_string).unwrap();

    let start_at = DateTime::parse_from_rfc3339(query_parameters.get("start_at").unwrap()).unwrap();

    let end_at = DateTime::parse_from_rfc3339(query_parameters.get("end_at").unwrap()).unwrap();

    let data_client = DataClient::new(
        std::env::var("ALPACA_API_KEY_ID").unwrap(),
        std::env::var("ALPACA_API_SECRET_KEY").unwrap(),
        std::env::var("AWS_ACCESS_KEY_ID").unwrap(),
        std::env::var("AWS_SECRET_ACCESS_KEY").unwrap(),
        std::env::var("S3_DATA_BUCKET_NAME").unwrap(),
    );

    let old_bars = data_client.load_equities_bars().await.unwrap();

    let filtered_bars: Vec<Bar> = old_bars
        .into_iter()
        .filter(|bar| bar.timestamp >= start_at && bar.timestamp <= end_at)
        .collect();

    HttpResponse::Ok().json(filtered_bars)
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
