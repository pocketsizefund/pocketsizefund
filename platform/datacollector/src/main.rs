use actix_web::middleware::Logger;
use actix_web::{post, web, App, HttpResponse, HttpServer};
use chrono::Utc;
use cloudevents::{Data, Event};
use log::info;
use mockall::mock;
use pocketsizefund::data::{
    Client as DataClient, Error as DataError, Interface as DataInterface, Object, Type as DataType,
};
use pocketsizefund::events::build_response_event;
use serde::Deserialize;
use serde_json::json;
use std::io;
use std::num::ParseIntError;
use std::sync::Arc;
use std::{env, vec};

#[post("/health")]
async fn health_handler() -> HttpResponse {
    HttpResponse::Ok().body("OK")
}

#[derive(Deserialize)]
struct Payload {
    bars: Option<Vec<Object>>,
    predictions: Option<Vec<Object>>,
    portfolio: Option<Object>,
}

#[post("/data")]
async fn data_handler(
    event: web::Json<Event>,
    data_client: web::Data<Arc<dyn DataInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    store_payload(DataType::Bar, event, data_client).await
}

#[post("/predictions")]
async fn predictions_handler(
    event: web::Json<Event>,
    data_client: web::Data<Arc<dyn DataInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    store_payload(DataType::Prediction, event, data_client).await
}

#[post("/portfolio")]
async fn portfolio_handler(
    event: web::Json<Event>,
    data_client: web::Data<Arc<dyn DataInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    store_payload(DataType::Portfolio, event, data_client).await
}

async fn store_payload(
    object_type: DataType,
    event: web::Json<Event>,
    data_client: web::Data<Arc<dyn DataInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut objects: Vec<Object> = Vec::new();

    if let Some(Data::Json(json)) = event.data() {
        match serde_json::from_value::<Payload>(json.clone()) {
            Ok(payload) => match object_type {
                DataType::Bar => objects = payload.bars.unwrap_or(vec![]),
                DataType::Prediction => objects = payload.predictions.unwrap_or(vec![]),
                DataType::Portfolio => {
                    objects = {
                        let mut portfolio = vec![];
                        if let Some(portfolio_object) = payload.portfolio {
                            portfolio.push(portfolio_object);
                        }
                        portfolio
                    }
                }
            },
            Err(error) => {
                return Err(error.into());
            }
        };
    }

    let type_name = match object_type {
        DataType::Bar => "bars",
        DataType::Prediction => "predictions",
        DataType::Portfolio => "portfolio",
    };

    match data_client.store(objects).await {
        Ok(_) => Ok(build_response_event(
            "datacollector".to_string(),
            vec![
                "equities".to_string(),
                type_name.to_string(),
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
            info!("Failed to write {}: {}", type_name, e);
            Err(Box::new(e))
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
        env::var("AWS_ACCESS_KEY_ID").expect("AWS access key ID"),
        env::var("AWS_SECRET_ACCESS_KEY").expect("AWS secret access key"),
        env::var("S3_DATA_BUCKET_NAME").expect("S3 data bucket name"),
    );

    let data_client: Arc<dyn DataInterface> = Arc::new(data_client);

    let data_client = web::Data::new(data_client);

    HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .app_data(data_client.clone())
            .service(health_handler)
            .service(data_handler)
            .service(predictions_handler)
            .service(portfolio_handler)
    })
    .bind(("0.0.0.0", server_port))?
    .run()
    .await
}

mock! {
    pub DataInterfaceMock {}

    #[async_trait::async_trait]
    impl DataInterface for DataInterfaceMock {
        async fn store(&self, objects: Vec<Object>) -> Result<(), DataError>;
        async fn load(&self, object_type: DataType) -> Result<Vec<Object>, DataError>;
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

        mock_data_client.expect_store().returning(|_| Ok(()));

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
                    "bars": []
                }
            }))
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }

    #[actix_web::test]
    async fn test_predictions_handler() {
        let mut mock_data_client = MockDataInterfaceMock::new();

        mock_data_client.expect_store().returning(|_| Ok(()));

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

    #[actix_web::test]
    async fn test_portfolios_handler() {
        let mut mock_data_client = MockDataInterfaceMock::new();

        mock_data_client.expect_store().returning(|_| Ok(()));

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
                "type": "equities.portfolio.updated",
                "source": "positionmanager",
                "id": "1234",
                "time": "1997-05-25T20:00:00Z",
                "data": {
                    "portfolios": []
                }
            }))
            .to_request();

        let response = test::call_service(&app, request).await;

        assert!(response.status().is_success());
    }
}
