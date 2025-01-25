use actix_web::middleware::Logger;
use actix_web::{post, web, App, HttpResponse, HttpServer};
use chrono::{DateTime, Utc};
use cloudevents::{Data, Event};
use mockall::mock;
use pocketsizefund::data::{
    Bar, Client as DataClient, Error as DataError, Interface as DataInterface, Object, Portfolio,
    Prediction, Type as DataType,
};
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
    data_client: web::Data<Arc<dyn DataInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut filtered_bars: Vec<Bar> = Vec::new();

    if let Some(Data::Json(json)) = event.data() {
        let payload: Payload = match serde_json::from_value(json.clone()) {
            Ok(val) => val,
            Err(error) => {
                return Err(error.into());
            }
        };

        let old_objects = data_client.load(DataType::Bar).await.unwrap_or_else(|e| {
            tracing::error!("Failed to load old bars: {}", e);
            Vec::new()
        });

        let old_bars = extract_bars(old_objects);

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

fn extract_bars(objects: Vec<Object>) -> Vec<Bar> {
    objects
        .into_iter()
        .filter_map(|obj| {
            if let Object::Bar(bar) = obj {
                Some(bar)
            } else {
                None
            }
        })
        .collect()
}

#[post("/predictions")]
async fn predictions_handler(
    event: web::Json<Event>,
    data_client: web::Data<Arc<dyn DataInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut filtered_predictions: Vec<Prediction> = Vec::new();

    if let Some(Data::Json(json)) = event.data() {
        let payload: Payload = match serde_json::from_value(json.clone()) {
            Ok(val) => val,
            Err(error) => {
                return Err(error.into());
            }
        };

        let old_objects = data_client
            .load(DataType::Prediction)
            .await
            .unwrap_or_else(|e| {
                tracing::error!("Failed to load old predictions: {}", e);
                Vec::new()
            });

        let old_predictions = extract_predictions(old_objects);

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

fn extract_predictions(objects: Vec<Object>) -> Vec<Prediction> {
    objects
        .into_iter()
        .filter_map(|obj| {
            if let Object::Prediction(prediction) = obj {
                Some(prediction)
            } else {
                None
            }
        })
        .collect()
}

#[post("/portfolio")]
async fn portfolio_handler(
    event: web::Json<Event>,
    data_client: web::Data<Arc<dyn DataInterface>>,
) -> Result<Event, Box<dyn std::error::Error>> {
    let mut filtered_portfolios: Vec<Portfolio> = Vec::new();

    if let Some(Data::Json(json)) = event.data() {
        let payload: Payload = match serde_json::from_value(json.clone()) {
            Ok(val) => val,
            Err(error) => {
                return Err(error.into());
            }
        };

        let old_objects = data_client
            .load(DataType::Portfolio)
            .await
            .unwrap_or_else(|e| {
                tracing::error!("Failed to load old portfolios: {}", e);
                Vec::new()
            });

        let old_portfolios = extract_portfolios(old_objects);

        filtered_portfolios = old_portfolios
            .into_iter()
            .filter(|portfolio| {
                portfolio.timestamp >= payload.start_at && portfolio.timestamp <= payload.end_at
            })
            .collect();
    }

    let event = build_response_event(
        "dataprovider".to_string(),
        vec![
            "equities".to_string(),
            "portfolios".to_string(),
            "read".to_string(),
        ],
        Some(
            json!({
                "status": "success".to_string(),
                "data": filtered_portfolios,
            })
            .to_string(),
        ),
    );

    Ok(event)
}

fn extract_portfolios(objects: Vec<Object>) -> Vec<Portfolio> {
    objects
        .into_iter()
        .filter_map(|obj| {
            if let Object::Portfolio(portfolio) = obj {
                Some(portfolio)
            } else {
                None
            }
        })
        .collect()
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init_from_env(env_logger::Env::default().default_filter_or("info"));

    let server_port_environment_variable = env::var("SERVER_PORT").unwrap_or("8080".to_string());

    let server_port = server_port_environment_variable
        .parse::<u16>()
        .map_err(|e: ParseIntError| io::Error::new(io::ErrorKind::InvalidInput, e))?;

    let data_client = DataClient::new(
        env::var("AWS_ACCESS_KEY_ID").expect("AWS access key ID not found"),
        env::var("AWS_SECRET_ACCESS_KEY").expect("AWS secret access key not found"),
        env::var("S3_DATA_BUCKET_NAME").expect("S3 data bucket name not found"),
    );

    let data_client: Arc<dyn DataInterface> = Arc::new(data_client);

    let data_client = web::Data::new(data_client);

    let new_client = DataClient::new(
        env::var("AWS_ACCESS_KEY_ID").expect("AWS access key ID not found"),
        env::var("AWS_SECRET_ACCESS_KEY").expect("AWS secret access key not found"),
        env::var("S3_DATA_BUCKET_NAME").expect("S3 data bucket name not found"),
    );

    let new_client: Arc<dyn DataInterface> = Arc::new(new_client);

    let new_client = web::Data::new(new_client);

    HttpServer::new(move || {
        App::new()
            .wrap(Logger::default())
            .app_data(data_client.clone())
            .app_data(new_client.clone())
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
        async fn store(&self, objects: Vec<Object>) -> Result<(), DataError>;
        async fn load(&self, object_type: DataType) -> Result<Vec<Object>, DataError>;
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
        let mut mock_data_client: MockDataInterfaceMock = MockDataInterfaceMock::new();

        let mock_output = vec![
            Object::Bar(Bar {
                ticker: Some("AAPL".to_string()),
                timestamp: Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap(),
                open: 150.0,
                high: 152.5,
                low: 149.5,
                close: 151.5,
                volume: 1_000_000,
                number_of_trades: 5_000,
                volume_weighted_average_price: 151.2,
            }),
            Object::Bar(Bar {
                ticker: Some("AAPL".to_string()),
                timestamp: Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap(),
                open: 150.5,
                high: 152.3,
                low: 149.8,
                close: 151.0,
                volume: 1_200_000,
                number_of_trades: 5_000,
                volume_weighted_average_price: 150.9,
            }),
        ];

        mock_data_client
            .expect_load()
            .returning(move |_| Ok(mock_output.clone()));

        env::set_var("ALPACA_API_KEY", "VALUE");
        env::set_var("ALPACA_API_SECRET", "VALUE");
        env::set_var("AWS_ACCESS_KEY_ID", "VALUE");
        env::set_var("AWS_SECRET_ACCESS_KEY", "VALUE");
        env::set_var("S3_DATA_BUCKET_NAME", "VALUE");

        let mock_client: Arc<dyn DataInterface> = Arc::new(mock_data_client);

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
        let mut mock_client = MockDataInterfaceMock::new();

        let mock_output = vec![
            Object::Prediction(Prediction {
                ticker: "AAPL".to_string(),
                timestamp: Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap(),
                timestamps: vec![Utc.with_ymd_and_hms(1977, 5, 25, 0, 0, 0).unwrap()],
                prices: vec![150.0],
            }),
            Object::Prediction(Prediction {
                ticker: "AAPL".to_string(),
                timestamp: Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap(),
                timestamps: vec![Utc.with_ymd_and_hms(1977, 5, 26, 0, 0, 0).unwrap()],
                prices: vec![150.5],
            }),
        ];

        mock_client
            .expect_load()
            .returning(move |_| Ok(mock_output.clone()));

        mock_client.expect_store().returning(|_| Ok(()));

        env::set_var("ALPACA_API_KEY", "VALUE");
        env::set_var("ALPACA_API_SECRET", "VALUE");
        env::set_var("AWS_ACCESS_KEY_ID", "VALUE");
        env::set_var("AWS_SECRET_ACCESS_KEY", "VALUE");
        env::set_var("S3_DATA_BUCKET_NAME", "VALUE");

        let mock_client: Arc<dyn DataInterface> = Arc::new(mock_client);

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
