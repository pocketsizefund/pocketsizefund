use std::fmt::Write;
use std::time::Duration;

use actix_service::{Service, Transform};
use actix_web::body::{BoxBody, EitherBody};
use actix_web::dev::{ServiceRequest, ServiceResponse};
use actix_web::middleware::Logger;
use actix_web::{
    error, get, post,
    web::{self, Json, ServiceConfig},
    Error, HttpResponse, Result,
};
use clerk_rs::{
    apis::users_api::User, clerk::Clerk, validators::actix::ClerkMiddleware, ClerkConfiguration,
};
use kafka::producer::{Producer, Record, RequiredAcks};
use serde::{Deserialize, Serialize};
use shuttle_actix_web::ShuttleActixWeb;
use shuttle_runtime::CustomError;
use sqlx::{Executor, FromRow, PgPool};

use pocketsizefund::health_check;

#[derive(Serialize, Deserialize, FromRow)]
struct Ticker {
    pub id: i32,
    pub edgar_id: String,
    pub cik: String,
    pub ticker: String,
    pub title: String,
}

#[derive(Clone)]
struct AppState {
    pool: PgPool,
    client: Clerk,
}

#[post("/ticker")]
async fn new_ticker(ticker: web::Json<Ticker>, state: web::Data<AppState>) -> Result<Json<Ticker>> {
    let ticker = sqlx::query_as(
        "INSERT INTO edgar (ticker, edgar_id, cik, title) VALUES ($1, $2, $3, $4) RETURNING *",
    )
    .bind(&ticker.ticker)
    .bind(&ticker.edgar_id)
    .bind(&ticker.cik)
    .bind(&ticker.title)
    .fetch_one(&state.pool)
    .await
    .map_err(|err| error::ErrorBadRequest(err.to_string()))?;

    Ok(Json(ticker))
}

#[get("/ticker/{ticker}")]
async fn get_ticker(path: web::Path<String>, state: web::Data<AppState>) -> Result<Json<Ticker>> {
    let ticker = sqlx::query_as("SELECT * FROM edgar WHERE ticker = $1")
        .bind(&*path)
        .fetch_one(&state.pool)
        .await
        .map_err(|err| error::ErrorBadRequest(err.to_string()))?;

    Ok(Json(ticker))
}

#[shuttle_runtime::main]
async fn main(
    #[shuttle_shared_db::Postgres] pool: PgPool,
    #[shuttle_runtime::Secrets] secrets: shuttle_runtime::SecretStore,
) -> ShuttleActixWeb<impl FnOnce(&mut ServiceConfig) + Send + Clone + 'static> {
    pool.execute(include_str!("./schema.sql"))
        .await
        .map_err(CustomError::new)?;

    let clerk_secret_key = secrets
        .get("CLERK_SECRET_KEY")
        .expect("Clerk Secret key is not set");
    let clerk_config = ClerkConfiguration::new(None, None, Some(clerk_secret_key), None);
    let client = Clerk::new(clerk_config.clone());

    let state = web::Data::new(AppState { pool, client });

    let config = move |cfg: &mut ServiceConfig| {
        cfg.service(
            web::scope("")
                .wrap(Logger::default())
                .wrap(ClerkMiddleware::new(clerk_config, None, true))
                .service(health_check)
                .service(get_ticker)
                .service(new_ticker)
                .app_data(state),
        );
    };

    let psf_endpoint = secrets
        .get("PSF_TOPICS_ENDPOINT")
        .expect("PSF_TOPICS_ENDPOINT is not set");

    let mut producer = Producer::from_hosts(vec![psf_endpoint.to_owned()])
        .with_ack_timeout(Duration::from_secs(1))
        .with_required_acks(RequiredAcks::One)
        .create()
        .unwrap();

    let mut buf = String::with_capacity(2);
    for i in 0..10 {
        let _ = write!(&mut buf, "{}", i); // some computation of the message data to be sent
        producer
            .send(&Record::from_value("my-topic", buf.as_bytes()))
            .unwrap();
        buf.clear();
    }

    Ok(config.into())
}
