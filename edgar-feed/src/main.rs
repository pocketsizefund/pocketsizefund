use actix_web::{get, post, error, web::{self, Json, ServiceConfig}, Result, Responder, HttpResponse};
use actix_web::middleware::Logger;
use shuttle_actix_web::ShuttleActixWeb;
use shuttle_runtime::{CustomError};
use sqlx::{Executor, FromRow, PgPool};
use serde::{Deserialize, Serialize};



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
}


#[get("/health")]
async fn health_check() -> impl Responder {
    HttpResponse::Ok()
}

#[post("/ticker")]
async fn new_ticker(ticker: web::Json<Ticker>, state: web::Data<AppState>) -> Result<Json<Ticker>> {
    let ticker = sqlx::query_as("INSERT INTO edgar (ticker, edgar_id, cik, title) VALUES ($1, $2, $3, $4) RETURNING *")
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
    #[shuttle_shared_db::Postgres]
    pool: PgPool,
) -> ShuttleActixWeb<impl FnOnce(&mut ServiceConfig) + Send + Clone + 'static> {
    pool.execute(include_str!("./schema.sql"))
        .await
        .map_err(CustomError::new)?;

    let state = web::Data::new(AppState { pool });

    let config = move |cfg: &mut ServiceConfig| {
        cfg.service(
            web::scope("")
                .wrap(Logger::default())
                .service(health_check)
                .service(get_ticker)
                .service(new_ticker)
                .app_data(state),
        );
    };

    Ok(config.into())
}
