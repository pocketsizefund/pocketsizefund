use actix_web::{get, HttpResponse};

#[get("/health")]
async fn check() -> HttpResponse {
    HttpResponse::Ok().finish()
}