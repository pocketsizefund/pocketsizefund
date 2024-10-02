use actix_web::{get, HttpResponse};

#[get("/health")]
pub async fn check() -> HttpResponse {
    HttpResponse::Ok().finish()
}
