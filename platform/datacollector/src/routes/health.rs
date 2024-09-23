use actix_web::{get, HttpResponse};

#[get("/health")]
pub async fn handler() -> HttpResponse {
    HttpResponse::Ok().body("OK")
}
