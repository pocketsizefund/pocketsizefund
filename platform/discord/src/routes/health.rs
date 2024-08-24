use actix_web::{get, post, web, HttpResponse};

#[get("/health")]
pub async fn check() -> HttpResponse {
    HttpResponse::Ok().finish()
}
