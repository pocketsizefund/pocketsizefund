use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
};

pub async fn check() -> Response {
    (StatusCode::OK).into_response()
}
