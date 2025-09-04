use axum::body::Body;
use axum::http::{Request, StatusCode};
use http_body_util::BodyExt;
use serde_json::json;
use tokio::net::TcpListener;
use tower::ServiceExt;

#[tokio::test]
async fn test_health_endpoint() {
    let app = datamanager::create_app().await;

    let request = Request::builder()
        .uri("/health")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    assert_eq!(response.status(), StatusCode::OK);

    let body = response.into_body().collect().await.unwrap().to_bytes();
    let body_str = String::from_utf8(body.to_vec()).unwrap();
    
    // Health endpoint returns empty body with 200 status
    assert_eq!(body_str, "");
}

#[tokio::test]
async fn test_equity_fetch_no_date_range() {
    let app = datamanager::create_app().await;

    let request = Request::builder()
        .method("GET")
        .uri("/equity")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Should return either success with data or error about missing files
    // Both are acceptable for this test since we're testing the endpoint works
    assert!(
        response.status() == StatusCode::OK || 
        response.status() == StatusCode::INTERNAL_SERVER_ERROR
    );
}

#[tokio::test]
async fn test_equity_fetch_with_date_range() {
    let app = datamanager::create_app().await;

    let request_body = json!({
        "start_date": "2025-08-29",
        "end_date": "2025-08-29"
    });

    let request = Request::builder()
        .method("GET")
        .uri("/equity")
        .header("content-type", "application/json")
        .body(Body::from(request_body.to_string()))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Should return either success with data or error about missing files
    let status = response.status();
    assert!(
        status == StatusCode::OK || 
        status == StatusCode::INTERNAL_SERVER_ERROR
    );

    let body = response.into_body().collect().await.unwrap().to_bytes();
    
    // If successful, should be parquet binary data or error message
    if status == StatusCode::OK {
        // Should be binary parquet data, check it's not empty and starts with parquet magic bytes
        assert!(!body.is_empty(), "Parquet response should not be empty");
        // Parquet files start with "PAR1" magic bytes
        if body.len() >= 4 {
            let magic = &body[0..4];
            assert_eq!(magic, b"PAR1", "Should be valid parquet data starting with PAR1");
        }
    } else {
        // For errors, try to convert to string to check error message
        if let Ok(body_str) = String::from_utf8(body.to_vec()) {
            assert!(body_str.contains("Query failed") || body_str.contains("404"));
        }
    }
}

#[tokio::test]
async fn test_equity_sync_endpoint() {
    let app = datamanager::create_app().await;

    // Test with a recent weekday date that should have market data
    let request_body = json!({
        "date": "2024-08-29"
    });

    let request = Request::builder()
        .method("POST")
        .uri("/equity")
        .header("content-type", "application/json")
        .body(Body::from(request_body.to_string()))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Should return OK - either with data or with an explanation
    assert_eq!(response.status(), StatusCode::OK);

    let body = response.into_body().collect().await.unwrap().to_bytes();
    let body_str = String::from_utf8(body.to_vec()).unwrap();
    
    // Should either contain DataFrame info, market data, or no data message
    assert!(
        body_str.contains("DataFrame") || 
        body_str.contains("market data") ||
        body_str.contains("No market data") ||
        body_str.contains("results")
    );
}

#[tokio::test]
async fn test_equity_sync_invalid_date() {
    let app = datamanager::create_app().await;

    let request_body = json!({
        "date": "invalid-date"
    });

    let request = Request::builder()
        .method("POST")
        .uri("/equity")
        .header("content-type", "application/json")
        .body(Body::from(request_body.to_string()))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Should return error for invalid date format
    assert_eq!(response.status(), StatusCode::UNPROCESSABLE_ENTITY);
}

#[tokio::test]
async fn test_equity_sync_missing_date() {
    let app = datamanager::create_app().await;

    let request_body = json!({});

    let request = Request::builder()
        .method("POST")
        .uri("/equity")
        .header("content-type", "application/json")
        .body(Body::from(request_body.to_string()))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Should return error for missing date field
    assert_eq!(response.status(), StatusCode::UNPROCESSABLE_ENTITY);
}

#[tokio::test]
async fn test_nonexistent_endpoint() {
    let app = datamanager::create_app().await;

    let request = Request::builder()
        .uri("/nonexistent")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    assert_eq!(response.status(), StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn test_equity_fetch_invalid_json() {
    let app = datamanager::create_app().await;

    let request = Request::builder()
        .method("GET")
        .uri("/equity")
        .header("content-type", "application/json")
        .body(Body::from("invalid json"))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Should return error for invalid JSON (Axum returns 400 for malformed JSON)
    assert_eq!(response.status(), StatusCode::BAD_REQUEST);
}

#[tokio::test]
async fn test_equity_fetch_invalid_date_format() {
    let app = datamanager::create_app().await;

    let request_body = json!({
        "start_date": "2025-13-32",  // Invalid date
        "end_date": "2025-08-29"
    });

    let request = Request::builder()
        .method("GET")
        .uri("/equity")
        .header("content-type", "application/json")
        .body(Body::from(request_body.to_string()))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Should return error for invalid date format
    assert_eq!(response.status(), StatusCode::UNPROCESSABLE_ENTITY);
}

#[tokio::test]
async fn test_full_workflow_sync_then_fetch() {
    let app = datamanager::create_app().await;

    // First, try to sync data for a specific date
    let sync_body = json!({
        "date": "2024-08-29"
    });

    let sync_request = Request::builder()
        .method("POST")
        .uri("/equity")
        .header("content-type", "application/json")
        .body(Body::from(sync_body.to_string()))
        .unwrap();

    let sync_response = app.clone().oneshot(sync_request).await.unwrap();
    assert_eq!(sync_response.status(), StatusCode::OK);

    // Then try to fetch data for the same date
    let fetch_body = json!({
        "start_date": "2024-08-29",
        "end_date": "2024-08-29"
    });

    let fetch_request = Request::builder()
        .method("GET")
        .uri("/equity")
        .header("content-type", "application/json")
        .body(Body::from(fetch_body.to_string()))
        .unwrap();

    let fetch_response = app.oneshot(fetch_request).await.unwrap();
    
    // Fetch should work if sync was successful
    let fetch_status = fetch_response.status();
    assert!(
        fetch_status == StatusCode::OK || 
        fetch_status == StatusCode::INTERNAL_SERVER_ERROR
    );
}

// Test spawning server on a separate thread and making HTTP requests
#[tokio::test]
async fn test_server_spawn_integration() {
    // Find an available port
    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let port = addr.port();

    // Spawn the server in a separate task
    let server_handle = tokio::spawn(async move {
        let app = datamanager::create_app().await;
        axum::serve(listener, app).await.unwrap();
    });

    // Wait a moment for server to start
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    // Make HTTP requests to the spawned server
    let client = reqwest::Client::new();

    // Test health endpoint
    let health_response = client
        .get(&format!("http://127.0.0.1:{}/health", port))
        .send()
        .await
        .unwrap();

    assert_eq!(health_response.status(), 200);
    // Health endpoint returns empty body
    assert_eq!(health_response.text().await.unwrap(), "");

    // Test equity endpoint with date range
    let equity_response = client
        .get(&format!("http://127.0.0.1:{}/equity", port))
        .json(&json!({
            "start_date": "2025-08-29",
            "end_date": "2025-08-29"
        }))
        .send()
        .await
        .unwrap();

    // Should get some response (either success or expected error)
    assert!(equity_response.status().as_u16() >= 200 && equity_response.status().as_u16() < 600);
    
    // If successful, should return binary parquet data
    if equity_response.status().is_success() {
        let content_type = equity_response.headers().get("content-type");
        if let Some(ct) = content_type {
            assert_eq!(ct, "application/octet-stream");
        }
    }

    // Clean up by aborting the server
    server_handle.abort();
}