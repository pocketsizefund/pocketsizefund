use lazy_static::lazy_static;
use reqwest::header;
use serde_json::json;
use std::sync::Once;
use std::{env, result::Result};

static INIT: Once = Once::new();
const TEST_PORT: u16 = 8080;

lazy_static! {
    static ref TEST_APP: () = {
        INIT.call_once(|| {
            spawn_app(TEST_PORT);
        });
    };
}

fn initialize() {
    lazy_static::initialize(&TEST_APP);
}

#[tokio::test]
async fn test_event_handler() -> Result<(), Box<dyn std::error::Error>> {
    initialize();
    let address: String = format!("http://127.0.0.1:{}/market-status", TEST_PORT);

    let client = reqwest::Client::new();

    let response = client
        .post(&address)
        .header(header::CONTENT_TYPE, "application/cloudevents+json")
        .body(
            json!({
                "specversion": "1.0",
                "id": "1",
                "source": "psf.platform.unittest",
                "type": "dev.knative.example",
                "data": {"message": "Good morning!"}
            })
            .to_string(),
        )
        .send()
        .await?;

    println!("Response status: {:?}", response.status());
    println!("Response headers: {:?}", response.headers());

    assert!(response.status().is_success());

    let response_headers = response.headers();
    assert!(response_headers.contains_key("ce-specversion"));
    assert!(response_headers.contains_key("ce-source"));
    assert!(response_headers.contains_key("ce-type"));

    let body_text = response.text().await?;
    let body: serde_json::Value = serde_json::from_str(&body_text)?;
    println!("Response body: {:?}", body);

    assert!(Some(body.get("status")).is_some());

    Ok(())
}

#[tokio::test]
async fn test_cloud_event_response() -> Result<(), Box<dyn std::error::Error>> {
    initialize();
    let address = format!("http://127.0.0.1:{}/market-status", TEST_PORT);

    env::set_var("APCA_API_KEY_ID", "test_value");
    env::set_var("APCA_API_SECRET_KEY", "test_value");

    let client = reqwest::Client::new();

    let response = client
        .post(&address)
        .header(header::CONTENT_TYPE, "application/cloudevents+json")
        .body(
            json!({
                "specversion": "1.0",
                "type": "dev.knative.example",
                "source": "psf.platform.unittest",
                "id": "1",
                "data": {"message": "Good morning!"}
            })
            .to_string(),
        )
        .send()
        .await?;

    let status = response.status();

    if !status.is_success() {
        let error_body = response.text().await?;
        println!("Response body: {}", error_body);
        return Err(format!("Unexpected status code: {}", status).into());
    }

    let response_headers = response.headers();

    assert!(
        response_headers.contains_key("ce-specversion"),
        "ce-specversion header is missing"
    );
    assert!(
        response_headers.contains_key("ce-source"),
        "ce-source header is missing"
    );
    assert!(
        response_headers.contains_key("ce-type"),
        "ce-type header is missing"
    );

    assert_eq!(
        response_headers
            .get("ce-specversion")
            .ok_or("ce-specversion header not found")?,
        "1.0"
    );
    assert_eq!(
        response_headers
            .get("ce-source")
            .ok_or("ce-source header not found")?,
        "platform.chronos"
    );
    assert_eq!(
        response_headers
            .get("ce-type")
            .ok_or("ce-type header not found")?,
        "pocketsizefund.chronos.market.status.updated"
    );

    let body_text = response.text().await?;
    let body: serde_json::Value = serde_json::from_str(&body_text)?;

    assert!(body.get("status").is_some());
    assert!(body.get("next_close").is_some());
    assert!(body.get("next_open").is_some());

    Ok(())
}

fn spawn_app(port: u16) {
    let address = format!("127.0.0.1:{}", port);
    let listener = std::net::TcpListener::bind(address).unwrap();
    let server = chronos::run(listener).unwrap();
    let _ = tokio::spawn(server);
}
