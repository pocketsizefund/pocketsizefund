use lazy_static::lazy_static;
use reqwest::header;
use serde_json::json;
use std::sync::Once;

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
    let address: String = format!("http://127.0.0.1:{}/market-open-check", TEST_PORT);

    let client = reqwest::Client::new();

    let response = client
        .post(&address)
        .header(header::CONTENT_TYPE, "application/cloudevents+json")
        .header("ce-specversion", "1.0")
        .header("ce-id", "1")
        .header("ce-source", "platform:chronos")
        .header("ce-type", "dev.knative.example")
        .body(json!({
            "specversion": "1.0",
            "id": "1",
            "source": "platform:chronos",
            "type": "dev.knative.example",
            "data": {"message": "Good morning!"}
        }).to_string())
        .send()
        .await?;

    println!("Response status: {:?}", response.status());
    println!("Response headers: {:?}", response.headers());

    assert!(response.status().is_success());

    let response_headers = response.headers();
    assert!(response_headers.contains_key("ce-specversion"));
    assert!(response_headers.contains_key("ce-source"));
    assert!(response_headers.contains_key("ce-type"));

    let body: serde_json::Value = response.json().await?;
    println!("Response body: {:?}", body);

    let actual_response_payload = body["data"].clone();
    let expected_response_payload = json!({
        "message": "Message successfully sent",
        "status": "success"
    });

    assert_eq!(expected_response_payload, actual_response_payload);
    Ok(())
}

#[tokio::test]
async fn test_cloud_event_response() -> Result<(), Box<dyn std::error::Error>> {
    initialize();
    let address = format!("http://127.0.0.1:{}/market-open-check", TEST_PORT);

    let client = reqwest::Client::new();

    let response = client
        .post(&address)
        .header(header::CONTENT_TYPE, "application/cloudevents+json")
        .header("ce-specversion", "1.0")
        .header("ce-id", "1")
        .header("ce-source", "platform:chronos")
        .header("ce-type", "dev.knative.example")
        .body(json!({
            "specversion": "1.0",
            "type": "dev.knative.example",
            "source": "platform:chronos",
            "id": "1",
            "data": {"message": "Good morning!"}
        }).to_string())
        .send()
        .await?;

    println!("Response status: {:?}", response.status());
    println!("Response headers: {:?}", response.headers());

    let status = response.status();
    if !status.is_success() {
        let error_body = response.text().await?;
        println!("Response body: {}", error_body);
        return Err(format!("Unexpected status code: {}", status).into());
    }

    let response_headers = response.headers();

    assert!(response_headers.contains_key("ce-specversion"), "ce-specversion header is missing");
    assert!(response_headers.contains_key("ce-source"), "ce-source header is missing");
    assert!(response_headers.contains_key("ce-type"), "ce-type header is missing");

    assert_eq!(response_headers.get("ce-specversion").ok_or("ce-specversion header not found")?, "1.0");
    assert_eq!(response_headers.get("ce-source").ok_or("ce-source header not found")?, "platform:chronos");
    assert_eq!(response_headers.get("ce-type").ok_or("ce-type header not found")?, "chronos.webhook.response");

    let body: serde_json::Value = response.json().await?;
    println!("Response body: {:?}", body);

    assert!(body.get("data").is_some(), "Response should contain a 'data' field");

    Ok(())
}

fn spawn_app(port: u16) {
    let address = format!("127.0.0.1:{}", port);
    let listener = std::net::TcpListener::bind(address).unwrap();
    let server = chronos::run(listener).unwrap();
    let _ = tokio::spawn(server);
}