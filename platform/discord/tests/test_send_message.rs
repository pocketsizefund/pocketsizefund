use cloudevents::{Event, EventBuilder, EventBuilderV10};
use discord::routes::message::event_handler;
use reqwest::header;
use serde_json::json;

#[tokio::test]
async fn test_event_handler() {
    let port = 8123;
    spawn_app(port);

    let payload = json!({
        "specversion": "1.0",
        "id": "1234",
        "type": "dev.knative.example",
        "source": "http://cloudevents.io",
        "data": {
            "message": "this is a unit test"
    }});

    let response = reqwest::Client::new()
        .post(format!("http://127.0.0.1:{}/message", port))
        .header(header::CONTENT_TYPE, "application/cloudevents+json")
        .body(payload.to_string())
        .send()
        .await
        .expect("Failed to execute request.");

    assert!(response.status().is_success());

    let actual_response_payload: serde_json::Value = response.json().await.unwrap();
    let expected_response_payload =
        json!({"message": "Message successfully sent", "status": "success".to_string()});

    assert_eq!(expected_response_payload, actual_response_payload);
}

fn spawn_app(port: u16) {
    let listener = std::net::TcpListener::bind(format!("127.0.0.1:{}", port)).unwrap();
    let server = discord::run(listener).unwrap();
    let _ = tokio::spawn(server);
}
