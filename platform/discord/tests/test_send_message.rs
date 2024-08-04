use cloudevents::EventBuilder;
use reqwest::header;
use serde_json::json;

#[tokio::test]
async fn test_event_handler() {
    let port = 8082;
    spawn_app(port);

    let client = reqwest::Client::new();

    let response = client
        .post(format!("http://127.0.0.1:{}/", port))
        .header(header::CONTENT_TYPE, "application/json")
        .header("ce-specversion", "1.0")
        .header("ce-id", "1")
        .header("ce-source", "http://cloudevents.io")
        .header("ce-type", "dev.knative.example")
        .body("this is a unit test")
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
    tokio::spawn(server);
}
