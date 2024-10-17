use reqwest::header;
use serde_json::json;
use std::env;

#[tokio::test]
async fn test_event_handler() {
    spawn_app();

    let client = reqwest::Client::new();

    let response = client
        .post("http://127.0.0.1:8080/")
        .header(header::CONTENT_TYPE, "application/json")
        .header("ce-specversion", "1.0")
        .header("ce-id", "1")
        .header("ce-source", "http://cloudevents.io")
        .header("ce-type", "dev.knative.example")
        .body("this unit test is now our TODO list @John Forstmeier")
        .send()
        .await
        .expect("Failed to execute request.");

    assert!(response.status().is_success());

    let actual_response_payload: serde_json::Value = response.json().await.unwrap();
    let expected_response_payload =
        json!({"message": "Message successfully sent", "status": "success".to_string()});

    assert_eq!(expected_response_payload, actual_response_payload);
}

fn spawn_app() {
    env::var("DISCORD_WEBHOOK_URL").unwrap();
    let listener = std::net::TcpListener::bind("127.0.0.1:8080").unwrap();
    let server = discordbot::run(listener).unwrap();
    let _ = tokio::spawn(server);
}
