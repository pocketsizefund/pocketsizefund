use cloudevents::{Event, EventBuilder, EventBuilderV10};
use reqwest::header;
use serde_json::json;

#[tokio::test]
async fn test_buy() {
    let port = 8081;
    spawn_app(port);

    let client = reqwest::Client::new();

    let payload = json!({
    "specversion" : "1.0",
    "type" : "com.example.someevent",
    "source" : "/mycontext",
    "id" : "A234-1234-1234",
    "time" : "2018-04-05T17:31:00Z",
    "datacontenttype" : "application/json",
    "data" : {
        "message" : "Hello, World!"
    }

    });

    let response = client
        .post(format!("http://127.0.0.1:{}/buy", port))
        .header(header::CONTENT_TYPE, "application/cloudevents+json")
        .body(payload.to_string())
        .send()
        .await
        .expect("Failed to execute request.");

    assert!(response.status().is_success());
    # TODO: check the response
}

fn spawn_app(port: u16) {
    let listener = std::net::TcpListener::bind(format!("127.0.0.1:{}", port)).unwrap();
    let server = positionmanager::run(listener).unwrap();
    let _ = tokio::spawn(server);
}
