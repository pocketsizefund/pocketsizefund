use reqwest::header;
use serde_json::json;

#[tokio::test]
async fn test_nyt_article_about_boeing_777x() {
    let port = 8083;
    spawn_app(port);

    let article = "https://www.nytimes.com/2024/08/20/business/boeing-777x-grounded.html";

    let payload = json!({
        "type": "dev.knative.example",
            "source": "http://localhost",
            "id": "1",
            "specversion": "1.0",
            "data": {"source_url": article }
    });

    let response = reqwest::Client::new()
        .post(format!("http://127.0.0.1:{}/news", port))
        .header(header::CONTENT_TYPE, "application/cloudevents+json")
        .body(payload.to_string())
        .send()
        .await
        .expect("Failed to execute request.");

    assert!(response.status().is_success());

    //let response_body = response.text().await.expect("Failed to parse response.");
    //
    //assert_eq!(response_body, "hello");

    //let response_body = response["body"].clone();

    //let body: serde_json::Value = response.json().await?;
    //assert_eq!(response, "10".to_string());

    //let actual_response_payload = body["data"].clone();
    //let expected_response_payload = json!({
    //    "message": "Message successfully sent",
    //    "status": "success"
    //});
    //
    //assert_eq!(expected_response_payload, actual_response_payload);
    //Ok(())
}

fn spawn_app(port: u16) {
    let listener = std::net::TcpListener::bind(format!("127.0.0.1:{}", port)).unwrap();
    let server = newsfeed::run(listener).unwrap();
    tokio::spawn(server);
}
