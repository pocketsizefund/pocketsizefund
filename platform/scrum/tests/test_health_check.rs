use pretty_assertions::assert_eq;

#[tokio::test]
async fn health_check_works() {
    spawn_app();

    let client = reqwest::Client::new();

    let response = client
        .get("http://127.0.0.1:8080/health")
        .send()
        .await
        .expect("Failed to execute request.");

    assert!(response.status().is_success());
    assert_eq!(Some(0), response.content_length());
}

fn spawn_app() {
    let listener = std::net::TcpListener::bind("127.0.0.1:8080").unwrap();
    let server = scrum::run(listener).unwrap();
    let _ = tokio::spawn(server);
}
