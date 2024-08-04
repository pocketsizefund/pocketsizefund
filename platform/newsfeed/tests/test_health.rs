#[tokio::test]
async fn health_check_works() {
    let port = 8081;
    spawn_app(port);

    let client = reqwest::Client::new();

    let response = client
        .get(format!("http://127.0.0.1:{}/health", port))
        .send()
        .await
        .expect("Failed to execute request.");

    assert!(response.status().is_success());
    assert_eq!(Some(0), response.content_length());
}

fn spawn_app(port: u16) {
    let listener = std::net::TcpListener::bind(format!("127.0.0.1:{}", port)).unwrap();
    let server = discord::run(listener).unwrap();
    tokio::spawn(server);
}
