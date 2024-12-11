use std::env;

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
    env::set_var("DISCORD_WEBHOOK_URL", "http://discord/.com/test");

    let listener = std::net::TcpListener::bind("127.0.0.1:8080").unwrap();
    let server = discordbot::run(listener).unwrap();
    let _ = tokio::spawn(server);
}
