use lazy_static::lazy_static;
use std::sync::Once;

static INIT: Once = Once::new();
const TEST_PORT: u16 = 8081;

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
async fn issues_returns_list_of_issues() {
    initialize();
    let address: String = format!("http://127.0.0.1:{}/issues", TEST_PORT);

    let response = reqwest::Client::new()
        .post(address)
        .send()
        .await
        .expect("Failed to execute request.");

    assert!(response.status().is_success());
    assert_eq!(Some(0), response.content_length());
}

fn spawn_app(port: u16) {
    let address = format!("127.0.0.1:{}", port);
    let listener = std::net::TcpListener::bind(address).unwrap();
    let server = scrum::run(listener).unwrap();
    let _ = tokio::spawn(server);
}
