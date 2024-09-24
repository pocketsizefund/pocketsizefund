use anyhow::Result;
use lazy_static::lazy_static;
use reqwest::header;
use serde_json::json;
use std::sync::Once;

static INIT: Once = Once::new();
const TEST_PORT: u16 = 8099;

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
async fn issues_returns_list_of_issues() -> Result<()> {
    initialize();
    let address: String = format!("http://127.0.0.1:{}/issues", TEST_PORT);

    let response = reqwest::Client::new()
        .post(&address)
        .header(header::CONTENT_TYPE, "cloudevents+json")
        .header("ce-specversion", "1.0")
        .header("ce-type", "test.integration")
        .header("ce-id", uuid::Uuid::new_v4().to_string())
        .header("ce-source", "local")
        .send()
        .await?;

    println!("Response status: {:?}", response.status());
    println!("Response headers: {:?}", response.headers());

    assert!(response.status().is_success());

    assert!(response.status().is_success());

    Ok(())
}

fn spawn_app(port: u16) {
    let address = format!("127.0.0.1:{}", port);
    let listener = std::net::TcpListener::bind(address).unwrap();
    let server = scrum::run(listener).unwrap();
    let _ = tokio::spawn(server);
}
