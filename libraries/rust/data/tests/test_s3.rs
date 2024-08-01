use aws_config::{BehaviorVersion, Region};
use aws_sdk_s3 as s3;
use data::client::Client;
use testcontainers_modules::{
    localstack::LocalStack,
    testcontainers::{runners::AsyncRunner, ImageExt},
};
use chrono::{DateTime, Utc};
use data::client::Bar;

#[tokio::test]
async fn test_s3_client() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let _ = pretty_env_logger::try_init();

    let request = LocalStack::default().with_env_var("SERVICES", "s3");
    let container = request.start().await?;

    let host_ip = container.get_host().await?;
    let host_port = container.get_host_port_ipv4(4566).await?;
    // Set up AWS client
    let endpoint_url = format!("http://{host_ip}:{host_port}");
    let creds = s3::config::Credentials::new("fake", "fake", None, None, "test");

    let config = aws_sdk_s3::config::Builder::default()
        .behavior_version(BehaviorVersion::v2024_03_28())
        .region(Region::new("us-east-1"))
        .credentials_provider(creds)
        .endpoint_url(endpoint_url)
        .force_path_style(true)
        .build();

    println!("config: {:#?}", config);

    let client: Client = Client::new(
        "fake".to_string(),
        "fake".to_string(),
        "fake".to_string(),
        "fake".to_string(),
        "fake".to_string(),
    );


    let test_bar = Bar {
        ticker: Some("AAPL".to_string()),
        timestamp: Utc::now(),
        open: 150.0,
        high: 155.0,
        low: 149.0,
        close: 153.0,
        volume: 1000000,
        number_of_trades: 5000,
        volume_weighted_average_price: 152.5,
    };

    let test_bar = vec![test_bar];


    client.write_equities_bars_to_storage(test_bar).await?;

    Ok(())
}
