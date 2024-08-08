use pocketsizefund::prelude::*;
use pocketsizefund::data::ticker::Ticker;
use pocketsizefund::data::clients::polygon::PolygonClient;
use rustify::{Client, Endpoint};
use std::env;

#[tokio::test]
async fn test_indicators() -> anyhow::Result<()> {
    // let endpoint = RelativeStrengthIndex{ticker: StockTicker("AAPL".to_string())};
    // let client = Client::default("https://api.polygon.io/v2");
    // let result = endpoint.exec(&client).await; // Sends GET request to http://api.com/test/path

    // assert!(result.is_ok());

    let api_key = env::var("POLYGON_API_KEY").unwrap();

    let ticker = Ticker::fetch("AAPL".to_string()).await?;
    // let result = client.get_stock_timeseries("AAPL".to_string()).await;
    // assert!(result.is_ok());


    Ok(())
}
