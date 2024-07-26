use apca::api::v2::orders;
use apca::{ApiInfo, Client};

#[tokio::main]
async fn main() {
    let api_info = match ApiInfo::from_env() {
        Ok(api_info) => api_info,
        Err(e) => {
            panic!("Failed to get API info: {:?}", e);
        }
    };
    let client = Client::new(api_info);

    let request = orders::ListReq {
        status: orders::Status::Closed,
        limit: Some(100),
        _non_exhaustive: (),
        symbols: vec!["JNJ".to_string()],
        nested: false,
    };

    match client.issue::<orders::List>(&request).await {
        Ok(result) => println!("orders: {:#?}", result),
        Err(e) => {
            eprintln!("Failed to list orders: {:?}", e);
        }
    };
}
