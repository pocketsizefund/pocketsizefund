use apca::api::v2::account::Get;
// use apca::api::v2::asset::{Asset, Class, Status};
// use apca::api::v2::assets::{List, ListReq};
use apca::api::v2::positions::{List};
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
    let account = client.issue::<Get>(&()).await.unwrap();

    println!("{:?}", account);

    let positions = client.issue::<List>(&()).await.unwrap();

    // let request = ListReq {
    //   class: Class::Crypto,
    //   ..Default::default()
    // };
    //
    // let assets = client.issue::<List>(&request).await.unwrap();

    println!("{:?}", positions);

    // match client.issue::<orders::List>(&request).await {
    //     Ok(result) => println!("orders: {:#?}", result),
    //     Err(e) => {
    //         eprintln!("Failed to list orders: {:?}", e);
    //     }
    // };
}
