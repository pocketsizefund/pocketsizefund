use apca::api::v2::orders;
use apca::{ApiInfo, Client};

pub struct AlpacaConfig {
    pub client: Client,
}

impl Default for AlpacaConfig {
    fn default() -> Self {
        let api_info = match ApiInfo::from_env() {
            Ok(api_info) => api_info,
            Err(e) => {
                panic!("Failed to get API info: {:?}", e);
            }
        };
        let client = Client::new(api_info);

        Self { client }
    }
}
