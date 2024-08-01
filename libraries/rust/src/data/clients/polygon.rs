use rustify::clients::reqwest::Client;
use rustify;

pub struct PolygonClient {
    api_key: String,
}


impl PolygonClient {
    pub fn new(api_key: String) -> Client{
        Client::default(&format!("https://api.polygon.io?apiKey={}", api_key))
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn test_polygon_client() {
        let client = PolygonClient::new("API_KEY".to_string());
    }
}