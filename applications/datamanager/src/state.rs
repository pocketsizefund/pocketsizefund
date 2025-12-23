use aws_sdk_s3::Client as S3Client;
use reqwest::Client as HTTPClient;

#[derive(Clone)]
pub struct PolygonSecrets {
    pub base: String,
    pub key: String,
}

#[derive(Clone)]
pub struct State {
    pub http_client: HTTPClient,
    pub polygon: PolygonSecrets,
    pub s3_client: S3Client,
    pub bucket_name: String,
}

impl State {
    pub async fn from_env() -> Self {
        let http_client = HTTPClient::builder()
            .timeout(std::time::Duration::from_secs(10))
            .build()
            .unwrap();

        let config = aws_config::load_defaults(aws_config::BehaviorVersion::latest()).await;
        let s3_client = S3Client::new(&config);
        let bucket_name =
            std::env::var("AWS_S3_DATA_BUCKET_NAME").unwrap_or("pocketsizefund-data".to_string());

        Self {
            http_client,
            polygon: PolygonSecrets {
                base: std::env::var("POLYGON_BASE_URL")
                    .unwrap_or("https://api.polygon.io".to_string()),
                key: std::env::var("POLYGON_API_KEY")
                    .expect("POLYGON_API_KEY must be set in environment"),
            },
            s3_client,
            bucket_name,
        }
    }
}
