use async_trait::async_trait;
use aws_credential_types::Credentials;
use aws_sdk_s3::client::Client as S3Client;
use aws_sdk_s3::error::SdkError;
use aws_sdk_s3::operation::get_object::GetObjectError;
use aws_sdk_s3::primitives::{ByteStream, ByteStreamError};
use aws_sdk_s3::Config;
use aws_types::region::Region;
use chrono::{DateTime, Utc};
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use flate2::Compression;
use mockall::automock;
use serde::{Deserialize, Serialize};
use serde_json::Error as SerdeError;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};
use std::io::{Error as IOError, Read, Write};
use thiserror::Error as ThisError;

const EQUITIES_BARS_PATH: &str = "equities/bar/all.gz";

const PREDICTIONS_PATH: &str = "predictions/all.gz";

const PORTFOLIOS_PATH: &str = "portfolio/all.gz";

#[derive(ThisError, Debug)]
pub enum Error {
    #[error("IO error: {0}")]
    IOError(#[from] IOError),
    #[error("JSON error: {0}")]
    SerdeError(#[from] SerdeError),
    #[error("Get S3 object error: {0}")]
    SDKError(#[from] SdkError<GetObjectError>),
    #[error("Byte stream error: {0}")]
    ByteStreamError(#[from] ByteStreamError),
    #[error("Other error: {0}")]
    OtherError(String),
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Bar {
    pub ticker: Option<String>,
    #[serde(rename = "t")]
    pub timestamp: DateTime<Utc>,
    #[serde(rename = "o")]
    pub open_price: f32,
    #[serde(rename = "h")]
    pub high_price: f32,
    #[serde(rename = "l")]
    pub low_price: f32,
    #[serde(rename = "c")]
    pub close_price: f32,
    #[serde(rename = "v")]
    pub volume: u32,
    #[serde(rename = "n")]
    pub number_of_trades: u32,
    #[serde(rename = "vw")]
    pub volume_weighted_average_price: f32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Prediction {
    pub ticker: String,
    pub timestamp: DateTime<Utc>,
    pub timestamps: Vec<DateTime<Utc>>,
    pub upper_prices: Vec<f32>,
    pub mean_prices: Vec<f32>,
    pub lower_prices: Vec<f32>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Portfolio {
    pub timestamp: DateTime<Utc>,
    pub ticker: String,
    pub predicted_mean: f64,
    pub allocation: f64,
    pub investment_amount: f64,
}

pub enum Type {
    Bar,
    Prediction,
    Portfolio,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum Object {
    Bar(Bar),
    Prediction(Prediction),
    Portfolio(Portfolio),
}

impl Eq for Object {}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Bar(a), Object::Bar(b)) => a.ticker == b.ticker && a.timestamp == b.timestamp,
            (Object::Prediction(a), Object::Prediction(b)) => {
                a.ticker == b.ticker && a.timestamp == b.timestamp
            }
            (Object::Portfolio(a), Object::Portfolio(b)) => {
                a.ticker == b.ticker && a.timestamp == b.timestamp
            }
            _ => false,
        }
    }
}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Object::Bar(bar) => {
                bar.ticker.hash(state);
                bar.timestamp.hash(state);
            }
            Object::Prediction(prediction) => {
                prediction.ticker.hash(state);
                prediction.timestamp.hash(state);
            }
            Object::Portfolio(portfolio) => {
                portfolio.ticker.hash(state);
                portfolio.timestamp.hash(state);
            }
        }
    }
}

#[automock]
#[async_trait]
pub trait Interface: Send + Sync {
    async fn store(&self, objects: Vec<Object>) -> Result<(), Error>;
    async fn load(&self, object_type: Type) -> Result<Vec<Object>, Error>;
}

pub struct Client {
    s3_client: S3Client,
    s3_data_bucket_name: String,
}

impl Client {
    pub fn new(
        aws_access_key_id: String,
        aws_secret_access_key: String,
        s3_data_bucket_name: String,
    ) -> Self {
        let credentials = Credentials::from_keys(aws_access_key_id, aws_secret_access_key, None);

        let conf = Config::builder()
            .credentials_provider(credentials)
            .region(Region::new("us-east-1"))
            .build();

        let s3_client = S3Client::from_conf(conf);

        Client {
            s3_client,
            s3_data_bucket_name,
        }
    }

    async fn load_objects(&self, bucket: String, key: String) -> Result<Vec<Object>, Error> {
        let get_object_response = self
            .s3_client
            .get_object()
            .bucket(bucket)
            .key(key)
            .send()
            .await?;

        let compressed_data = get_object_response.body.collect().await?;

        let compressed_bytes = compressed_data.into_bytes();

        let mut decoder = GzDecoder::new(&compressed_bytes[..]);

        let mut decompressed_data = Vec::new();

        decoder.read_to_end(&mut decompressed_data)?;

        let objects = serde_json::from_slice(&decompressed_data)?;

        Ok(objects)
    }

    async fn write_objects(
        &self,
        bucket: String,
        key: String,
        objects: HashSet<Object>,
    ) -> Result<(), Error> {
        let objects_json = serde_json::to_vec(&objects)?;

        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());

        encoder.write_all(&objects_json)?;

        let compressed_data = encoder.finish()?;

        let objects_bytes = ByteStream::from(compressed_data);

        let output = self
            .s3_client
            .put_object()
            .bucket(bucket)
            .key(key)
            .body(objects_bytes)
            .send();

        let result = output.await;

        match result {
            Ok(_) => Ok(()),
            Err(e) => Err(Error::OtherError(format!("Error writing objects: {e}"))),
        }
    }
}

#[async_trait]
impl Interface for Client {
    async fn store(&self, objects: Vec<Object>) -> Result<(), Error> {
        let mut combined_objects: HashSet<Object> = HashSet::new();

        let path = match objects.get(0) {
            Some(Object::Bar(_)) => EQUITIES_BARS_PATH,
            Some(Object::Prediction(_)) => PREDICTIONS_PATH,
            Some(Object::Portfolio(_)) => PORTFOLIOS_PATH,
            _ => return Err(Error::OtherError("Invalid object type".to_string())),
        };

        let original_objects: Vec<Object> = self
            .load_objects(self.s3_data_bucket_name.clone(), path.to_string())
            .await?;

        combined_objects.extend(original_objects.clone().into_iter());

        let result = self
            .write_objects(
                self.s3_data_bucket_name.clone(),
                path.to_string(),
                combined_objects,
            )
            .await?;

        Ok(result)
    }

    async fn load(&self, object_type: Type) -> Result<Vec<Object>, Error> {
        let path = match object_type {
            Type::Bar => EQUITIES_BARS_PATH,
            Type::Prediction => PREDICTIONS_PATH,
            Type::Portfolio => PORTFOLIOS_PATH,
        };

        let original_objects = self
            .load_objects(self.s3_data_bucket_name.clone(), path.to_string())
            .await?;

        Ok(original_objects)
    }
}
