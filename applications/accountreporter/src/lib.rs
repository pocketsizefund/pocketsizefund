use chrono::{DateTime, Utc};
use rdkafka::config::ClientConfig;
use rdkafka::producer::{FutureProducer, FutureRecord};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::time::Duration;
use thiserror::Error;
use tokio::time;
use tracing::{error, info};

#[derive(Error, Debug)]
pub enum AccountManagerError {
    #[error("HTTP request failed: {0}")]
    HttpError(#[from] reqwest::Error),
    #[error("Kafka error: {0}")]
    KafkaError(#[from] rdkafka::error::KafkaError),
    #[error("JSON parsing error: {0}")]
    JsonError(#[from] serde_json::Error),
    #[error("Environment variable error: {0}")]
    EnvError(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AccountSnapshot {
    pub timestamp: DateTime<Utc>,
    pub account_id: String,
    pub equity: f64,
    pub cash: f64,
    pub buying_power: f64,
    pub portfolio_value: f64,
    pub day_trade_count: i32,
    pub pattern_day_trader: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Position {
    pub symbol: String,
    pub qty: f64,
    pub market_value: f64,
    pub cost_basis: f64,
    pub unrealized_pl: f64,
    pub unrealized_plpc: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AccountEvent {
    pub event_type: String,
    pub timestamp: DateTime<Utc>,
    pub account_snapshot: AccountSnapshot,
    pub positions: Vec<Position>,
}

#[derive(Clone)]
pub struct AlpacaClient {
    pub client: Client,
    pub base_url: String,
    pub api_key: String,
    pub api_secret: String,
}

impl AlpacaClient {
    pub fn new() -> Result<Self, AccountManagerError> {
        let client = Client::builder()
            .timeout(Duration::from_secs(30))
            .build()?;

        // Try to read from Docker secrets first, fallback to environment variables
        let base_url = Self::read_secret_or_env("ALPACA_BASE_URL", "https://paper-api.alpaca.markets")?;
        let api_key = Self::read_secret_or_env("ALPACA_API_KEY", "")?;
        let api_secret = Self::read_secret_or_env("ALPACA_API_SECRET", "")?;

        if api_key.is_empty() {
            return Err(AccountManagerError::EnvError("ALPACA_API_KEY must be set".to_string()));
        }
        if api_secret.is_empty() {
            return Err(AccountManagerError::EnvError("ALPACA_API_SECRET must be set".to_string()));
        }

        Ok(Self {
            client,
            base_url,
            api_key,
            api_secret,
        })
    }

    fn read_secret_or_env(name: &str, default: &str) -> Result<String, AccountManagerError> {
        // Try reading from Docker secret file first (mounted at /run/secrets/{name})
        let secret_path = format!("/run/secrets/{}", name);
        if let Ok(content) = std::fs::read_to_string(&secret_path) {
            return Ok(content.trim().to_string());
        }

        // Fallback to environment variable
        match std::env::var(name) {
            Ok(value) => Ok(value),
            Err(_) => {
                if default.is_empty() {
                    Err(AccountManagerError::EnvError(format!("{} must be set", name)))
                } else {
                    Ok(default.to_string())
                }
            }
        }
    }

    pub async fn get_account(&self) -> Result<serde_json::Value, AccountManagerError> {
        let response = self.client
            .get(&format!("{}/v2/account", self.base_url))
            .header("APCA-API-KEY-ID", &self.api_key)
            .header("APCA-API-SECRET-KEY", &self.api_secret)
            .send()
            .await?;

        let account_data: serde_json::Value = response.json().await?;
        Ok(account_data)
    }

    pub async fn get_positions(&self) -> Result<Vec<serde_json::Value>, AccountManagerError> {
        let response = self.client
            .get(&format!("{}/v2/positions", self.base_url))
            .header("APCA-API-KEY-ID", &self.api_key)
            .header("APCA-API-SECRET-KEY", &self.api_secret)
            .send()
            .await?;

        let positions: Vec<serde_json::Value> = response.json().await?;
        Ok(positions)
    }
}

pub struct EventPublisher {
    producer: FutureProducer,
    topic: String,
}

impl EventPublisher {
    pub fn new() -> Result<Self, AccountManagerError> {
        let kafka_brokers = std::env::var("KAFKA_BROKERS")
            .unwrap_or("localhost:9092".to_string());
        let topic = std::env::var("KAFKA_TOPIC")
            .unwrap_or("account-events".to_string());

        let producer: FutureProducer = ClientConfig::new()
            .set("bootstrap.servers", kafka_brokers)
            .set("message.timeout.ms", "5000")
            .create()?;

        Ok(Self { producer, topic })
    }

    pub async fn publish_account_event(&self, event: &AccountEvent) -> Result<(), AccountManagerError> {
        let payload = serde_json::to_string(event)?;
        let key = format!("account_{}", event.account_snapshot.account_id);

        let record = FutureRecord::to(&self.topic)
            .key(&key)
            .payload(&payload);

        match self.producer.send(record, Duration::from_secs(5)).await {
            Ok(_) => {
                info!("Published account event for account {}", event.account_snapshot.account_id);
                Ok(())
            },
            Err((e, _)) => Err(AccountManagerError::KafkaError(e)),
        }
    }
}

pub async fn run_account_monitor() -> Result<(), AccountManagerError> {
    let alpaca_client = AlpacaClient::new()?;
    let event_publisher = EventPublisher::new()?;

    let poll_interval = std::env::var("POLL_INTERVAL_SECONDS")
        .unwrap_or("1".to_string())
        .parse::<u64>()
        .unwrap_or(1);

    let mut interval = time::interval(Duration::from_secs(poll_interval));

    info!("Starting account monitor with {}s interval", poll_interval);

    loop {
        interval.tick().await;

        match poll_and_publish(&alpaca_client, &event_publisher).await {
            Ok(_) => {},
            Err(e) => {
                error!("Error polling account: {}", e);
            }
        }
    }
}

async fn poll_and_publish(
    alpaca_client: &AlpacaClient,
    event_publisher: &EventPublisher,
) -> Result<(), AccountManagerError> {
    let account_data = alpaca_client.get_account().await?;
    let positions_data = alpaca_client.get_positions().await?;

    let account_snapshot = AccountSnapshot {
        timestamp: Utc::now(),
        account_id: account_data["id"].as_str().unwrap_or("unknown").to_string(),
        equity: account_data["equity"].as_str().unwrap_or("0").parse().unwrap_or(0.0),
        cash: account_data["cash"].as_str().unwrap_or("0").parse().unwrap_or(0.0),
        buying_power: account_data["buying_power"].as_str().unwrap_or("0").parse().unwrap_or(0.0),
        portfolio_value: account_data["portfolio_value"].as_str().unwrap_or("0").parse().unwrap_or(0.0),
        day_trade_count: account_data["daytrade_count"].as_i64().unwrap_or(0) as i32,
        pattern_day_trader: account_data["pattern_day_trader"].as_bool().unwrap_or(false),
    };

    let positions: Vec<Position> = positions_data
        .iter()
        .map(|pos| Position {
            symbol: pos["symbol"].as_str().unwrap_or("").to_string(),
            qty: pos["qty"].as_str().unwrap_or("0").parse().unwrap_or(0.0),
            market_value: pos["market_value"].as_str().unwrap_or("0").parse().unwrap_or(0.0),
            cost_basis: pos["cost_basis"].as_str().unwrap_or("0").parse().unwrap_or(0.0),
            unrealized_pl: pos["unrealized_pl"].as_str().unwrap_or("0").parse().unwrap_or(0.0),
            unrealized_plpc: pos["unrealized_plpc"].as_str().unwrap_or("0").parse().unwrap_or(0.0),
        })
        .collect();

    let event = AccountEvent {
        event_type: "account_snapshot".to_string(),
        timestamp: Utc::now(),
        account_snapshot,
        positions,
    };

    info!(
        target: "account_metrics",
        account_id = %event.account_snapshot.account_id,
        equity = %event.account_snapshot.equity,
        cash = %event.account_snapshot.cash,
        buying_power = %event.account_snapshot.buying_power,
        portfolio_value = %event.account_snapshot.portfolio_value,
        position_count = %event.positions.len(),
        "Account snapshot captured"
    );

    event_publisher.publish_account_event(&event).await?;

    Ok(())
}
