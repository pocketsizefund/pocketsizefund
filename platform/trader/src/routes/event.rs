use actix_web::post;
use anyhow::Result;
use apca::api::v2::account;
use apca::api::v2::order::{Amount, Create, CreateReqInit, Order, Side, Type};
use apca::api::v2::orders::List;
use apca::api::v2::orders::ListReq;
use apca::ApiInfo;
use apca::Client as ApcaClient;
use cloudevents::AttributesReader;
use cloudevents::Event;
use num_decimal::Num;
use pocketsizefund::events::Market;
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use std::fmt;
use tracing::{error, info};

#[post("/")]
pub async fn handler(event: Event) -> Event {
    info!("received event: {:?}", event);
    info!("received event attributes: {:?}", event.source());
    let mut market = Market::default();
    market.check_current_status().await;

    info!("market status: {:?}", market.status);

    let account = get_account().await.unwrap();
    let account_standing = AccountStanding::new(account);

    if !account_standing.in_good_standing() {
        error!("account not in good standing");
        post_to_discord(format!(
            "account not in good standing: {}",
            account_standing
        ))
        .await;
        return event;
    }
    tracing::info!("account in good standing: {}", account_standing);

    let predictions = get_predictions().await;

    let open_orders = get_orders().await.unwrap();

    post_to_discord(format!("open orders: {}", open_orders.len())).await;

    match predictions {
        Ok(predictions) => {
            let choice = predictions.pick_at_random().unwrap();
            let response = baseline_buy(choice).await.unwrap();
            post_to_discord(format!(
                "initiated market order [symbol={}, order id={}]",
                response.symbol, response.client_order_id
            ))
            .await;
        }
        Err(e) => {
            info!("failed to get predictions: {:?}", e);
            post_to_discord("failed to get predictions".to_string()).await;
            return event;
        }
    };

    info!("open orders: {:?}", open_orders);
    event
}

#[derive(Debug, Serialize, Deserialize)]
struct Tickers {
    tickers: HashMap<String, Vec<f64>>,
}

impl Tickers {
    fn pick_at_random(&self) -> Option<&String> {
        use rand::seq::SliceRandom;
        self.tickers
            .keys()
            .collect::<Vec<_>>()
            .choose(&mut rand::thread_rng())
            .copied()
    }
}

async fn get_predictions() -> Result<Tickers, Box<dyn std::error::Error>> {
    let predictions_endpoint = env::var("PRICE_MODEL_URL")?;

    let response = Client::new()
        .post(predictions_endpoint)
        .header("Content-Type", "text/plain")
        .header("ce-specversion", "1.0")
        .header("ce-id", uuid::Uuid::new_v4().to_string())
        .header("ce-source", "psf.platform.trader")
        .header("ce-type", "price.prediciton.request")
        .send()
        .await?;

    info!("response: {:?}", response);

    let predictions: Tickers = response.json().await?;

    info!("predictions: {:?}", predictions);

    Ok(predictions)
}

async fn post_to_discord(message: String) {
    let discord_endpoint = env::var("DISCORD_BOT_URL").unwrap();

    reqwest::Client::new()
        .post(discord_endpoint)
        .header("Content-Type", "text/plain")
        .header("ce-specversion", "1.0")
        .header("ce-id", uuid::Uuid::new_v4().to_string())
        .header("ce-source", "psf.platform.trader")
        .header("ce-type", "discord.message.request")
        .body(message)
        .send()
        .await
        .expect("Failed to execute request.");
}

async fn get_account() -> Result<account::Account, Box<dyn std::error::Error>> {
    let api_info = ApiInfo::from_env().unwrap();
    let client = ApcaClient::new(api_info);

    let account = client.issue::<account::Get>(&()).await?;
    info!("account: {:#?}", account);
    Ok(account)
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
enum AccountStatus {
    Active,
    Unknown,
}

#[derive(Debug, Serialize, Deserialize)]
struct AccountStanding {
    status: AccountStatus,
    day_trader: bool,
    trading_suspended: bool,
    trading_blocked: bool,
    transfers_blocked: bool,
    account_blocked: bool,
    daytrade_count: u64,
}

impl AccountStanding {
    fn new(account: account::Account) -> Self {
        Self {
            status: match account.status {
                account::Status::Active => AccountStatus::Active,
                _ => AccountStatus::Unknown,
            },
            day_trader: account.day_trader,
            trading_suspended: account.trading_suspended,
            trading_blocked: account.trading_blocked,
            transfers_blocked: account.transfers_blocked,
            account_blocked: account.account_blocked,
            daytrade_count: account.daytrade_count,
        }
    }

    fn in_good_standing(&self) -> bool {
        self.status == AccountStatus::Active
            && !self.trading_suspended
            && !self.trading_blocked
            && !self.transfers_blocked
            && !self.account_blocked
            && !self.day_trader
            && self.daytrade_count < 3
    }
}

impl fmt::Display for AccountStanding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "status: {:?}\n, day_trader: {}\n, trading_suspended: {}\n, trading_blocked: {}\n, transfers_blocked: {}\n, account_blocked: {}\n, daytrade_count: {}\n",
            self.status,
            self.day_trader,
            self.trading_suspended,
            self.trading_blocked,
            self.transfers_blocked,
            self.account_blocked,
            self.daytrade_count
        )
    }
}

async fn get_orders() -> Result<Vec<Order>> {
    let api_info = ApiInfo::from_env()?;
    let client = ApcaClient::new(api_info);

    let request = ListReq::default();

    let orders = client.issue::<List>(&request).await?;

    Ok(orders)
}

async fn baseline_buy(ticker: &str) -> Result<Order> {
    let api_info = ApiInfo::from_env()?;
    let client = ApcaClient::new(api_info);

    let order_req = CreateReqInit {
        type_: Type::Market,
        time_in_force: apca::api::v2::order::TimeInForce::Day,
        ..Default::default()
    }
    .init(
        ticker.to_string(),
        Side::Buy,
        Amount::notional(Num::from(1)),
    );

    let order = client.issue::<Create>(&order_req).await?;

    Ok(order)
}
