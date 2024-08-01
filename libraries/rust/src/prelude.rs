use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Price(Decimal);

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Volume(u64);

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct StockTicker(pub String);
