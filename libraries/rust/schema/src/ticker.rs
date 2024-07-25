
use serde::{Deserialize, Serialize};


#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Ticker {
    symbol: Symbol,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct Symbol(String);