use rustify::{Client, Endpoint};
use rustify_derive::Endpoint;
use crate::prelude::StockTicker;

#[derive(Endpoint)]
#[endpoint(path = "test/path")]
/// Get the relative strength index (RSI) for a ticker symbol over a given time range.
pub struct RelativeStrengthIndex {
    pub ticker: StockTicker,
}

