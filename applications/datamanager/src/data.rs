use crate::errors::Error;
use polars::prelude::*;
use serde::Deserialize;
use std::io::Cursor;

#[derive(Debug, Deserialize)]
pub struct EquityBar {
    pub ticker: String,
    pub timestamp: i64,
    pub open_price: Option<u64>,
    pub high_price: Option<u64>,
    pub low_price: Option<u64>,
    pub close_price: Option<u64>,
    pub volume: Option<u64>,
    pub volume_weighted_average_price: Option<u64>,
    pub transactions: Option<u64>,
}

pub fn create_equity_bar_dataframe(equity_bars_rows: Vec<EquityBar>) -> Result<DataFrame, Error> {
    let equity_bars_dataframe = df!(
        "ticker" => equity_bars_rows.iter().map(|b| b.ticker.as_str()).collect::<Vec<_>>(),
        "timestamp" => equity_bars_rows.iter().map(|b| b.timestamp).collect::<Vec<_>>(),
        "open_price" => equity_bars_rows.iter().map(|b| b.open_price).collect::<Vec<_>>(),
        "high_price" => equity_bars_rows.iter().map(|b| b.high_price).collect::<Vec<_>>(),
        "low_price" => equity_bars_rows.iter().map(|b| b.low_price).collect::<Vec<_>>(),
        "close_price" => equity_bars_rows.iter().map(|b| b.close_price).collect::<Vec<_>>(),
        "volume" => equity_bars_rows.iter().map(|b| b.volume).collect::<Vec<_>>(),
        "volume_weighted_average_price" => equity_bars_rows.iter().map(|b| b.volume_weighted_average_price).collect::<Vec<_>>(),
        "transactions" => equity_bars_rows.iter().map(|b| b.transactions).collect::<Vec<_>>(),
    )
    .map_err(|e| Error::Other(format!("Failed to create DataFrame: {}", e)))?;

    let equity_bars_dataframe = equity_bars_dataframe
        .lazy()
        .with_columns([col("ticker").str().to_uppercase().alias("ticker")])
        .collect()?;

    Ok(equity_bars_dataframe)
}

#[derive(Debug, Deserialize)]
pub struct Prediction {
    pub ticker: String,
    pub timestamp: i64,
    pub quantile_10: f64,
    pub quantile_50: f64,
    pub quantile_90: f64,
}

pub fn create_predictions_dataframe(prediction_rows: Vec<Prediction>) -> Result<DataFrame, Error> {
    let prediction_dataframe = df!(
        "ticker" => prediction_rows.iter().map(|p| p.ticker.as_str()).collect::<Vec<_>>(),
        "timestamp" => prediction_rows.iter().map(|p| p.timestamp).collect::<Vec<_>>(),
        "quantile_10" => prediction_rows.iter().map(|p| p.quantile_10).collect::<Vec<_>>(),
        "quantile_50" => prediction_rows.iter().map(|p| p.quantile_50).collect::<Vec<_>>(),
        "quantile_90" => prediction_rows.iter().map(|p| p.quantile_90).collect::<Vec<_>>(),
    )
    .map_err(|e| Error::Other(format!("Failed to create DataFrame: {}", e)))?;

    let unfiltered_prediction_dataframe = prediction_dataframe
        .lazy()
        .with_columns([col("ticker").str().to_uppercase().alias("ticker")])
        .collect()?;

    // filtering necessary due to potentially overlapping tickers in predictions parquet files
    let filtered_prediction_dataframe = unfiltered_prediction_dataframe
        .lazy()
        .with_columns([col("timestamp")
            .max()
            .over([col("ticker")])
            .alias("max_timestamp")])
        .filter(col("timestamp").eq(col("max_timestamp")))
        .select([
            col("ticker"),
            col("timestamp"),
            col("quantile_10"),
            col("quantile_50"),
            col("quantile_90"),
        ])
        .collect()?;

    Ok(filtered_prediction_dataframe)
}

#[derive(Debug, Deserialize)]
pub struct Portfolio {
    pub ticker: String,
    pub timestamp: i64,
    pub side: String,
    pub dollar_amount: f64,
    pub action: String,
}

pub fn create_portfolio_dataframe(portfolio_rows: Vec<Portfolio>) -> Result<DataFrame, Error> {
    let portfolio_dataframe = df!(
        "ticker" => portfolio_rows.iter().map(|p| p.ticker.as_str()).collect::<Vec<&str>>(),
        "timestamp" => portfolio_rows.iter().map(|p| p.timestamp).collect::<Vec<i64>>(),
        "side" => portfolio_rows.iter().map(|p| p.side.as_str()).collect::<Vec<&str>>(),
        "dollar_amount" => portfolio_rows.iter().map(|p| p.dollar_amount).collect::<Vec<f64>>(),
        "action" => portfolio_rows.iter().map(|p| p.action.as_str()).collect::<Vec<&str>>(),
    )
    .map_err(|e| Error::Other(format!("Failed to create DataFrame: {}", e)))?;

    let portfolio_dataframe = portfolio_dataframe
        .lazy()
        .with_columns([col("ticker").str().to_uppercase().alias("ticker")])
        .with_columns([col("side").str().to_uppercase().alias("side")])
        .with_columns([col("action").str().to_uppercase().alias("action")])
        .collect()?;

    Ok(portfolio_dataframe)
}

pub fn create_equity_details_dataframe(csv_content: String) -> Result<DataFrame, Error> {
    let cursor = Cursor::new(csv_content.as_bytes());
    let mut dataframe = CsvReadOptions::default()
        .with_has_header(true)
        .into_reader_with_file_handle(cursor)
        .finish()
        .map_err(|e| Error::Other(format!("Failed to parse CSV: {}", e)))?;

    let required_columns = vec!["sector", "industry"];
    let column_names = dataframe.get_column_names();
    for column in &required_columns {
        if !column_names.iter().any(|c| c.as_str() == *column) {
            let message = format!("CSV missing required column: {}", column);
            return Err(Error::Other(message));
        }
    }

    dataframe = dataframe
        .select(required_columns)
        .map_err(|e| Error::Other(format!("Failed to select columns: {}", e)))?;

    let equity_details_dataframe = dataframe
        .lazy()
        .with_columns([
            col("sector")
                .str()
                .to_uppercase()
                .fill_null(lit("NOT AVAILABLE"))
                .alias("sector"),
            col("industry")
                .str()
                .to_uppercase()
                .fill_null(lit("NOT AVAILABLE"))
                .alias("industry"),
        ])
        .collect()
        .map_err(|e| Error::Other(format!("Failed to transform columns: {}", e)))?;

    Ok(equity_details_dataframe)
}
