use crate::data::{
    create_equity_bar_dataframe, create_portfolio_dataframe, create_predictions_dataframe,
    EquityBar, Portfolio, Prediction,
};
use crate::errors::Error;
use crate::state::State;
use aws_credential_types::provider::ProvideCredentials;
use aws_sdk_s3::primitives::ByteStream;
use chrono::{DateTime, Utc};
use duckdb::Connection;
use polars::prelude::*;
use serde::Deserialize;
use std::io::Cursor;
use tracing::{debug, info};

pub async fn write_equity_bars_dataframe_to_s3(
    state: &State,
    dataframe: &DataFrame,
    date: &DateTime<Utc>,
) -> Result<String, Error> {
    write_dataframe_to_s3(state, dataframe, date, "bars".to_string()).await
}

pub async fn write_portfolio_dataframe_to_s3(
    state: &State,
    dataframe: &DataFrame,
    date: &DateTime<Utc>,
) -> Result<String, Error> {
    write_dataframe_to_s3(state, dataframe, date, "portfolios".to_string()).await
}

pub async fn write_predictions_dataframe_to_s3(
    state: &State,
    dataframe: &DataFrame,
    date: &DateTime<Utc>,
) -> Result<String, Error> {
    write_dataframe_to_s3(state, dataframe, date, "predictions".to_string()).await
}

async fn write_dataframe_to_s3(
    state: &State,
    dataframe: &DataFrame,
    date: &DateTime<Utc>,
    dataframe_type: String,
) -> Result<String, Error> {
    info!("Uploading DataFrame to S3 as parquet");

    let year = date.format("%Y");
    let month = date.format("%m");
    let day = date.format("%d");

    let key = format!(
        "equity/{}/daily/year={}/month={}/day={}/data.parquet",
        dataframe_type, year, month, day,
    );

    let mut buffer = Vec::new();
    {
        let cursor = Cursor::new(&mut buffer);
        let writer = ParquetWriter::new(cursor);
        match writer.finish(&mut dataframe.clone()) {
            Ok(_) => {
                info!(
                    "DataFrame successfully converted to parquet, size: {} bytes",
                    buffer.len()
                );
            }
            Err(err) => {
                return Err(Error::Other(format!("Failed to write parquet: {}", err)));
            }
        }
    }

    let body = ByteStream::from(buffer);

    match state
        .s3_client
        .put_object()
        .bucket(&state.bucket_name)
        .key(&key)
        .body(body)
        .content_type("application/octet-stream")
        .send()
        .await
    {
        Ok(_) => {
            info!(
                "Successfully uploaded parquet file to s3://{}/{}",
                state.bucket_name, key
            );
            Ok(key)
        }
        Err(err) => Err(Error::Other(format!("Failed to upload to S3: {}", err))),
    }
}

async fn create_duckdb_connection() -> Result<Connection, Error> {
    let connection = Connection::open_in_memory()?;

    connection.execute_batch("INSTALL httpfs; LOAD httpfs;")?;

    let config = aws_config::load_defaults(aws_config::BehaviorVersion::latest()).await;
    let provider = config
        .credentials_provider()
        .ok_or_else(|| Error::Other("No AWS credentials provider found".into()))?;
    let credentials = provider.provide_credentials().await?;
    let region = config
        .region()
        .map(|r| r.as_ref().to_string())
        .unwrap_or_else(|| "us-east-1".to_string());
    let session_token = credentials.session_token().unwrap_or_default();
    let s3_config = format!(
        "
            SET s3_region='{}';
            SET s3_url_style='path';
            SET s3_access_key_id='{}';
            SET s3_secret_access_key='{}';
            SET s3_session_token='{}';
        ",
        region,
        credentials.access_key_id(),
        credentials.secret_access_key(),
        session_token
    );

    connection.execute_batch(&s3_config)?;

    Ok(connection)
}

pub async fn query_equity_bars_parquet_from_s3(
    state: &State,
    tickers: Option<Vec<String>>,
    start_date: Option<DateTime<Utc>>,
    end_date: Option<DateTime<Utc>>,
) -> Result<Vec<u8>, Error> {
    let connection = create_duckdb_connection().await?;

    let (start_date, end_date) = match (start_date, end_date) {
        (Some(start), Some(end)) => (start, end),
        _ => {
            let end_date = chrono::Utc::now();
            let start_date = end_date - chrono::Duration::days(7);
            (start_date, end_date)
        }
    };

    info!("Querying data from {} to {}", start_date, end_date);

    let mut s3_paths = Vec::new();
    let mut current_date = start_date;

    while current_date <= end_date {
        let year = current_date.format("%Y");
        let month = current_date.format("%m");
        let day = current_date.format("%d");

        let s3_path = format!(
            "s3://{}/equity/bars/daily/year={}/month={}/day={}/data.parquet",
            state.bucket_name, year, month, day
        );
        s3_paths.push(s3_path);

        current_date += chrono::Duration::days(1);
    }

    if s3_paths.is_empty() {
        return Err(Error::Other(
            "No files to query for the given date range".to_string(),
        ));
    }

    info!("Querying {} S3 files", s3_paths.len());

    let s3_paths_str = s3_paths
        .iter()
        .map(|path| format!("SELECT * FROM '{}'", path))
        .collect::<Vec<_>>()
        .join(" UNION ALL ");

    let ticker_filter = match &tickers {
        Some(ticker_list) if !ticker_list.is_empty() => {
            let ticker_values = ticker_list
                .iter()
                .map(|t| format!("'{}'", t.replace('\'', "''")))
                .collect::<Vec<_>>()
                .join(", ");
            format!("WHERE ticker IN ({})", ticker_values)
        }
        _ => String::new(),
    };

    let query_sql = format!(
        "
        SELECT
            ticker,
            timestamp,
            open_price,
            high_price,
            low_price,
            close_price,
            volume,
            volume_weighted_average_price,
            transactions
        FROM ({})
        {}
        ORDER BY timestamp, ticker
        ",
        s3_paths_str, ticker_filter
    );

    debug!("Executing query SQL: {}", query_sql);

    let mut statement = connection.prepare(&query_sql)?;
    let equity_bars: Vec<EquityBar> = statement
        .query_map([], |row| {
            Ok(EquityBar {
                ticker: row.get(0)?,
                timestamp: row.get(1)?,
                open_price: row.get(2)?,
                high_price: row.get(3)?,
                low_price: row.get(4)?,
                close_price: row.get(5)?,
                volume: row.get(6)?,
                volume_weighted_average_price: row.get(7)?,
                transactions: row.get(8)?,
            })
        })?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| Error::Other(format!("Failed to map query results: {}", e)))?;

    let equity_bars_dataframe = create_equity_bar_dataframe(equity_bars);

    let mut buffer = Vec::new();
    {
        let cursor = Cursor::new(&mut buffer);
        let writer = ParquetWriter::new(cursor);
        writer
            .finish(&mut equity_bars_dataframe?.clone())
            .map_err(|e| Error::Other(format!("Failed to write parquet: {}", e)))?;
    }

    info!("Query returned {} bytes of parquet data", buffer.len());

    Ok(buffer)
}

#[derive(Deserialize)]
pub struct PredictionQuery {
    pub ticker: String,
    pub timestamp: DateTime<Utc>,
}

pub async fn query_predictions_dataframe_from_s3(
    state: &State,
    predictions_query: Vec<PredictionQuery>,
) -> Result<DataFrame, Error> {
    let connection = create_duckdb_connection().await?;

    let mut s3_paths = Vec::new();
    let mut tickers = Vec::new();

    for prediction_query in predictions_query.iter() {
        let year = prediction_query.timestamp.format("%Y");
        let month = prediction_query.timestamp.format("%m");
        let day = prediction_query.timestamp.format("%d");

        let s3_path = format!(
            "s3://{}/equity/predictions/daily/year={}/month={}/day={}/data.parquet",
            state.bucket_name, year, month, day
        );

        s3_paths.push(s3_path);

        tickers.push(prediction_query.ticker.clone());
    }

    if s3_paths.is_empty() {
        return Err(Error::Other("No positions provided".into()));
    }

    info!("Querying {} S3 files", s3_paths.len());

    let s3_paths_query = s3_paths
        .iter()
        .map(|path| format!("SELECT * FROM '{}'", path))
        .collect::<Vec<_>>()
        .join(" UNION ALL ");

    let tickers_query = tickers
        .iter()
        .map(|ticker| format!("'{}'", ticker))
        .collect::<Vec<_>>()
        .join(", ");

    let query = format!(
        "
        SELECT
            ticker,
            timestamp,
            quantile_10,
            quantile_50,
            quantile_90
        FROM ({})
        WHERE ticker IN ({})
        ORDER BY timestamp, ticker
        ",
        s3_paths_query, tickers_query,
    );

    debug!("Executing export SQL: {}", query);

    let mut statement = connection.prepare(&query)?;

    let predictions: Vec<Prediction> = statement
        .query_map([], |row| {
            Ok(Prediction {
                ticker: row.get(0)?,
                timestamp: row.get(1)?,
                quantile_10: row.get(2)?,
                quantile_50: row.get(3)?,
                quantile_90: row.get(4)?,
            })
        })?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| Error::Other(format!("Failed to map query results: {}", e)))?;

    let predictions_dataframe = create_predictions_dataframe(predictions)?;

    Ok(predictions_dataframe)
}

pub async fn query_portfolio_dataframe_from_s3(
    state: &State,
    timestamp: &DateTime<Utc>,
) -> Result<DataFrame, Error> {
    let connection = create_duckdb_connection().await?;

    let year = timestamp.format("%Y");
    let month = timestamp.format("%m");
    let day = timestamp.format("%d");
    let s3_path = format!(
        "s3://{}/equity/portfolios/daily/year={}/month={}/day={}/data.parquet",
        state.bucket_name, year, month, day
    );
    info!("Querying 1 S3 file");
    let s3_paths_query = format!("SELECT * FROM '{}'", s3_path);

    let query = format!(
        "
        SELECT
            ticker,
            timestamp,
            side,
            dollar_amount
        FROM ({})
        ORDER BY timestamp, ticker
        ",
        s3_paths_query,
    );
    debug!("Executing export SQL: {}", query);

    let mut statement = connection.prepare(&query)?;

    let portfolios: Vec<Portfolio> = statement
        .query_map([], |row| {
            Ok(Portfolio {
                ticker: row.get::<_, String>(0)?,
                timestamp: row.get::<_, i64>(1)?,
                side: row.get::<_, String>(2)?,
                dollar_amount: row.get::<_, f64>(3)?,
            })
        })?
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| Error::Other(format!("Failed to map query results: {}", e)))?;

    let portfolio_dataframe = create_portfolio_dataframe(portfolios)?;

    Ok(portfolio_dataframe)
}

pub async fn read_equity_details_csv_from_s3(state: &State) -> Result<String, Error> {
    info!("Reading equity details CSV from S3");

    let key = "equity/details/categories.csv";

    let response = state
        .s3_client
        .get_object()
        .bucket(&state.bucket_name)
        .key(key)
        .send()
        .await
        .map_err(|e| Error::Other(format!("Failed to get object from S3: {}", e)))?;

    let bytes = response
        .body
        .collect()
        .await
        .map_err(|e| Error::Other(format!("Failed to read response body: {}", e)))?
        .into_bytes();

    let csv_content = String::from_utf8(bytes.to_vec())
        .map_err(|e| Error::Other(format!("Failed to convert bytes to UTF-8: {}", e)))?;

    info!(
        "Successfully read CSV from S3, size: {} bytes",
        csv_content.len()
    );

    Ok(csv_content)
}
