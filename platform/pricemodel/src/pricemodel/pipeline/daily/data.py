import os
from prefect import task
from polygon import RESTClient
from datetime import timedelta
from prefect.tasks import task_input_hash
import pandas as pd


polygon = RESTClient(api_key=os.getenv("POLYGON_API_KEY"))


# @task(cache_key_fn=task_input_hash, cache_expiration=timedelta(hours=24))
def load_all_tickers() -> list:
    tickers = []
    for t in polygon.list_tickers(market="stocks", type="CS", active=True, limit=1000):
        tickers.append(t)

    tickers = pd.DataFrame([{
        "symbol": ticker.ticker,
        "name": ticker.name,
        "cik": ticker.cik,
        "composite_figi": ticker.composite_figi,
        "currency_name": ticker.currency_name,
        "currency_symbol": ticker.currency_symbol,
        "base_currency_symbol": ticker.base_currency_symbol,
        "base_currency_name": ticker.base_currency_name,
        "delisted_utc": ticker.delisted_utc,
        "last_updated_utc": ticker.last_updated_utc,
        "locale": ticker.locale,
        "market": ticker.market,
        "primary_exchange": ticker.primary_exchange,
        "share_class_figi": ticker.share_class_figi,
        "type": ticker.type,
        "source_feed": ticker.source_feed,

    } for ticker in tickers])

    tickers.to_csv("tickers.csv")

    return tickers["symbol"].tolist()



# @task(cache_key_fn=task_input_hash, cache_expiration=timedelta(hours=24))
def filter_tickers(tickers: list) -> list:
    existing_files = os.listdir("data/previous-close")

    existing_tickers = [file.split(".csv")[0] for file in existing_files if file.endswith(".csv")]

    filtered_tickers = [ticker for ticker in tickers if ticker not in existing_tickers]

    return filtered_tickers


# @task(cache_key_fn=task_input_hash, cache_expiration=timedelta(hours=24))
def load_previous_close(ticker: str) -> tuple[str, list]:
    """Get yesterday's bars for a ticker."""
    aggs = []
    for a in polygon.list_aggs(ticker=ticker, multiplier=1, timespan="day", from_="2020-01-01", to="2024-09-16", limit=5000):
        aggs.append(a)

    return ticker, aggs


# @task(cache_key_fn=task_input_hash, cache_expiration=timedelta(hours=24))
def transform_previous_close(previous_close: tuple[str, list]) -> list:
    """Transform previous close data."""
    return pd.DataFrame([{
        "ticker": previous_close[0],
        "open": agg.open,
        "high": agg.high,
        "low": agg.low,
        "close": agg.close,
        "volume": agg.volume,
        "timestamp": pd.to_datetime(agg.timestamp, unit="ms").strftime("%Y-%m-%d"),
    }
        for agg
        in previous_close[1]
    ])



# @task(cache_key_fn=task_input_hash, cache_expiration=timedelta(hours=24))
def save_previous_close(ticker, previous_close: list) -> None:
    """Save previous close data."""
    previous_close.to_csv(f"data/previous-close/{ticker}.csv")
