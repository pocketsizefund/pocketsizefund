import os
import time
from datetime import datetime, timedelta
from typing import cast
from zoneinfo import ZoneInfo

import polars as pl
from alpaca.data.enums import Adjustment, DataFeed
from alpaca.data.historical import StockHistoricalDataClient
from alpaca.data.models import BarSet
from alpaca.data.requests import StockBarsRequest
from alpaca.data.timeframe import TimeFrame, TimeFrameUnit
from alpaca.trading import TradingClient
from alpaca.trading.enums import AssetClass, AssetStatus
from alpaca.trading.models import Asset
from alpaca.trading.requests import GetAssetsRequest
from internal.equity_bar import equity_bar_schema
from loguru import logger

if __name__ == "__main__":
    rate_limit_sleep = 0.5  # seconds

    api_key = os.getenv("ALPACA_API_KEY_ID")
    secret_key = os.getenv("ALPACA_API_SECRET_KEY")

    if not api_key or not secret_key:
        message = "Missing required environment variables: ALPACA_API_KEY_ID and/or ALPACA_API_SECRET_KEY"  # noqa: E501
        logger.error(message)
        raise ValueError(message)

    alpaca_trading_client = TradingClient(
        api_key=api_key,
        secret_key=secret_key,
        paper=os.getenv("ALPACA_PAPER", "true").lower() == "true",
    )

    alpaca_data_client = StockHistoricalDataClient(
        api_key=api_key,
        secret_key=secret_key,
        sandbox=os.getenv("ALPACA_PAPER", "true").lower() == "true",
    )

    try:
        assets: list[Asset] = cast(
            "list[Asset]",
            alpaca_trading_client.get_all_assets(
                GetAssetsRequest(
                    status=AssetStatus.ACTIVE,
                    asset_class=AssetClass.US_EQUITY,
                    attributes="has_options",
                )
            ),
        )

    except Exception as e:
        logger.error(f"Error fetching Alpaca assets: {e}")
        raise

    time.sleep(rate_limit_sleep)

    end = datetime.now(tz=ZoneInfo("America/New_York"))
    start = end - timedelta(days=365 * 6)

    saved_files: list[str] = []
    for i, asset in enumerate(assets):
        ticker = asset.symbol

        logger.info(f"Fetching {i + 1}/{len(assets)}: {ticker}")

        try:
            equity_bars: BarSet = cast(
                "BarSet",
                alpaca_data_client.get_stock_bars(
                    StockBarsRequest(
                        symbol_or_symbols=ticker,
                        start=start,
                        end=end,
                        limit=10000,
                        timeframe=TimeFrame(
                            amount=1,
                            unit=TimeFrameUnit("Day"),
                        ),
                        adjustment=Adjustment("all"),
                        feed=DataFeed("iex"),
                    )
                ),
            )

        except Exception as e:  # noqa: BLE001
            logger.error(f"Error fetching equity bars for {ticker}: {e}")

            time.sleep(rate_limit_sleep)

            continue

        if len(equity_bars.dict()) == 0:
            logger.info(f"No equity bars found for {ticker}.")

            time.sleep(rate_limit_sleep)

            continue

        equity_bars_data = pl.DataFrame(equity_bars[ticker])

        equity_bars_data = equity_bars_data.rename(
            {
                "symbol": "ticker",
                "open": "open_price",
                "high": "high_price",
                "low": "low_price",
                "close": "close_price",
                "vwap": "volume_weighted_average_price",
            }
        )
        equity_bars_data = equity_bars_data.with_columns(
            (
                pl.col("timestamp").map_elements(lambda x: int(x.timestamp() * 1000))
            ).alias("timestamp")
        )

        file_path = f"equity_bars_{ticker}.csv"
        equity_bars_data.write_csv(file_path)
        saved_files.append(file_path)

        logger.info(f"Saved bars for {ticker} to {file_path}.")

        time.sleep(rate_limit_sleep)

    logger.info("Finished fetching all tickers.")

    if saved_files:
        all_equity_bars = pl.concat([pl.read_csv(fp) for fp in saved_files])
        all_equity_bars = equity_bar_schema.validate(all_equity_bars)
        all_equity_bars.write_csv("equity_bars.csv")
        logger.info("Finished saving combined equity bars.")

    else:
        logger.warning("No equity bars saved; skipping combined file.")
