import math
import time
from datetime import UTC, date
from typing import TYPE_CHECKING, cast
from zoneinfo import ZoneInfo

import polars as pl
from alpaca.data.enums import DataFeed
from alpaca.data.historical import StockHistoricalDataClient
from alpaca.data.requests import StockSnapshotRequest
from alpaca.trading import TradingClient
from alpaca.trading.enums import AssetClass, AssetStatus
from alpaca.trading.requests import GetAssetsRequest
from loguru import logger

if TYPE_CHECKING:
    from alpaca.data.models import Snapshot
    from alpaca.trading.models import Asset


class AlpacaClient:
    def __init__(
        self,
        api_key: str,
        api_secret: str,
        is_paper: bool,  # noqa: FBT001
    ) -> None:
        self.rate_limit_sleep = 0.5  # seconds

        self.historical_client = StockHistoricalDataClient(
            api_key=api_key,
            secret_key=api_secret,
            sandbox=is_paper,
        )

        self.trading_client = TradingClient(
            api_key=api_key,
            secret_key=api_secret,
            paper=is_paper,
        )

    def _get_tickers(self) -> list[str]:
        try:
            assets: list[Asset] = cast(
                "list[Asset]",
                self.trading_client.get_all_assets(
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

        time.sleep(self.rate_limit_sleep)

        return [asset.symbol for asset in assets]

    def fetch_latest_data(self, current_date: date) -> pl.DataFrame:
        tickers = self._get_tickers()

        equity_bars: list[dict[str, str | int | float | None]] = []
        chunk_size = 100
        failed_chunks = 0
        maximum_failed_chunks = len(tickers) // chunk_size // 4
        # determine total chunks (ceiling) and allow up to 25% failures, minimum 1
        n_chunks = max(1, (len(tickers) + chunk_size - 1) // chunk_size)
        maximum_failed_chunks = max(1, math.ceil(n_chunks * 0.25))  # up to 25%
        for i in range(0, len(tickers), chunk_size):
            tickers_subset = tickers[i : i + chunk_size]

            try:
                snapshots: dict[str, Snapshot] = cast(
                    "dict[str, Snapshot]",
                    self.historical_client.get_stock_snapshot(
                        StockSnapshotRequest(
                            symbol_or_symbols=tickers_subset,
                            feed=DataFeed("iex"),
                        )
                    ),
                )

                processed_tickers = set()
                missing_tickers = []

                for snapshot in snapshots.values():
                    if snapshot.daily_bar is None:
                        missing_tickers.append(snapshot.symbol)
                        continue

                    processed_tickers.add(snapshot.symbol)

                    daily_equity_bar = snapshot.daily_bar

                    daily_equity_bar_timestamp = daily_equity_bar.timestamp

                    # normalize naive timestamps to UTC to preserve the moment in time
                    if daily_equity_bar_timestamp.tzinfo is None:
                        utc_normalized_timestamp = daily_equity_bar_timestamp.replace(
                            tzinfo=UTC
                        )
                    else:
                        utc_normalized_timestamp = daily_equity_bar_timestamp

                    # create NY-localized datetime solely for date comparison
                    ny_localized_timestamp = utc_normalized_timestamp.astimezone(
                        ZoneInfo("America/New_York")
                    )
                    daily_equity_bar_date = ny_localized_timestamp.date()

                    if daily_equity_bar_date != current_date:
                        logger.info(
                            f"Skipping equity bar for {snapshot.symbol} on {daily_equity_bar_date}"  # noqa: E501
                        )
                        continue

                    equity_bars.append(
                        {
                            "ticker": snapshot.symbol,
                            "timestamp": int(
                                utc_normalized_timestamp.timestamp() * 1000
                            ),  # convert to milliseconds using UTC-normalized datetime
                            "open_price": float(daily_equity_bar.open),
                            "high_price": float(daily_equity_bar.high),
                            "low_price": float(daily_equity_bar.low),
                            "close_price": float(daily_equity_bar.close),
                            "volume": int(daily_equity_bar.volume),
                            "volume_weighted_average_price": (
                                float(daily_equity_bar.vwap)
                                if daily_equity_bar.vwap is not None
                                else None
                            ),
                        }
                    )

                tickers_without_snapshots = (
                    set(tickers_subset) - processed_tickers - set(missing_tickers)
                )
                if tickers_without_snapshots:
                    logger.warning(
                        f"No snapshots available for tickers: {list(tickers_without_snapshots)}"  # noqa: E501
                    )

                if missing_tickers:
                    logger.warning(
                        f"No daily_bar available for tickers: {missing_tickers}"
                    )

                time.sleep(self.rate_limit_sleep)

            except Exception as e:
                logger.error(
                    f"Error fetching Alpaca snapshots for chunk {i // chunk_size + 1}: {e}"  # noqa: E501
                )
                failed_chunks += 1
                if failed_chunks > maximum_failed_chunks:
                    message = f"Too many chunk failures: {failed_chunks} chunks failed"
                    raise RuntimeError(message) from e
                continue  # continue with next chunk instead of raising

        logger.info(
            f"Collected {len(equity_bars)} equity bar records from {len(tickers)} tickers"  # noqa: E501
        )

        return pl.DataFrame(equity_bars)
