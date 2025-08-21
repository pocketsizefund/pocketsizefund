from datetime import date, datetime
from unittest.mock import MagicMock, patch
from zoneinfo import ZoneInfo

import polars as pl
from datamanager.alpaca_client import AlpacaClient


def test_alpaca_client_fetch_latest_data() -> None:
    with (
        patch(
            "datamanager.alpaca_client.StockHistoricalDataClient"
        ) as mock_historical_client,
        patch("datamanager.alpaca_client.TradingClient") as mock_trading_client,
        patch("time.sleep") as mock_sleep,  # noqa: F841
    ):
        mock_trading_instance = MagicMock()
        mock_trading_client.return_value = mock_trading_instance

        mock_asset1 = MagicMock()
        mock_asset1.symbol = "AAPL"
        mock_asset2 = MagicMock()
        mock_asset2.symbol = "GOOGL"

        mock_trading_instance.get_all_assets.return_value = [mock_asset1, mock_asset2]

        mock_historical_instance = MagicMock()
        mock_historical_client.return_value = mock_historical_instance

        open_price = 150.0
        high_price = 155.0
        low_price = 149.0
        close_price = 153.0
        volume = 1000000
        vwap = 152.5

        timezone = ZoneInfo("America/New_York")

        current_date = date(2024, 1, 15)
        mock_daily_bar = MagicMock()
        mock_daily_bar.timestamp = datetime(2024, 1, 15, 16, 0, 0, tzinfo=timezone)
        mock_daily_bar.open = open_price
        mock_daily_bar.high = high_price
        mock_daily_bar.low = low_price
        mock_daily_bar.close = close_price
        mock_daily_bar.volume = volume
        mock_daily_bar.vwap = vwap

        mock_snapshot = MagicMock()
        mock_snapshot.symbol = "AAPL"
        mock_snapshot.daily_bar = mock_daily_bar

        mock_snapshots = {"AAPL": mock_snapshot}
        mock_historical_instance.get_stock_snapshot.return_value = mock_snapshots

        alpaca_client = AlpacaClient("test-key", "test-secret", True)  # noqa: FBT003

        result = alpaca_client.fetch_latest_data(current_date)

        assert isinstance(result, pl.DataFrame)
        assert len(result) == 1

        row = result.row(0, named=True)
        assert row["ticker"] == "AAPL"
        assert row["open_price"] == open_price
        assert row["high_price"] == high_price
        assert row["low_price"] == low_price
        assert row["close_price"] == close_price
        assert row["volume"] == volume
        assert row["volume_weighted_average_price"] == vwap

        expected_timestamp = int(
            datetime(2024, 1, 15, 16, 0, 0, tzinfo=timezone).timestamp() * 1000
        )
        assert row["timestamp"] == expected_timestamp


def test_alpaca_client_fetch_latest_data_no_daily_bar() -> None:
    with (
        patch(
            "datamanager.alpaca_client.StockHistoricalDataClient"
        ) as mock_historical_client,
        patch("datamanager.alpaca_client.TradingClient") as mock_trading_client,
        patch("time.sleep") as mock_sleep,  # noqa: F841
    ):
        mock_trading_instance = MagicMock()
        mock_trading_client.return_value = mock_trading_instance

        mock_asset = MagicMock()
        mock_asset.symbol = "TEST"
        mock_trading_instance.get_all_assets.return_value = [mock_asset]

        mock_historical_instance = MagicMock()
        mock_historical_client.return_value = mock_historical_instance

        mock_snapshot = MagicMock()
        mock_snapshot.symbol = "TEST"
        mock_snapshot.daily_bar = None

        mock_snapshots = {"TEST": mock_snapshot}
        mock_historical_instance.get_stock_snapshot.return_value = mock_snapshots

        alpaca_client = AlpacaClient("test-key", "test-secret", True)  # noqa: FBT003

        current_date = date(2024, 1, 15)
        result = alpaca_client.fetch_latest_data(current_date)

        assert isinstance(result, pl.DataFrame)
        assert len(result) == 0


def test_alpaca_client_fetch_latest_data_wrong_date() -> None:
    with (
        patch(
            "datamanager.alpaca_client.StockHistoricalDataClient"
        ) as mock_historical_client,
        patch("datamanager.alpaca_client.TradingClient") as mock_trading_client,
        patch("time.sleep") as mock_sleep,  # noqa: F841
    ):
        mock_trading_instance = MagicMock()
        mock_trading_client.return_value = mock_trading_instance

        mock_asset = MagicMock()
        mock_asset.symbol = "AAPL"
        mock_trading_instance.get_all_assets.return_value = [mock_asset]

        mock_historical_instance = MagicMock()
        mock_historical_client.return_value = mock_historical_instance

        timezone = ZoneInfo("America/New_York")

        mock_daily_bar = MagicMock()
        mock_daily_bar.timestamp = datetime(
            2024, 1, 14, 16, 0, 0, tzinfo=timezone
        )  # Wrong date
        mock_daily_bar.open = 150.0
        mock_daily_bar.high = 155.0
        mock_daily_bar.low = 149.0
        mock_daily_bar.close = 153.0
        mock_daily_bar.volume = 1000000
        mock_daily_bar.vwap = 152.5

        mock_snapshot = MagicMock()
        mock_snapshot.symbol = "AAPL"
        mock_snapshot.daily_bar = mock_daily_bar

        mock_snapshots = {"AAPL": mock_snapshot}
        mock_historical_instance.get_stock_snapshot.return_value = mock_snapshots

        alpaca_client = AlpacaClient("test-key", "test-secret", True)  # noqa: FBT003

        current_date = date(2024, 1, 15)
        result = alpaca_client.fetch_latest_data(current_date)

        assert isinstance(result, pl.DataFrame)
        assert len(result) == 0
