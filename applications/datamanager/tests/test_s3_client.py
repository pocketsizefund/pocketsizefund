from datetime import date
from unittest.mock import MagicMock, patch

import pandas as pd
import polars as pl
import pytest
from datamanager.s3_client import S3Client


def test_s3_client_write_equity_bars_data() -> None:
    with (
        patch("boto3.Session") as mock_session,
        patch("duckdb.connect") as mock_duckdb_connect,
        patch("polars.DataFrame.write_parquet") as mock_write_parquet,
    ):
        mock_session.return_value.region_name = "us-east-1"
        mock_duckdb_conn = MagicMock()
        mock_duckdb_connect.return_value = mock_duckdb_conn

        s3_client = S3Client("test-bucket")

        test_data = pl.DataFrame(
            {
                "ticker": ["AAPL", "GOOGL"],
                "timestamp": [
                    1640995200000,
                    1641081600000,
                ],  # 2022-01-01, 2022-01-02 in ms
                "open_price": [150.0, 2800.0],
                "high_price": [155.0, 2850.0],
                "low_price": [149.0, 2790.0],
                "close_price": [153.0, 2820.0],
                "volume": [1000000, 500000],
                "volume_weighted_average_price": [152.5, 2815.0],
            }
        )

        s3_client.write_equity_bars_data(test_data)

        mock_write_parquet.assert_called_once()
        call_args = mock_write_parquet.call_args
        assert call_args[1]["file"] == "s3://test-bucket/equity/bars/"
        assert call_args[1]["partition_by"] == ["year", "month", "day"]


def test_s3_client_read_equity_bars_data() -> None:
    with (
        patch("boto3.Session") as mock_session,
        patch("duckdb.connect") as mock_duckdb_connect,
    ):
        mock_session.return_value.region_name = "us-east-1"
        mock_duckdb_conn = MagicMock()
        mock_duckdb_connect.return_value = mock_duckdb_conn

        mock_result = MagicMock()
        mock_result.fetchdf.return_value = pd.DataFrame(
            {
                "ticker": ["AAPL", "GOOGL"],
                "timestamp": [1640995200000, 1641081600000],
                "open_price": [150.0, 2800.0],
                "high_price": [155.0, 2850.0],
                "low_price": [149.0, 2790.0],
                "close_price": [153.0, 2820.0],
                "volume": [1000000, 500000],
                "volume_weighted_average_price": [152.5, 2815.0],
                "year": [2022, 2022],
                "month": [1, 1],
                "day": [1, 2],
            }
        )

        mock_duckdb_conn.execute.return_value = mock_result

        s3_client = S3Client("test-bucket")

        start_date = date(2022, 1, 1)
        end_date = date(2022, 1, 2)
        result = s3_client.read_equity_bars_data(start_date, end_date)

        assert mock_duckdb_conn.execute.call_count == 2  # noqa: PLR2004

        assert isinstance(result, pl.DataFrame)


def test_s3_client_write_equity_bars_data_empty_dataframe() -> None:
    with (
        patch("boto3.Session") as mock_session,
        patch("duckdb.connect") as mock_duckdb_connect,
    ):
        mock_session.return_value.region_name = "us-east-1"
        mock_duckdb_conn = MagicMock()
        mock_duckdb_connect.return_value = mock_duckdb_conn

        s3_client = S3Client("test-bucket")

        empty_data = pl.DataFrame()

        with pytest.raises(
            pl.exceptions.ColumnNotFoundError, match='unable to find column "ticker"'
        ):
            s3_client.write_equity_bars_data(empty_data)
