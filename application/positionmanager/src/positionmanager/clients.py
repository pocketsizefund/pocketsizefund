from typing import Any

import polars as pl
import pyarrow as pa
import requests
from alpaca.trading.client import TradingClient
from alpaca.trading.enums import OrderSide, TimeInForce
from alpaca.trading.requests import MarketOrderRequest

from .models import DateRange, Money


class AlpacaClient:
    def __init__(
        self,
        *,
        api_key: str | None = "",
        api_secret: str | None = "",
        paper: bool = True,
    ) -> None:
        if not api_key or not api_secret:
            message = "Alpaca API key and secret are required"
            raise ValueError(message)

        self.trading_client = TradingClient(api_key, api_secret, paper=paper)

    def get_cash_balance(self) -> Money:
        account = self.trading_client.get_account()
        cash_balance = getattr(account, "cash", None)

        if cash_balance is None:
            message = "Cash balance is not available"
            raise ValueError(message)

        return Money.from_float(float(cash_balance))

    def place_notional_order(
        self,
        ticker: str,
        notional_amount: Money,
    ) -> dict[str, Any]:
        market_order_request = MarketOrderRequest(
            symbol=ticker,
            notional=float(notional_amount),
            side=OrderSide.BUY,
            time_in_force=TimeInForce.DAY,
        )

        self.trading_client.submit_order(market_order_request)

        return {
            "status": "success",
            "message": f"Order placed [{ticker=}, {notional_amount}]",
        }

    def clear_positions(self) -> dict[str, Any]:
        self.trading_client.close_all_positions(cancel_orders=True)

        return {
            "status": "success",
            "message": "All positions have been closed",
        }


class DataClient:
    def __init__(self, datamanager_base_url: str | None) -> None:
        self.datamanager_base_url = datamanager_base_url

    def get_data(
        self,
        date_range: DateRange,
    ) -> pl.DataFrame:
        if not self.datamanager_base_url:
            message = "Data manager URL is not configured"
            raise ValueError(message)

        endpoint = f"{self.datamanager_base_url}/equity-bars"

        params = {
            "start_date": date_range.start.date().isoformat(),
            "end_date": date_range.end.date().isoformat(),
        }

        try:
            response = requests.get(endpoint, params=params, timeout=30)
        except requests.RequestException as err:
            message = f"Data manager service call error: {err}"
            raise RuntimeError(message) from err

        if response.status_code == requests.codes.not_found:
            return pl.DataFrame()
        if response.status_code != requests.codes.ok:
            message = f"Data service error: {response.text}, status code: {response.status_code}"  # noqa: E501
            raise requests.HTTPError(
                message,
            )

        buffer = pa.py_buffer(response.content)
        reader = pa.ipc.RecordBatchStreamReader(buffer)
        table = reader.read_all()

        data = pl.DataFrame(pl.from_arrow(table))

        data = data.with_columns(pl.col("t").cast(pl.Datetime).dt.date().alias("date"))

        return (
            data.sort("date")
            .pivot(on="T", index="date", values="c")
            .with_columns(pl.all().exclude("date").cast(pl.Float64))
        )
