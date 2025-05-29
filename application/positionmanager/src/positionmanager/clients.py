from typing import Any, Dict

import polars as pl
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
            msg = "Alpaca API key and secret are required"
            raise ValueError(msg)

        self.trading_client = TradingClient(api_key, api_secret, paper=paper)

    def get_cash_balance(self) -> Money:
        account = self.trading_client.get_account()
        return Money.from_float(float(account.cash))

    def place_notional_order(
        self,
        ticker: str,
        notional_amount: Money,
    ) -> Dict[str, Any]:
        market_order_request = MarketOrderRequest(
            symbol=ticker,
            notional=float(notional_amount),
            side=OrderSide.BUY,
            time_in_force=TimeInForce.DAY,
        )

        self.trading_client.submit_order(market_order_request)

        return {
            "status": "success",
            "message": f"Order placed for {ticker} with notional amount {notional_amount}",
        }

    def clear_positions(self) -> Dict[str, Any]:
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
            msg = "Data manager URL is not configured"
            raise ValueError(msg)

        endpoint = f"{self.datamanager_base_url}/equity-bars"

        try:
            response = requests.post(endpoint, json=date_range.to_payload(), timeout=10)
        except requests.RequestException as err:
            msg = f"Data manager service call error: {err}"
            raise RuntimeError(msg) from err

        if response.status_code != 200:
            msg = (
                f"Data service error: {response.text}, status code: {response.status_code}",
            )
            raise Exception(msg)

        response_data = response.json()

        data = pl.DataFrame(response_data["data"])

        data = data.with_columns(
            pl.col("timestamp")
            .str.slice(0, 10)
            .str.strptime(pl.Date, "%Y-%m-%d")
            .alias("date"),
        )

        return (
            data.sort("date")
            .pivot(index="date", columns="ticker", values="close_price")
            .with_columns(pl.all().exclude("date").cast(pl.Float64))
        )
