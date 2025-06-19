from typing import TYPE_CHECKING, Any, cast

import polars as pl
import pyarrow as pa
import requests
from alpaca.trading.client import TradingClient
from alpaca.trading.enums import OrderSide, TimeInForce
from alpaca.trading.requests import MarketOrderRequest

from .models import DateRange, Money

if TYPE_CHECKING:
    from alpaca.trading.models import Position, TradeAccount


class AlpacaClient:
    def __init__(
        self,
        *,
        api_key: str | None = None,
        api_secret: str | None = None,
        paper: bool = True,
    ) -> None:
        if not api_key or not api_secret:
            message = "Alpaca API key and secret are required"
            raise ValueError(message)

        self.trading_client: TradingClient = TradingClient(
            api_key,
            api_secret,
            paper=paper,
            raw_data=False,
        )

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

    def get_account_information(self) -> dict[str, Any]:
        account: TradeAccount = cast("TradeAccount", self.trading_client.get_account())
        return {
            "portfolio_value": float(account.portfolio_value or 0),
            "cash": float(account.cash or 0),
            "buying_power": float(account.buying_power or 0),
            "equity": float(account.equity or 0),
        }

    def get_positions(self) -> list[dict[str, Any]]:
        positions: list[Position] = cast(
            "list[Position]",
            self.trading_client.get_all_positions(),
        )
        position_list = []

        for position in positions:
            position_data = {
                "symbol": position.symbol,
                "quantity": float(position.qty or 0),
                "market_value": float(position.market_value or 0),
                "cost_basis": float(position.cost_basis or 0),
                "unrealized_profit_and_loss": float(position.unrealized_pl or 0),
                "unrealized_profit_and_loss_percent": float(
                    position.unrealized_plpc or 0
                ),
                "current_price": float(position.current_price or 0),
                "side": position.side.value,
            }
            position_list.append(position_data)

        return position_list


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

        if response.status_code == requests.codes["no_content"]:
            return pl.DataFrame()
        if response.status_code != requests.codes["ok"]:
            message = f"Data service error: {response.text}, status code: {response.status_code}"  # noqa: E501
            raise requests.HTTPError(
                message,
                response=response,
            )

        buffer = pa.py_buffer(response.content)
        reader = pa.ipc.RecordBatchStreamReader(buffer)
        table = reader.read_all()

        data = pl.DataFrame(pl.from_arrow(table))

        data = data.with_columns(
            pl.col("datetime").cast(pl.Datetime).dt.date().alias("date")
        )

        return (
            data.sort("date")
            .pivot(on="T", index="date", values="c")
            .with_columns(pl.all().exclude("date").cast(pl.Float64))
        )
