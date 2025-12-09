import time
from typing import cast

from alpaca.trading import (
    ClosePositionRequest,
    OrderRequest,
    TradeAccount,
    TradingClient,
)
from alpaca.trading.enums import OrderSide, OrderType, TimeInForce

from .enums import TradeSide


class AlpacaAccount:
    def __init__(
        self,
        cash_amount: float,
    ) -> None:
        self.cash_amount = cash_amount


class AlpacaClient:
    def __init__(
        self,
        api_key: str,
        api_secret: str,
        is_paper: bool,  # noqa: FBT001
    ) -> None:
        self.rate_limit_sleep = 0.5  # seconds

        self.trading_client = TradingClient(
            api_key=api_key,
            secret_key=api_secret,
            paper=is_paper,
        )

    def get_account(self) -> AlpacaAccount:
        account: TradeAccount = cast("TradeAccount", self.trading_client.get_account())

        time.sleep(self.rate_limit_sleep)

        return AlpacaAccount(
            cash_amount=float(cast("str", account.cash)),
        )

    def open_position(
        self,
        ticker: str,
        side: TradeSide,
        dollar_amount: float,
    ) -> None:
        self.trading_client.submit_order(
            order_data=OrderRequest(
                symbol=ticker.upper(),
                notional=dollar_amount,
                side=OrderSide(side.value.lower()),
                type=OrderType.MARKET,
                time_in_force=TimeInForce.DAY,  # required for notional trades
            ),
        )

        time.sleep(self.rate_limit_sleep)

    def close_position(
        self,
        ticker: str,
    ) -> None:
        self.trading_client.close_position(
            symbol_or_asset_id=ticker.upper(),
            close_options=ClosePositionRequest(
                percentage="100",
            ),
        )

        time.sleep(self.rate_limit_sleep)
