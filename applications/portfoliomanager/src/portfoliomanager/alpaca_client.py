import time
from typing import cast

import pandera.polars as pa
import polars as pl
from alpaca.trading import Position, TradeAccount, TradingClient


class AlpacaAccount:
    def __init__(
        self,
        cash_amount: float,
        positions: pl.DataFrame,
    ) -> None:
        self.cash_amount = cash_amount

        position_schema.validate(positions)

        self.positions = positions


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

        account_positions: list[Position] = cast(
            "list[Position]", self.trading_client.get_all_positions()
        )
        if not account_positions:
            time.sleep(self.rate_limit_sleep)
            empty_positions = pl.DataFrame(
                {
                    "ticker": pl.Series([], dtype=pl.String),
                    "side": pl.Series([], dtype=pl.String),
                    "dollar_amount": pl.Series([], dtype=pl.Float64),
                    "share_amount": pl.Series([], dtype=pl.Float64),
                }
            )

            return AlpacaAccount(
                cash_amount=float(cast("str", account.cash)),
                positions=empty_positions,
            )

        position_data = [
            {
                "ticker": account_position.symbol,
                "side": str(account_position.side).replace("PositionSide.", "").upper(),
                "dollar_amount": float(cast("str", account_position.market_value)),
                "share_amount": float(cast("str", account_position.qty)),
            }
            for account_position in account_positions
        ]

        time.sleep(self.rate_limit_sleep)

        positions = pl.DataFrame(position_data)

        position_schema.validate(positions)

        return AlpacaAccount(
            cash_amount=float(cast("str", account.cash)),
            positions=positions,
        )


def is_uppercase(data: pa.PolarsData) -> pl.LazyFrame:
    return data.lazyframe.select(
        pl.col(data.key).str.to_uppercase() == pl.col(data.key)
    )


position_schema = pa.DataFrameSchema(
    {
        "ticker": pa.Column(
            dtype=str,
            checks=[pa.Check(is_uppercase)],
        ),
        "side": pa.Column(
            dtype=str,
            checks=[
                pa.Check.isin(["LONG", "SHORT"]),
                pa.Check(is_uppercase),
            ],
        ),
        "dollar_amount": pa.Column(
            dtype=float,
            checks=[pa.Check.greater_than(0)],
        ),
        "share_amount": pa.Column(
            dtype=float,
            checks=[pa.Check.greater_than(0)],
        ),
    },
)
