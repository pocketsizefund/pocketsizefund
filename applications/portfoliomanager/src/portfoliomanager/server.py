import io
import os
from datetime import UTC, datetime
from typing import cast

import polars as pl
import requests
from fastapi import FastAPI, Response, status
from internal.equity_bars_schema import equity_bars_schema

from .alpaca_client import AlpacaClient
from .enums import PositionAction, PositionSide, TradeSide
from .portfolio_schema import portfolio_schema
from .risk_management import (
    add_equity_bars_returns_and_realized_volatility_columns,
    add_portfolio_action_column,
    add_portfolio_performance_columns,
    add_predictions_zscore_ranked_columns,
    create_optimal_portfolio,
)

application: FastAPI = FastAPI()

DATAMANAGER_BASE_URL = os.getenv("PSF_DATAMANAGER_BASE_URL", "http://datamanager:8080")
EQUITYPRICEMODEL_BASE_URL = os.getenv(
    "PSF_EQUITYPRICEMODEL_BASE_URL",
    "http://equitypricemodel:8080",
)

alpaca_client = AlpacaClient(
    api_key=os.getenv("ALPACA_API_KEY", ""),
    api_secret=os.getenv("ALPACA_API_SECRET", ""),
    is_paper=os.getenv("ALPACA_IS_PAPER", "true").lower() == "true",
)


@application.get("/health")
def health_check() -> Response:
    return Response(status_code=status.HTTP_200_OK)


@application.get("/portfolio")
def create_portfolio() -> Response:
    current_timestamp = datetime.now(tz=UTC)

    account = alpaca_client.get_account()

    current_predictions = get_current_predictions()

    prior_portfolio = get_prior_portfolio(current_timestamp=current_timestamp)

    optimal_portfolio = get_optimal_portfolio(
        current_predictions=current_predictions,
        prior_portfolio=prior_portfolio,
        maximum_capital=float(account.cash_amount),
        current_timestamp=current_timestamp,
    )

    open_positions, close_positions = get_positions(
        prior_portfolio=prior_portfolio,
        optimal_portfolio=optimal_portfolio,
    )

    for close_position in close_positions:
        alpaca_client.close_position(
            ticker=close_position["ticker"],
        )

    for open_position in open_positions:
        alpaca_client.open_position(
            ticker=open_position["ticker"],
            side=open_position["side"],
            dollar_amount=open_position["dollar_amount"],
        )

    return Response(status_code=status.HTTP_200_OK)


def get_current_predictions() -> pl.DataFrame:
    current_predictions_response = requests.get(
        url=f"{EQUITYPRICEMODEL_BASE_URL}/predictions",
        timeout=60,
    )

    current_predictions_response.raise_for_status()

    current_predictions = pl.DataFrame(current_predictions_response.json())

    return add_predictions_zscore_ranked_columns(
        current_predictions=current_predictions
    )


def get_prior_portfolio(current_timestamp: datetime) -> pl.DataFrame:  # TEMP
    prior_portfolio_response = requests.get(
        url=f"{DATAMANAGER_BASE_URL}/portfolios",
        timeout=60,
    )

    prior_portfolio_response.raise_for_status()

    prior_portfolio = pl.DataFrame(prior_portfolio_response.json())

    if prior_portfolio.is_empty():
        return pl.DataFrame(
            {
                "ticker": [],
                "timestamp": [],
                "side": [],
                "dollar_amount": [],
                "action": [],
            }
        )

    tickers = prior_portfolio["ticker"].unique().to_list()
    timestamps = prior_portfolio["timestamp"].cast(pl.Float64)
    start_timestamp = datetime.fromtimestamp(cast("float", timestamps.min()), tz=UTC)
    current_timestamp = datetime.now(tz=UTC)

    prior_equity_bars_response = requests.get(
        url=f"{DATAMANAGER_BASE_URL}/equity-bars",
        params={
            "tickers": ",".join(tickers),
            "start_timestamp": start_timestamp.isoformat(),
            "end_timestamp": current_timestamp.isoformat(),
        },
        timeout=60,
    )

    prior_equity_bars_response.raise_for_status()

    tickers_and_timestamps = [
        {"ticker": row[0], "timestamp": row[1]}
        for row in prior_portfolio[["ticker", "timestamp"]].rows()
    ]

    prior_predictions_response = requests.get(
        url=f"{DATAMANAGER_BASE_URL}/predictions",
        params={"tickers_and_timestamps": str(tickers_and_timestamps)},
        timeout=60,
    )

    prior_predictions_response.raise_for_status()

    prior_portfolio = add_portfolio_action_column(
        prior_portfolio=prior_portfolio,
        current_timestamp=current_timestamp,
    )

    prior_equity_bars = pl.read_parquet(io.BytesIO(prior_equity_bars_response.content))

    prior_equity_bars = equity_bars_schema.validate(prior_equity_bars)

    prior_equity_bars = add_equity_bars_returns_and_realized_volatility_columns(
        prior_equity_bars=prior_equity_bars
    )

    prior_predictions = pl.DataFrame(prior_predictions_response.json())

    return add_portfolio_performance_columns(
        prior_portfolio=prior_portfolio,
        prior_equity_bars=prior_equity_bars,
        prior_predictions=prior_predictions,
        current_timestamp=current_timestamp,
    )


def get_optimal_portfolio(
    current_predictions: pl.DataFrame,
    prior_portfolio: pl.DataFrame,
    maximum_capital: float,
    current_timestamp: datetime,
) -> pl.DataFrame:
    optimal_portfolio = create_optimal_portfolio(
        current_predictions=current_predictions,
        prior_portfolio=prior_portfolio,
        maximum_capital=maximum_capital,
        current_timestamp=current_timestamp,
    )

    optimal_portfolio = portfolio_schema.validate(optimal_portfolio)

    save_portfolio_response = requests.post(
        url=f"{DATAMANAGER_BASE_URL}/portfolios",
        json={
            "timestamp": current_timestamp.isoformat(),
            "data": optimal_portfolio.to_dicts(),
        },
        timeout=60,
    )

    save_portfolio_response.raise_for_status()

    return optimal_portfolio


def get_positions(
    prior_portfolio: pl.DataFrame,
    optimal_portfolio: pl.DataFrame,
) -> tuple[list[dict], list[dict]]:
    prior_portfolio = prior_portfolio.clone()
    optimal_portfolio = optimal_portfolio.clone()

    close_positions = []
    if not prior_portfolio.is_empty():
        prior_tickers = set(prior_portfolio["ticker"].to_list())
        optimal_tickers = set(optimal_portfolio["ticker"].to_list())
        closing_tickers = prior_tickers - optimal_tickers

        if closing_tickers:
            close_positions = [
                {
                    "ticker": row["ticker"],
                    "side": (
                        TradeSide.BUY.value
                        if row["side"] == PositionSide.LONG.value
                        else TradeSide.SELL.value
                    ),
                    "dollar_amount": row["dollar_amount"],
                }
                for row in prior_portfolio.filter(
                    pl.col("ticker").is_in(list(closing_tickers))
                ).iter_rows(named=True)
            ]

    open_positions = [
        {
            "ticker": row["ticker"],
            "side": (
                TradeSide.BUY.value
                if row["side"] == PositionSide.LONG.value
                else TradeSide.SELL.value
            ),
            "dollar_amount": row["dollar_amount"],
        }
        for row in optimal_portfolio.filter(
            pl.col("action") == PositionAction.OPEN_POSITION.value
        ).iter_rows(named=True)
    ]

    return open_positions, close_positions
