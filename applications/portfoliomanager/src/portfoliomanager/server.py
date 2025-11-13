import os
from datetime import UTC, datetime, timedelta

import polars as pl
import requests
from fastapi import FastAPI, Response, status

from .alpaca_client import AlpacaClient
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
    end_date = datetime.now(tz=UTC)
    start_date = end_date - timedelta(
        days=30
    )  # required minimum lookback period for risk management calculations

    prior_equity_bars_response = requests.get(
        url=f"{DATAMANAGER_BASE_URL}/equity-bars",
        params={
            "start_date": start_date.isoformat(),
            "end_date": end_date.isoformat(),
        },
        timeout=60,
    )

    prior_equity_bars_response.raise_for_status()

    prior_portfolio_response = requests.get(
        url=f"{DATAMANAGER_BASE_URL}/portfolio",
        timeout=60,
    )

    prior_portfolio_response.raise_for_status()

    prior_predictions_response = requests.get(
        url=f"{DATAMANAGER_BASE_URL}/predictions",
        timeout=60,
    )

    prior_predictions_response.raise_for_status()

    account = alpaca_client.get_account()

    current_predictions_response = requests.get(
        url=f"{EQUITYPRICEMODEL_BASE_URL}/predictions",
        timeout=60,
    )

    current_predictions_response.raise_for_status()

    prior_portfolio = pl.DataFrame(prior_portfolio_response.json())

    prior_portfolio = add_portfolio_action_column(
        prior_portfolio=prior_portfolio,
        current_timestamp=end_date,
    )

    prior_equity_bars = pl.DataFrame(prior_equity_bars_response.json())

    prior_equity_bars = add_equity_bars_returns_and_realized_volatility_columns(
        prior_equity_bars=prior_equity_bars
    )

    prior_predictions = pl.DataFrame(prior_predictions_response.json())

    prior_portfolio = add_portfolio_performance_columns(
        prior_portfolio=prior_portfolio,
        prior_equity_bars=prior_equity_bars,
        prior_predictions=prior_predictions,
        current_timestamp=end_date,
    )

    current_predictions = pl.DataFrame(current_predictions_response.json())

    current_predictions = add_predictions_zscore_ranked_columns(
        current_predictions=current_predictions
    )

    optimal_portfolio = create_optimal_portfolio(
        current_predictions=current_predictions,
        prior_portfolio=prior_portfolio,
        maximum_capital=float(account.cash_amount),
        current_timestamp=end_date,
    )

    optimal_portfolio = portfolio_schema.validate(optimal_portfolio)

    # outline:
    # [x] get equity price model url
    # [x] get data manager url
    # [x] import risk management functions
    # [x] import alpaca client
    # [x] call data manager to get risk management data
    # [x] call equity price model to get predictions
    # [x] call per-ticker prediction extraction logic
    # [x] call risk management functions to create portfolio
    # [ ] call alpaca client to create portfolio

    return Response(status_code=status.HTTP_200_OK)  # TEMP
