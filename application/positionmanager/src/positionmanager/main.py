from fastapi import FastAPI, HTTPException
import requests
import os
from datetime import datetime, timedelta
import polars as pl
from typing import Dict, Any

from .models import Money, DateRange, PredictionPayload
from .clients import AlpacaClient, DataClient
from .portfolio import PortfolioOptimizer

from prometheus_fastapi_instrumentator import Instrumentator

from alpaca.common.rest import APIError
from pydantic import ValidationError


trading_days_per_year = 252

application = FastAPI()
Instrumentator().instrument(application).expose(application)


@application.get("/health")
def get_health() -> dict[str, str]:
    return {"status": "healthy"}


@application.post("/positions")
def create_position(payload: PredictionPayload) -> Dict[str, Any]:
    alpaca_client = AlpacaClient(
        api_key=os.getenv("ALPACA_API_KEY"),
        api_secret=os.getenv("ALPACA_API_SECRET"),
        paper=os.getenv("ALPACA_PAPER", "true").lower() == "true",
    )

    data_client = DataClient(datamanager_base_url=os.getenv("DATAMANAGER_BASE_URL"))

    portfolio_optimizer = PortfolioOptimizer(
        minimum_portfolio_tickers=int(os.getenv("MINIMUM_PORTFOLIO_TICKERS", "5")),
        maximum_portfolio_tickers=int(os.getenv("MAXIMUM_PORTFOLIO_TICKERS", "20")),
    )

    try:
        cash_balance = alpaca_client.get_cash_balance()

    except (requests.RequestException, APIError, ValidationError) as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error getting cash balance: {str(e)}",
        ) from e

    date_range = DateRange(
        start=datetime.now() - timedelta(days=trading_days_per_year),
        end=datetime.now(),
    )

    try:
        historical_data = data_client.get_data(date_range=date_range)

    except (requests.RequestException, APIError, ValidationError) as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error getting historical data: {str(e)}",
        ) from e

    try:
        optimized_portfolio = portfolio_optimizer.get_optimized_portfolio(
            data=historical_data,
            portfolio_value=cash_balance,
            predictions=payload.predictions,
        )

    except (requests.RequestException, APIError, ValidationError) as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error optimizing portfolio: {str(e)}",
        ) from e

    executed_trades = []
    for ticker, share_count in optimized_portfolio.items():
        if share_count <= 0:
            continue

        latest_prices = historical_data.filter(pl.col(ticker).is_not_null()).select(
            ticker
        )
        if latest_prices.is_empty():
            executed_trades.append(
                {
                    "ticker": ticker,
                    "status": "error",
                    "error": "No recent price available",
                }
            )
            continue
        latest_price = latest_prices.tail(1)[0, 0]

        notional_amount = Money.from_float(
            latest_price * share_count * 0.95
        )  # 5% buffer

        try:
            alpaca_client.place_notional_order(ticker, notional_amount)

            executed_trades.append(
                {
                    "ticker": ticker,
                    "share_count": share_count,
                    "notional_amount": float(notional_amount),
                    "status": "success",
                }
            )

        except (requests.RequestException, APIError, ValidationError) as e:
            executed_trades.append(
                {
                    "ticker": ticker,
                    "share_count": share_count,
                    "notional_amount": float(notional_amount),
                    "status": "error",
                    "error": str(e),
                }
            )

    final_cash_balance = alpaca_client.get_cash_balance()

    return {
        "status": "success",
        "initial_cash_balance": float(cash_balance),
        "final_cash_balance": float(final_cash_balance),
        "optimized_portfolio": optimized_portfolio,
        "executed_trades": executed_trades,
        "time_period": date_range.to_payload(),
    }


@application.delete("/positions")
def delete_positions() -> Dict[str, Any]:
    alpaca_client = AlpacaClient(
        api_key=os.getenv("ALPACA_API_KEY"),
        api_secret=os.getenv("ALPACA_API_SECRET"),
        paper=os.getenv("ALPACA_PAPER", "true").lower() == "true",
    )

    try:
        result = alpaca_client.clear_positions()

    except (requests.RequestException, APIError, ValidationError) as e:
        raise HTTPException(status_code=500, detail=str(e)) from e

    cash_balance = alpaca_client.get_cash_balance()

    return {
        "status": result["status"],
        "message": result["message"],
        "cash_balance": float(cash_balance),
    }
