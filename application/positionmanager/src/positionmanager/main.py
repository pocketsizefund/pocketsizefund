import json
import os
from datetime import datetime, timedelta
from zoneinfo import ZoneInfo

import polars as pl
import requests
from alpaca.common.exceptions import APIError
from cloudevents.pydantic.v2 import CloudEvent
from fastapi import FastAPI, HTTPException, Response, status
from prometheus_client import Gauge
from prometheus_fastapi_instrumentator import Instrumentator
from pydantic import ValidationError

from .clients import AlpacaClient, DataClient
from .models import DateRange, Money
from .portfolio import PortfolioOptimizer

trading_days_per_year = 252


application = FastAPI()
Instrumentator().instrument(application).expose(application)

portfolio_value_gauge = Gauge(
    "portfolio_total_value",
    "Current total portfolio value from Alpaca",
)

portfolio_cash_balance_gauge = Gauge(
    "portfolio_cash_balance",
    "Current cash balance in portfolio",
)

portfolio_positions_count_gauge = Gauge(
    "portfolio_positions_count",
    "Number of current positions in portfolio",
)

portfolio_position_value_gauge = Gauge(
    "portfolio_position_value",
    "Value of specific position",
    ["symbol"],
)

portfolio_position_profit_and_loss_gauge = Gauge(
    "portfolio_position_profit_and_loss",
    "Unrealized P&L for specific position",
    ["symbol"],
)


@application.get("/health")
def get_health() -> Response:
    return Response(status_code=status.HTTP_200_OK)


@application.get("/metrics")
def update_metrics() -> Response:
    alpaca_client = AlpacaClient(
        api_key=os.getenv("ALPACA_API_KEY", ""),
        api_secret=os.getenv("ALPACA_API_SECRET", ""),
        paper=os.getenv("ALPACA_PAPER", "true").lower() == "true",
    )

    try:
        account_information = alpaca_client.get_account_information()
        positions = alpaca_client.get_positions()

        portfolio_value_gauge.set(account_information["portfolio_value"])
        portfolio_cash_balance_gauge.set(account_information["cash"])
        portfolio_positions_count_gauge.set(len(positions))

        position_metrics = []
        for position in positions:
            symbol = position["symbol"]
            portfolio_position_value_gauge.labels(symbol=symbol).set(
                position["market_value"]
            )
            portfolio_position_profit_and_loss_gauge.labels(symbol=symbol).set(
                position["unrealized_profit_and_loss"]
            )

            position_metrics.append(
                {
                    "symbol": symbol,
                    "market_value": position["market_value"],
                    "unrealized_profit_and_loss": position[
                        "unrealized_profit_and_loss"
                    ],
                    "cost_basis": position["cost_basis"],
                    "current_price": position["current_price"],
                }
            )

        return Response(
            status_code=status.HTTP_200_OK,
            content=json.dumps(
                {
                    "portfolio_value": [account_information["portfolio_value"]],
                    "cash_balance": [account_information["cash"]],
                    "positions_count": [len(positions)],
                    "positions": position_metrics,
                }
            ),
            media_type="application/json",
        )

    except (requests.RequestException, APIError, ValidationError) as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error updating metrics: {e!r}",
        ) from e


@application.post("/positions/open", response_model=None)
def open_position(event: CloudEvent) -> CloudEvent:
    alpaca_client = AlpacaClient(
        api_key=os.getenv("ALPACA_API_KEY", ""),
        api_secret=os.getenv("ALPACA_API_SECRET", ""),
        paper=os.getenv("ALPACA_PAPER", "true").lower() == "true",
    )

    data_client = DataClient(datamanager_base_url=os.getenv("DATAMANAGER_BASE_URL", ""))

    portfolio_optimizer = PortfolioOptimizer(
        minimum_portfolio_tickers=int(os.getenv("MINIMUM_PORTFOLIO_TICKERS", "5")),
        maximum_portfolio_tickers=int(os.getenv("MAXIMUM_PORTFOLIO_TICKERS", "20")),
    )

    try:
        cash_balance = alpaca_client.get_cash_balance()

    except (requests.RequestException, APIError, ValidationError) as e:
        return create_cloud_event_error(f"Error opening position: {e!r}")

    eastern_timezone = ZoneInfo("America/New_York")

    date_range = DateRange(
        start=datetime.now(tz=eastern_timezone) - timedelta(days=trading_days_per_year),
        end=datetime.now(tz=eastern_timezone),
    )

    try:
        historical_data = data_client.get_data(date_range=date_range)

    except (requests.RequestException, APIError, ValidationError) as e:
        return create_cloud_event_error(f"Error fetching historical data: {e!r}")

    try:
        event_data = event.data
        if not isinstance(event_data, dict):
            return create_cloud_event_error(
                "Invalid event data format, expected a dictionary."
            )

        predictions: dict[str, dict[str, list[float]]] = event_data.get(
            "predictions", {}
        )

        predictions_percentile_50 = {
            key: value["percentile_50"] for key, value in predictions.items()
        }

        optimized_portfolio = portfolio_optimizer.get_optimized_portfolio(
            historical_data=historical_data,
            portfolio_value=cash_balance,
            predictions=predictions_percentile_50,
        )

    except (requests.RequestException, APIError, ValidationError) as e:
        return create_cloud_event_error(f"Error optimizing portfolio: {e!r}")

    executed_trades = []
    for ticker, share_count in optimized_portfolio.items():
        if share_count <= 0:
            continue

        latest_prices = historical_data.filter(pl.col(ticker).is_not_null()).select(
            ticker,
        )
        if latest_prices.is_empty():
            executed_trades.append(
                {
                    "ticker": ticker,
                    "status": "error",
                    "error": "No recent price available",
                },
            )
            continue

        latest_price = latest_prices.tail(1)[0, 0]

        notional_amount = Money.from_float(
            latest_price * share_count * 0.95,
        )  # 5% buffer

        try:
            alpaca_client.place_notional_order(ticker, notional_amount)

            executed_trades.append(
                {
                    "ticker": ticker,
                    "share_count": share_count,
                    "notional_amount": float(notional_amount),
                    "status": "success",
                },
            )

        except (requests.RequestException, APIError, ValidationError) as e:
            executed_trades.append(
                {
                    "ticker": ticker,
                    "share_count": share_count,
                    "notional_amount": float(notional_amount),
                    "status": "error",
                    "error": str(e),
                },
            )

    final_cash_balance = alpaca_client.get_cash_balance()

    return CloudEvent(
        attributes={
            "source": "positionmanager",
            "type": "application.positionmanager.positions.opened",
        },
        data={
            "date": date_range.end.isoformat(),
            "initial_cash_balance": float(cash_balance),
            "final_cash_balance": float(final_cash_balance),
            "optimized_portfolio": optimized_portfolio,
            "executed_trades": executed_trades,
            "time_period": date_range.to_payload(),
        },
    )


@application.post("/positions/close", response_model=None)
def close_positions() -> CloudEvent:
    alpaca_client = AlpacaClient(
        api_key=os.getenv("ALPACA_API_KEY", ""),
        api_secret=os.getenv("ALPACA_API_SECRET", ""),
        paper=os.getenv("ALPACA_PAPER", "true").lower() == "true",
    )

    try:
        result = alpaca_client.clear_positions()

    except (requests.RequestException, APIError, ValidationError) as e:
        return create_cloud_event_error(str(e))

    cash_balance = alpaca_client.get_cash_balance()

    return CloudEvent(
        attributes={
            "source": "positionmanager",
            "type": "application.positionmanager.positions.closed",
        },
        data={
            "date": datetime.now(tz=ZoneInfo("America/New_York")).isoformat(),
            "status": result["status"],
            "message": result["message"],
            "cash_balance": float(cash_balance),
        },
    )


def create_cloud_event_error(error_message: str) -> CloudEvent:
    return CloudEvent(
        attributes={
            "source": "positionmanager",
            "type": "application.positionmanager.positions.errored",
        },
        data={
            "date": datetime.now(tz=ZoneInfo("America/New_York")).isoformat(),
            "error": error_message,
        },
    )
