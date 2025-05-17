from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from typing import Dict, Any
import os
import requests
from datetime import datetime, timedelta
import polars as pl
from alpaca.trading.client import TradingClient
from alpaca.trading.requests import MarketOrderRequest
from alpaca.trading.enums import OrderSide, TimeInForce
from pypfopt import EfficientFrontier, risk_models, expected_returns
from pypfopt.discrete_allocation import DiscreteAllocation, get_latest_prices


class PredictionPayload(BaseModel):
    predictions: Dict[str, Any]


class DateRange(BaseModel):
    start_date: datetime
    end_date: datetime


class AlpacaClient:
    def __init__(
        self,
        api_key: str = "",
        api_secret: str = "",
        paper: bool = True,
    ) -> None:
        if not api_key or not api_secret:
            raise ValueError("Alpaca API key and secret are required")

        self.trading_client = TradingClient(api_key, api_secret, paper=paper)

    def get_cash_balance(self) -> float:
        account = self.trading_client.get_account()
        return float(account.cash)

    def place_notional_order(
        self,
        ticker: str,
        notional_amount: float,
    ) -> Dict[str, Any]:
        market_order_request = MarketOrderRequest(
            symbol=ticker,
            notional=notional_amount,
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
    def __init__(self, datamanager_base_url: str):
        self.datamanager_base_url = datamanager_base_url

    def get_data(
        self,
        start_date: datetime,
        end_date: datetime,
    ) -> pl.DataFrame:
        if not self.datamanager_base_url:
            raise ValueError("Data manager URL is not configured")

        endpoint = f"{self.datamanager_base_url}/equity-bars"

        payload = {
            "start_date": start_date.isoformat(),
            "end_date": end_date.isoformat(),
        }

        response = requests.post(endpoint, json=payload)

        if response.status_code != 200:
            raise Exception(
                f"Data service error: {response.text}, status code: {response.status_code}",
            )

        response_data = response.json()

        if "data" not in response_data:
            raise ValueError("Invalid data format returned from data service")

        data = pl.DataFrame(response_data["data"])

        data = data.with_columns(
            pl.col("timestamp")
            .str.slice(0, 10)
            .str.strptime(pl.Date, "%Y-%m-%d")
            .alias("date")
        )

        if "ticker" not in data.columns or "close_price" not in data.columns:
            raise ValueError("Data must contain ticker and close_price columns")

        data = (
            data.sort("date")
            .pivot(index="date", columns="ticker", values="close_price")
            .with_columns(pl.all().cast(pl.Float64))
        )

        return data


class PortfolioOptimizer:
    def __init__(
        self,
        minimum_portfolio_tickers: int = 5,
        maximum_portfolio_tickers: int = 20,
    ):
        self.minimum_portfolio_tickers = minimum_portfolio_tickers
        self.maximum_portfolio_tickers = maximum_portfolio_tickers

    def get_optimized_portfolio(
        self,
        data: pl.DataFrame,
        portfolio_value: int,
    ) -> Dict[str, float]:
        converted_data = data.to_pandas()

        if "date" in converted_data.columns:
            converted_data = converted_data.set_index("date")

        mu = expected_returns.mean_historical_return(converted_data, frequency=252)

        S = risk_models.CovarianceShrinkage(converted_data).ledoit_wolf()

        long_only_weight_bounds = (0, 1)
        efficient_frontier = EfficientFrontier(
            mu, S, weight_bounds=long_only_weight_bounds
        )

        efficient_frontier.max_sharpe(risk_free_rate=0.02)  # 2% risk-free rate
        weights = efficient_frontier.clean_weights()

        latest_prices = get_latest_prices(converted_data)

        discrete_allocation = DiscreteAllocation(
            weights, latest_prices, total_portfolio_value=portfolio_value
        )

        optimized_portfolio, _ = discrete_allocation.greedy_portfolio()

        return optimized_portfolio


trading_days_per_year = 252

application = FastAPI()


def get_alpaca_client():
    return AlpacaClient(
        api_key=os.getenv("ALPACA_API_KEY"),
        api_secret=os.getenv("ALPACA_API_SECRET"),
        paper=os.getenv("ALPACA_PAPER", "true").lower() == "true",
    )


def get_data_client():
    return DataClient(datamanager_base_url=os.getenv("DATAMANAGER_BASE_URL"))


def get_portfolio_optimizer():
    return PortfolioOptimizer(
        minimum_portfolio_tickers=int(os.getenv("MINIMUM_PORTFOLIO_TICKERS", "5")),
        maximum_portfolio_tickers=int(os.getenv("MAXIMUM_PORTFOLIO_TICKERS", "20")),
    )


@application.get("/health")
def get_health():
    return {"status": "healthy"}


@application.post("/positions")
def create_position(payload: PredictionPayload) -> Dict[str, Any]:
    alpaca_client = get_alpaca_client()
    data_client = get_data_client()
    portfolio_optimizer = get_portfolio_optimizer()

    try:
        cash_balance = alpaca_client.get_cash_balance()

    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error getting cash balance: {str(e)}",
        )

    end_date = datetime.now()
    start_date = end_date - timedelta(days=trading_days_per_year)

    try:
        historical_data = data_client.get_data(start_date, end_date)

    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error getting historical data: {str(e)}",
        )

    try:
        optimized_portfolio = portfolio_optimizer.get_optimized_portfolio(
            historical_data, int(cash_balance)
        )

    except Exception as e:
        raise HTTPException(
            status_code=500,
            detail=f"Error optimizing portfolio: {str(e)}",
        )

    executed_trades = []
    for ticker, weight in optimized_portfolio.items():
        if weight <= 0:
            continue

        notional_amount = cash_balance * weight * 0.95  # 5% cash buffer

        try:
            alpaca_client.place_notional_order(ticker, notional_amount)

            executed_trades.append(
                {
                    "ticker": ticker,
                    "notional_amount": notional_amount,
                    "status": "success",
                }
            )

        except Exception as e:
            executed_trades.append(
                {
                    "ticker": ticker,
                    "notional_amount": notional_amount,
                    "status": "error",
                    "error": str(e),
                }
            )

    final_cash_balance = alpaca_client.get_cash_balance()

    return {
        "status": "success",
        "initial_cash_balance": cash_balance,
        "final_cash_balance": final_cash_balance,
        "optimized_portfolio": optimized_portfolio,
        "executed_trades": executed_trades,
        "time_period": {"start_date": start_date, "end_date": end_date},
    }


@application.delete("/positions")
def delete_positions() -> Dict[str, Any]:
    alpaca_client = get_alpaca_client()

    try:
        result = alpaca_client.clear_positions()

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

    cash_balance = alpaca_client.get_cash_balance()

    return {
        "status": result["status"],
        "message": result["message"],
        "cash_balance": cash_balance,
    }
