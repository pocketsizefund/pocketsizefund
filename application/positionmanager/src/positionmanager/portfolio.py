from typing import Dict
import polars as pl
import pandas as pd
from pypfopt import EfficientFrontier, risk_models, expected_returns
from pypfopt.discrete_allocation import DiscreteAllocation, get_latest_prices

from .models import Money


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
        portfolio_value: Money,
        predictions: Dict[str, float],
        prediction_weight: float = 0.3,
    ) -> Dict[str, int]:
        converted_data = data.to_pandas()

        if "date" in converted_data.columns:
            converted_data = converted_data.set_index("date")

        mu = expected_returns.mean_historical_return(converted_data, frequency=252)

        prediction_series = pd.Series(predictions).reindex(mu.index).dropna()
        for ticker in prediction_series.index:
            mu[ticker] = (1 - prediction_weight) * mu[
                ticker
            ] + prediction_weight * prediction_series[ticker]

        S = risk_models.CovarianceShrinkage(converted_data).ledoit_wolf()

        long_only_weight_bounds = (0, 0.2)  # 20% max weight per asset
        efficient_frontier = EfficientFrontier(
            mu, S, weight_bounds=long_only_weight_bounds
        )

        efficient_frontier.max_sharpe(risk_free_rate=0.02)  # 2% risk-free rate
        weights = efficient_frontier.clean_weights()

        latest_prices = get_latest_prices(converted_data)

        discrete_allocation = DiscreteAllocation(
            weights=weights,
            latest_prices=latest_prices,
            total_portfolio_value=int(float(portfolio_value)),
        )

        optimized_portfolio_ticker_share_counts, _ = (
            discrete_allocation.greedy_portfolio()
        )

        return optimized_portfolio_ticker_share_counts
