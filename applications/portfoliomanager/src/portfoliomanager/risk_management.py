import math
from datetime import UTC, datetime

import polars as pl


def add_positions_action_column(
    positions: pl.DataFrame,
    current_datetime: datetime,
) -> pl.DataFrame:
    positions = positions.clone()

    return positions.with_columns(
        pl.when(
            pl.col("timestamp")
            .cast(pl.Float64)
            .map_elements(
                lambda ts: datetime.fromtimestamp(ts, tz=UTC).date(),
                return_dtype=pl.Date,
            )
            == current_datetime.date()
        )
        .then(pl.lit("PDT_LOCKED"))
        .otherwise(pl.lit("UNSPECIFIED"))
        .alias("action")
    )


def add_equity_bars_returns_and_realized_volatility_columns(
    equity_bars: pl.DataFrame,
) -> pl.DataFrame:
    equity_bars = equity_bars.clone()

    minimum_bars_per_ticker_required = 30

    ticker_counts = equity_bars.group_by("ticker").agg(pl.len().alias("count"))
    insufficient_tickers = ticker_counts.filter(
        pl.col("count") < minimum_bars_per_ticker_required
    )

    if insufficient_tickers.height > 0:
        insufficient_list = insufficient_tickers.select("ticker").to_series().to_list()
        message = f"Tickers with insufficient data (< {minimum_bars_per_ticker_required} rows): {insufficient_list}"  # noqa: E501
        raise ValueError(message)

    equity_bars = equity_bars.sort(["ticker", "timestamp"])
    daily_returns = pl.col("close_price").pct_change().over("ticker")
    return equity_bars.with_columns(
        pl.when(pl.col("close_price").is_not_null())
        .then(daily_returns)
        .otherwise(None)
        .alias("daily_returns"),
        pl.when(pl.col("close_price").is_not_null())
        .then(
            pl.when((daily_returns + 1) > 0)
            .then((daily_returns + 1).log())
            .otherwise(None)
        )
        .otherwise(None)
        .alias("log_daily_returns"),
        daily_returns.rolling_std(window_size=minimum_bars_per_ticker_required).alias(
            "realized_volatility"
        ),
    )


def add_positions_performance_columns(
    positions: pl.DataFrame,
    original_predictions: pl.DataFrame,  # per original position ticker and timestamp
    original_equity_bars: pl.DataFrame,  # per original position ticker and timestamp
    current_timestamp: datetime,
) -> pl.DataFrame:
    positions = positions.clone()
    original_predictions = original_predictions.clone()
    original_equity_bars = original_equity_bars.clone()

    position_predictions = positions.join(
        other=original_predictions,
        on=["ticker", "timestamp"],
        how="left",
    ).select(
        pl.col("ticker"),
        pl.col("timestamp"),
        pl.col("side"),
        pl.col("dollar_amount"),
        pl.col("action"),
        pl.col("quantile_10").alias("original_lower_threshold"),
        pl.col("quantile_90").alias("original_upper_threshold"),
    )

    original_equity_bars_with_returns = original_equity_bars.sort(
        ["ticker", "timestamp"]
    )

    position_returns = []

    for row in position_predictions.iter_rows(named=True):
        ticker = row["ticker"]
        position_timestamp = row["timestamp"]

        ticker_bars = original_equity_bars_with_returns.filter(
            (pl.col("ticker") == ticker)
            & (pl.col("timestamp") >= position_timestamp)
            & (pl.col("timestamp") <= current_timestamp.timestamp())
        )

        cumulative_log_return = (
            ticker_bars.select(pl.col("log_daily_returns").sum()).item() or 0
        )

        cumulative_simple_return = math.exp(cumulative_log_return) - 1

        position_returns.append(
            {
                "ticker": ticker,
                "timestamp": position_timestamp,
                "cumulative_simple_return": cumulative_simple_return,
            }
        )

    returns = pl.DataFrame(position_returns)

    positions_with_data = position_predictions.join(
        other=returns,
        on=["ticker", "timestamp"],
        how="left",
    )

    return positions_with_data.with_columns(
        pl.when(pl.col("action") == "PDT_LOCKED")
        .then(pl.lit("PDT_LOCKED"))
        .when(
            (pl.col("action") != "PDT_LOCKED")
            & (
                (
                    (pl.col("side") == "LONG")
                    & (
                        pl.col("cumulative_simple_return")
                        <= pl.col("original_lower_threshold")
                    )
                )
                | (
                    (pl.col("side") == "SHORT")
                    & (
                        pl.col("cumulative_simple_return")
                        >= pl.col("original_upper_threshold")
                    )
                )
            )
        )
        .then(pl.lit("CLOSE_POSITION"))
        .when(
            (
                (pl.col("side") == "LONG")
                & (
                    pl.col("cumulative_simple_return")
                    >= pl.col("original_upper_threshold")
                )
            )
            | (
                (pl.col("side") == "SHORT")
                & (
                    pl.col("cumulative_simple_return")
                    <= pl.col("original_lower_threshold")
                )
            )
        )
        .then(pl.lit("MAINTAIN_POSITION"))
        .otherwise(pl.lit("UNSPECIFIED"))
        .alias("action")
    ).drop(
        [
            "original_lower_threshold",
            "original_upper_threshold",
            "cumulative_simple_return",
        ]
    )


def add_predictions_zscore_ranked_columns(predictions: pl.DataFrame) -> pl.DataFrame:
    predictions = predictions.clone()

    quantile_50_mean = predictions.select(pl.col("quantile_50").mean()).item()
    quantile_50_standard_deviation = (
        predictions.select(pl.col("quantile_50").std()).item() or 1e-8
    )

    z_score_return = (
        pl.col("quantile_50") - quantile_50_mean
    ) / quantile_50_standard_deviation

    inter_quartile_range = pl.col("quantile_90") - pl.col("quantile_10")

    composite_score = z_score_return / (1 + inter_quartile_range)

    return predictions.with_columns(
        z_score_return.alias("z_score_return"),
        inter_quartile_range.alias("inter_quartile_range"),
        composite_score.alias("composite_score"),
        pl.lit("UNSPECIFIED").alias("action"),
    ).sort(["composite_score", "inter_quartile_range"], descending=[True, False])


def create_optimal_portfolio(
    predictions: pl.DataFrame,
    positions: pl.DataFrame,
    maximum_capital: float,
    current_timestamp: datetime,
) -> pl.DataFrame:
    predictions = predictions.clone()
    positions = positions.clone()

    minimum_inter_quartile_range = 0.75
    high_uncertainty_tickers = (
        predictions.filter(
            pl.col("inter_quartile_range") > minimum_inter_quartile_range
        )
        .select("ticker")
        .to_series()
        .to_list()
    )

    closed_positions, maintained_positions = _filter_positions(positions)

    closed_position_tickers = closed_positions.select("ticker").to_series().to_list()
    maintained_position_tickers = (
        maintained_positions.select("ticker").to_series().to_list()
    )

    excluded_tickers = (
        high_uncertainty_tickers + closed_position_tickers + maintained_position_tickers
    )

    available_predictions = predictions.filter(
        ~pl.col("ticker").is_in(excluded_tickers)
    )

    maintained_long_capital = _filter_side_capital_amount(maintained_positions, "LONG")
    maintained_short_capital = _filter_side_capital_amount(
        maintained_positions, "SHORT"
    )
    closed_long_capital = _filter_side_capital_amount(closed_positions, "LONG")
    closed_short_capital = _filter_side_capital_amount(closed_positions, "SHORT")

    target_side_capital = maximum_capital / 2
    available_long_capital = max(
        0.0,
        target_side_capital - maintained_long_capital + closed_long_capital,
    )
    available_short_capital = max(
        0.0,
        target_side_capital - maintained_short_capital + closed_short_capital,
    )

    maintained_long_count = maintained_positions.filter(pl.col("side") == "LONG").height
    maintained_short_count = maintained_positions.filter(
        pl.col("side") == "SHORT"
    ).height

    new_long_positions_needed = max(0, 10 - maintained_long_count)
    new_short_positions_needed = max(0, 10 - maintained_short_count)

    total_available = available_predictions.height
    maximum_long_candidates = min(new_long_positions_needed, total_available // 2)
    maximum_short_candidates = min(
        new_short_positions_needed, total_available - maximum_long_candidates
    )

    long_candidates = available_predictions.head(maximum_long_candidates)
    short_candidates = available_predictions.tail(maximum_short_candidates)

    dollar_amount_per_long = (
        available_long_capital / maximum_long_candidates
        if maximum_long_candidates > 0
        else 0
    )
    dollar_amount_per_short = (
        available_short_capital / maximum_short_candidates
        if maximum_short_candidates > 0
        else 0
    )

    long_positions = long_candidates.select(
        pl.col("ticker"),
        pl.lit(current_timestamp.timestamp()).cast(pl.Float64).alias("timestamp"),
        pl.lit("LONG").alias("side"),
        pl.lit(dollar_amount_per_long).alias("dollar_amount"),
        pl.lit("UNSPECIFIED").alias("action"),
    )

    short_positions = short_candidates.select(
        pl.col("ticker"),
        pl.lit(current_timestamp.timestamp()).cast(pl.Float64).alias("timestamp"),
        pl.lit("SHORT").alias("side"),
        pl.lit(dollar_amount_per_short).alias("dollar_amount"),
        pl.lit("UNSPECIFIED").alias("action"),
    )

    return _collect_portfolio_positions(
        long_positions,
        short_positions,
        maintained_positions,
    )


def _filter_positions(positions: pl.DataFrame) -> tuple[pl.DataFrame, pl.DataFrame]:
    positions = positions.clone()

    if positions.height == 0:
        return (
            pl.DataFrame(
                {
                    "ticker": [],
                    "timestamp": [],
                    "side": [],
                    "dollar_amount": [],
                    "action": [],
                }
            ),
            pl.DataFrame(
                {
                    "ticker": [],
                    "timestamp": [],
                    "side": [],
                    "dollar_amount": [],
                    "action": [],
                }
            ),
        )

    closed_positions = positions.filter(pl.col("action") == "CLOSE_POSITION")
    maintained_positions = positions.filter(pl.col("action") == "MAINTAIN_POSITION")

    return closed_positions, maintained_positions


def _filter_side_capital_amount(positions: pl.DataFrame, side: str) -> float:
    positions = positions.clone()

    filtered_positions = positions.filter(pl.col("side") == side.upper())

    if filtered_positions.height == 0:
        return 0.0

    try:
        side_capital_amount = filtered_positions.select(pl.sum("dollar_amount")).item()
        return float(side_capital_amount or 0)

    except Exception:  # noqa: BLE001
        return 0.0


def _collect_portfolio_positions(
    long_positions: pl.DataFrame,
    short_positions: pl.DataFrame,
    maintained_positions: pl.DataFrame,
) -> pl.DataFrame:
    long_positions = long_positions.clone()
    short_positions = short_positions.clone()
    maintained_positions = maintained_positions.clone()

    portfolio_components = []

    if long_positions.height > 0:
        portfolio_components.append(long_positions)
    if short_positions.height > 0:
        portfolio_components.append(short_positions)
    if maintained_positions.height > 0:
        portfolio_components.append(
            maintained_positions.with_columns(pl.col("timestamp").cast(pl.Float64))
        )

    if len(portfolio_components) == 0:
        message = "No portfolio components to create an optimal portfolio."
        raise ValueError(message)

    optimal_portfolio = pl.concat(portfolio_components)

    return optimal_portfolio.select(
        "ticker",
        pl.col("timestamp").cast(pl.Float64),
        "side",
        "dollar_amount",
    ).sort(["ticker", "side"])
