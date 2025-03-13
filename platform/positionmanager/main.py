from typing import List, Dict, Any, Tuple
import polars as pl
from fastapi import FastAPI
import os
from datetime import datetime
import requests
from library.events import build_response_event
from library.trade import Client

app = FastAPI()


@app.get("/portfolio")
async def portfolio_handler():
    environment = os.getenv("ENVIRONMENT", "development").lower()
    model_count = int(os.getenv("MODEL_COUNT"))
    confidence_level = float(os.getenv("CONFIDENCE_LEVEL"))
    risk_threshold = float(os.getenv("RISK_THRESHOLD"))

    trade_client = Client(
        alpaca_api_key=os.getenv("ALPACA_API_KEY"),
        alpaca_secret_key=os.getenv("ALPACA_SECRET_KEY"),
        is_paper=environment == "development",
    )

    current_timestamp = datetime.now().date()

    current_date = current_timestamp.date()

    data_provider_url = f"http://dataprovider.{environment}.svc.cluster.local:8080/predictions"

    payload = {"date": current_date}

    response = requests.post(data_provider_url, params=payload).json()

    try:
        model_predictions, calibration_data = parse_response(response, model_count)
    except ValueError as e:
        return build_response_event(
            "positionmanager",
            ["no_action"],
            {"message": e},
        )

    model_weights = [1 / model_count] * model_count

    combined_predictions = combine_predictions(model_predictions, model_weights)

    calibration_scores = calibrate_nonconformity_scores(calibration_data, combined_predictions)

    prediction_intervals = calculate_prediction_intervals(
        combined_predictions,
        calibration_scores,
        confidence_level,
    )

    account = trade_client.get_account()

    portfolio = select_portfolio(
        prediction_intervals,
        account["cash_amount"]
        + account["equity_amount"],  # assuming closing all positions and no short positions
        risk_threshold,
    )

    portfolio.with_columns(
        [
            pl.lit(current_timestamp).alias("timestamp"),
        ]
    )

    return build_response_event(
        "positionmanager",
        ["equities", "portfolio", "updated"],
        {"portfolio": portfolio.to_dicts()},
    )


def parse_response(
    response: List[List[Dict[str, Any]]],
    model_count: int,
) -> Tuple[List[pl.DataFrame], List[pl.DataFrame]]:
    if len(response) < model_count:
        raise ValueError("Not enough predictions available")

    model_predictions: List[pl.DataFrame] = []
    calibration_data: List[pl.DataFrame] = []
    for response_entity in response:
        prediction = pl.DataFrame(response_entity)

        prediction = prediction.with_columns(
            [pl.col("timestamp").str.strptime(pl.Datetime, format="%Y-%m-%dT%H:%M:%S")]
        )

        actual_prices = prediction.select(["ticker", "timestamp", "mean_prices"])
        calibration_data.append(actual_prices)

        model_prediction = prediction.select(
            [
                "ticker",
                "timestamp",
                "mean_prices",
                "lower_prices",
                "upper_prices",
            ]
        )
        model_predictions.append(model_prediction)

    return model_predictions, calibration_data


def combine_predictions(
    model_predictions: List[pl.DataFrame],
    model_weights: List[float],
) -> pl.DataFrame:
    if len(model_predictions) != len(model_weights):
        raise ValueError("Number of predictions and weights must be equal")
    if sum(model_weights) != 1.0:
        raise ValueError("Model weights must sum to 1.0")

    combined = (
        model_predictions[0]
        .select("ticker")
        .with_columns(
            [
                pl.lit(0.0).alias("predicted_mean"),
                pl.lit(0.0).alias("lower_bound"),
                pl.lit(0.0).alias("upper_bound"),
            ]
        )
    )

    for model_prediction, weight in zip(model_predictions, model_weights):
        combined = (
            combined.join(model_prediction, on="ticker", how="inner")
            .with_columns(
                [
                    (pl.col("predicted_mean") + pl.col("predicted_mean_right") * weight).alias(
                        "predicted_mean"
                    ),
                    (pl.col("lower_bound") + pl.col("lower_bound_right") * weight).alias(
                        "lower_bound"
                    ),
                    (pl.col("upper_bound") + pl.col("upper_bound_right") * weight).alias(
                        "upper_bound"
                    ),
                ]
            )
            .drop(["predicted_mean_right", "lower_bound_right", "upper_bound_right"])
        )

    return combined


def calibrate_nonconformity_scores(
    calibration_data: pl.DataFrame,
    model_predictions: pl.DataFrame,
) -> pl.DataFrame:
    calibrated = calibration_data.join(model_predictions, on="ticker", how="inner").with_columns(
        [(pl.col("actual_price") - pl.col("predicted_mean")).abs().alias("nonconformity_score")]
    )

    return calibrated.select(["ticker", "nonconformity_score"])


def calculate_prediction_intervals(
    combined_predictions: pl.DataFrame,
    calibration_scores: pl.DataFrame,
    confidence_level: float,
) -> pl.DataFrame:
    q = 1 - confidence_level
    quantile_score = calibration_scores.select("nonconformity_score").quantile(q).item()

    intervals = combined_predictions.with_columns(
        [
            (pl.col("predicted_mean") - quantile_score).alias("lower_bound"),
            (pl.col("predicted_mean") + quantile_score).alias("upper_bound"),
        ]
    )

    return intervals.select(["ticker", "predicted_mean", "lower_bound", "upper_bound"])


def select_portfolio(
    prediction_intervals: pl.DataFrame,
    budget: float,
    risk_threshold: float,
) -> pl.DataFrame:
    portfolio = prediction_intervals.with_columns(
        ((pl.col("upper_bound") - pl.col("lower_bound")) / pl.col("predicted_mean")).alias(
            "risk_percentage"
        )
    )

    portfolio = portfolio.filter(pl.col("risk_percentage") <= risk_threshold)

    portfolio = portfolio.with_columns(
        [
            (pl.lit(1.0) / pl.col("risk_percentage")).alias("investment_weight"),
        ]
    )

    portfolio = portfolio.with_columns(
        [
            (pl.col("investment_weight") / pl.col("investment_weight").sum()).alias("allocation"),
        ]
    )

    portfolio = portfolio.with_columns((pl.col("allocation") * budget).alias("investment_amount"))

    return portfolio.select(
        [
            "ticker",
            "predicted_mean",
            "allocation",
            "investment_amount",
        ]
    )
