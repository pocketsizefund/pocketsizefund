import polars as pl
from positionmanager.main import (
    parse_response,
    combine_predictions,
    calibrate_nonconformity_scores,
    calculate_prediction_intervals,
    select_portfolio,
)
from datetime import datetime


def test_parse_response():
    response = [
        [
            {
                "ticker": "AAPL",
                "timestamp": "2021-08-01T00:00:00",
                "actual_price": 150.0,
                "predicted_mean": 155.0,
                "lower_bound": 145.0,
                "upper_bound": 165.0,
            },
            {
                "ticker": "GOOGL",
                "timestamp": "2021-08-01T00:00:00",
                "actual_price": 2750.0,
                "predicted_mean": 2780.0,
                "lower_bound": 2770.0,
                "upper_bound": 2790.0,
            },
            {
                "ticker": "MSFT",
                "timestamp": "2021-08-01T00:00:00",
                "actual_price": 310.0,
                "predicted_mean": 300.0,
                "lower_bound": 290.0,
                "upper_bound": 310.0,
            },
        ]
    ]

    model_count = 1

    expected = [
        pl.DataFrame(
            {
                "ticker": ["AAPL", "GOOGL", "MSFT"],
                "timestamp": [
                    datetime(2021, 8, 1, 0, 0, 0),
                    datetime(2021, 8, 1, 0, 0, 0),
                    datetime(2021, 8, 1, 0, 0, 0),
                ],
                "predicted_mean": [155.0, 2780.0, 300.0],
                "lower_bound": [145.0, 2770.0, 290.0],
                "upper_bound": [165.0, 2790.0, 310.0],
            }
        )
    ]

    model_predictions, _ = parse_response(response, model_count)

    assert model_predictions[0].equals(
        expected[0]
    ), "Parsed response does not match expected values."


def test_combine_predictions():
    model_outputs = [
        pl.DataFrame(
            {
                "ticker": ["AAPL", "GOOG", "MSFT"],
                "predicted_mean": [150, 2800, 300],
                "lower_bound": [140, 2700, 290],
                "upper_bound": [160, 2900, 310],
            }
        ),
        pl.DataFrame(
            {
                "ticker": ["AAPL", "GOOG", "MSFT"],
                "predicted_mean": [155, 2850, 310],
                "lower_bound": [145, 2750, 300],
                "upper_bound": [165, 2950, 320],
            }
        ),
    ]
    model_weights = [0.6, 0.4]

    expected = pl.DataFrame(
        {
            "ticker": ["AAPL", "GOOG", "MSFT"],
            "predicted_mean": [152.0, 2820.0, 304.0],
            "lower_bound": [142.0, 2720.0, 294.0],
            "upper_bound": [162.0, 2920.0, 314.0],
        }
    )

    received = combine_predictions(model_outputs, model_weights)

    assert received.equals(expected)


def test_calibrate_nonconformity_scores():
    calibration_data = pl.DataFrame(
        {"ticker": ["AAPL", "GOOGL", "MSFT"], "actual_price": [150.0, 2750.0, 310.0]}
    )

    model_predictions = pl.DataFrame(
        {"ticker": ["AAPL", "GOOGL", "MSFT"], "predicted_mean": [155.0, 2780.0, 300.0]}
    )

    expected = pl.DataFrame(
        {"ticker": ["AAPL", "GOOGL", "MSFT"], "nonconformity_score": [5.0, 30.0, 10.0]}
    )

    received = calibrate_nonconformity_scores(calibration_data, model_predictions)

    assert received.equals(expected), "Nonconformity scores do not match expected values."


def test_calculate_prediction_intervals():
    combined_predictions = pl.DataFrame(
        {"ticker": ["AAPL", "GOOGL", "MSFT"], "predicted_mean": [155.0, 2780.0, 300.0]}
    )

    calibration_scores = pl.DataFrame({"nonconformity_score": [5.0, 10.0, 8.0]})

    confidence_level = 0.9  # 90% confidence level

    expected = pl.DataFrame(
        {
            "ticker": ["AAPL", "GOOGL", "MSFT"],
            "predicted_mean": [155.0, 2780.0, 300.0],
            "lower_bound": [150.0, 2775.0, 295.0],
            "upper_bound": [160.0, 2785.0, 305.0],
        }
    )

    received = calculate_prediction_intervals(
        combined_predictions,
        calibration_scores,
        confidence_level,
    )

    assert received.equals(expected), "Prediction intervals do not match expected values."


def test_select_portfolio():
    prediction_intervals = pl.DataFrame(
        {
            "ticker": ["AAPL", "GOOGL", "MSFT", "TSLA"],
            "predicted_mean": [155.0, 2780.0, 300.0, 700.0],
            "lower_bound": [150.0, 2770.0, 290.0, 680.0],
            "upper_bound": [160.0, 2790.0, 310.0, 720.0],
        }
    )

    budget = 1000.0
    risk_threshold = 0.06  # 6% risk threshold

    expected = pl.DataFrame(
        {
            "ticker": ["GOOGL", "TSLA"],
            "predicted_mean": [2780.0, 700.0],
            "allocation": [0.888179, 0.111821],
            "investment_amount": [888.178914, 111.821086],
        }
    )

    received = select_portfolio(prediction_intervals, budget, risk_threshold)

    assert received.shape == expected.shape, "Portfolio selection shapes do not match."

    googl_received = (
        received.filter(pl.col("ticker") == "GOOGL").select("allocation").to_numpy()[0][0]
    )
    googl_expected = (
        expected.filter(pl.col("ticker") == "GOOGL").select("allocation").to_numpy()[0][0]
    )

    tolerance = 1e-4

    assert (
        abs(googl_received - googl_expected) < tolerance
    ), "Allocation values for GOOGL are approximately unequal."
