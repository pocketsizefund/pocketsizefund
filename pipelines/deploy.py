from pipelines import train_model

train_model.pipeline.serve(
    name="basic-lstm-trainer",
    tags=["prediction", "ml"],
    parameters={
        "data_path": "data/test_data.csv",
        "timestamp_field": "timestamp",
        "ticker_field": "ticker",
        "features": [
            "open_price",
            "high_price",
            "low_price",
            "close_price",
            "volume",
        ],
        "close_price_index": 3,
        "train_test_splits": (0.7, 0.2, 0.1),
    },
    interval=60 * 60 * 24,  # daily for now to test
)
