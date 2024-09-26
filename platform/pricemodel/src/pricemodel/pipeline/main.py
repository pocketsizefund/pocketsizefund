import polars as pl
import numpy as np
from datetime import datetime, timedelta, timezone
import pickle
from tempfile import TemporaryDirectory
import os
import mlflow
from pathlib import Path
import mlflow.pyfunc
from category_encoders import OrdinalEncoder
from rich.console import Console
from rich import pretty
import sys
from types import SimpleNamespace


from pricemodel.pipeline.trainer import Trainer
from pricemodel.pipeline.experiment import Experiment

console = Console()
pretty.install()

MIN_OBSERVATIONS_PER_TICKER = 100
FRESHNESS_DATE = '2023-12-31'



def prepare_data(data_path, training_cutoff: str) -> pl.DataFrame:
    console.print("Starting data preparation...", style="cyan")
    console.print("Ensuring proper data types...", style="cyan")

    data = pl.scan_csv(data_path).sort(["ticker", "timestamp"]).with_columns([
        pl.col("timestamp").str.to_datetime().cast(pl.Date),
        pl.col("open").cast(pl.Float64),
        pl.col("high").cast(pl.Float64),
        pl.col("low").cast(pl.Float64),
        pl.col("close").cast(pl.Float64),
        pl.col("volume").cast(pl.Float64),
        pl.col("ticker").cast(pl.Utf8),
    ])

    console.print("Dropping duplicate rows...", style="cyan")
    data = data.unique(subset=["ticker", "timestamp"])
    
    console.print("Filtering tickers with insufficient data...", style="cyan")
    ticker_counts = data.group_by("ticker").agg(pl.count().alias("count"))
    valid_tickers = ticker_counts.collect().filter(
        pl.col("count") >= MIN_OBSERVATIONS_PER_TICKER
    )["ticker"]
    data = data.filter(pl.col("ticker").is_in(valid_tickers))
    
    assert data.collect().height > 0, "No data left after filtering for minimum observations."
    
    console.print(f"Filtering tickers without data after {FRESHNESS_DATE}...", style="cyan")
    freshness_dt = datetime.strptime(FRESHNESS_DATE, "%Y-%m-%d")

    valid_tickers = (
            data
            .group_by("ticker")
            .agg(pl.col("timestamp")
                 .max()
                 .alias("latest_date"))
    ).collect().filter(
        pl.col("latest_date") >= freshness_dt
    )["ticker"]

    data = data.filter(pl.col("ticker").is_in(valid_tickers))

    
    assert data.collect().height > 0, "No data left after filtering for freshness date."
    
    tickers = data.select("ticker").unique()
    min_date = data.select(pl.col("timestamp").min()).collect()[0, 0]
    max_date = data.select(pl.col("timestamp").max()).collect()[0, 0]

    
    console.print(f"min_date: {min_date}, type: {type(min_date)}", style="cyan")
    console.print(f"max_date: {max_date}, type: {type(max_date)}", style="cyan")
    
    console.print("Generating full date range and performing cross join.", style="cyan")
    full_dates = pl.DataFrame({
        "timestamp": pl.date_range(
            start=min_date,
            end=max_date,
            interval="1d",
            closed="both",
            eager=True
        )
    }).lazy()

    
    full_df = tickers.join(full_dates, how="cross")
    
    full_df = full_df.with_columns([
        pl.col("timestamp").rank(method="dense").cast(pl.Int32).alias("time_idx") - 1  # Start from 0
    ])
    
    console.print("Joining full dataframe with original data...", style="cyan")
    data = full_df.join(data, on=["ticker", "timestamp"], how="left")

    
    console.print("Filling missing numeric values using interpolation...", style="cyan")
    numeric_cols = ["open", "high", "low", "close", "volume"]
    

    data = (
        data
        .sort(["ticker", "timestamp"])
        .group_by("ticker")
        .agg([
            pl.all().exclude("ticker")
            .interpolate()
            .fill_null(strategy="forward")
            .fill_null(strategy="backward"),
            (pl.col("time_idx") - pl.col("time_idx").min()).alias("relative_time_idx")
        ])
        .explode(pl.exclude("ticker"))
    )

    for col in numeric_cols:
        missing_count = data.collect().select(pl.col(col).is_null().sum())[0, 0]
        assert missing_count == 0, f"Missing values remain in column {col}."
    
    ticker_counts = data.group_by("ticker").agg(pl.len()).collect()
    assert ticker_counts["len"].is_duplicated().all(), "Not all tickers have the same number of observations."
    
    console.print("Computing date features...", style="cyan")
    data = data.with_columns([
        pl.col("timestamp").dt.weekday().alias("day_of_week"),  # Monday=0, Sunday=6
        (pl.col("timestamp").dt.weekday() >= 5).alias("is_weekend"),
        pl.col("timestamp").dt.week().alias("week_of_year"),
        pl.col("timestamp").dt.month().alias("month_of_year"),
        ((pl.col("timestamp").dt.day() - 1) // 7 + 1).alias("week_of_month"),
    ])

    
    data = data.with_columns([
        (pl.col("day_of_week") / 6).alias("day_of_week"),
        (pl.col("week_of_year") / 52).alias("week_of_year"),
        (pl.col("month_of_year") / 12).alias("month_of_year"),
        (pl.col("week_of_month") / 4).alias("week_of_month"),
    ])

    

    
    console.print("Encoding 'ticker' using OrdinalEncoder...", style="cyan")
    ticker_df = data.select("ticker").collect().to_pandas()
    
    encoder = OrdinalEncoder(
        cols=["ticker"],
        handle_unknown='value',
        handle_missing='value',
    )
    
    encoder.fit(ticker_df)
    
    ticker_encoded = encoder.transform(ticker_df)["ticker"].values
    data = data.with_columns([
        pl.Series(name="ticker_encoded", values=ticker_encoded)
    ])
    
    console.print("Computing static real variables per ticker...", style="cyan")
    static_real_features = data.group_by("ticker_encoded").agg([
        pl.col("open").mean().alias("open_center"),
        pl.col("open").std().alias("open_scale"),
        pl.col("high").mean().alias("high_center"),
        pl.col("high").std().alias("high_scale"),
        pl.col("low").mean().alias("low_center"),
        pl.col("low").std().alias("low_scale"),
        pl.col("close").mean().alias("close_center"),
        pl.col("close").std().alias("close_scale"),
    ])

    
    console.print("Handling cases where standard deviation is zero...", style="cyan")
    static_real_features = static_real_features.with_columns([
        pl.when(pl.col(col) == 0).then(1e-8).otherwise(pl.col(col)).alias(col) for col in [
            "open_scale", "high_scale", "low_scale", "close_scale"
        ]
    ])
    
    static_features = static_real_features.collect().to_pandas()
    
    console.print("Joining static features back to data...", style="cyan")
    data = data.join(static_real_features, on="ticker_encoded", how="left")

    
    console.print("Normalizing real-valued features...", style="cyan")
    data = data.with_columns([
        ((pl.col("open") - pl.col("open_center")) / pl.col("open_scale")).alias("open"),
        ((pl.col("high") - pl.col("high_center")) / pl.col("high_scale")).alias("high"),
        ((pl.col("low") - pl.col("low_center")) / pl.col("low_scale")).alias("low"),
        ((pl.col("close") - pl.col("close_center")) / pl.col("close_scale")).alias("close"),
    ])

    normalized_cols = ["open", "high", "low", "close"]
    for col in normalized_cols:
        missing_count = data.select(pl.col(col).is_null().sum()).collect()[0, 0]
        assert missing_count == 0, f"Missing values in normalized column {col}."
    
    console.print("Data preparation completed successfully.", style="cyan")

    

    console.print("Splitting data into training and validation sets...", style="cyan")
    
    
    training_cutoff = datetime.strptime(training_cutoff, "%Y-%m-%d")
    
    training_data = data.filter(pl.col("timestamp") < training_cutoff)
    validation_data = data.filter(pl.col("timestamp") >= training_cutoff)

    training_dates = set(training_data.select("timestamp").unique().collect()["timestamp"].to_list())
    validation_dates = set(validation_data.select("timestamp").unique().collect()["timestamp"].to_list())
    overlap = training_dates.intersection(validation_dates)
    
    assert len(overlap) == 0, "Training and validation sets have overlapping dates."
    
    console.print("Data splitting completed successfully.", style="cyan")
    
    return SimpleNamespace(sets=SimpleNamespace(training=training_data, validation=validation_data),
                           artifacts=SimpleNamespace(static_features=static_features, encoder=encoder))


class TimeSeriesDataset(mlflow.pyfunc.PythonModel):
    def __init__(self, data):
        self.datasets = data.sets
        self.artifacts = data.artifacts
        self.features = [
            "ticker_encoded", "relative_time_idx", "time_idx",
            "open", "high", "low", "close",
            "day_of_week", "week_of_month", "month_of_year", "week_of_year",
            "is_weekend"
        ]
        self.target = ["open", "high", "low", "close"]
        missing_cols = [col for col in self.features + self.target if col not in self.datasets.training.collect_schema().names()]
        if missing_cols:
            raise ValueError(f"Missing columns in data: {missing_cols}")
    
    def load_artifacts(self, context):
        pass
    
    def prepare_inference_data(self, tickers, start_date, end_date):
        console.print("Preparing inference data...", style="cyan")
        date_range = pl.DataFrame({
            "timestamp": pl.date_range(
                start=start_date,
                end=end_date,
                interval="1d",
                closed="both",
                eager=True  # Ensure eager evaluation
            )
        })
        # Create DataFrame with all combinations of tickers and dates
        tickers_df = pl.DataFrame({"ticker": tickers})
        inference_df = tickers_df.join(date_range, how="cross")
        
        # Assign time_idx based on existing time_idx
        # Assuming that the time_idx continues from the training data
        last_time_idx = self.data["time_idx"].max()
        num_new_dates = len(date_range)
        new_time_idxs = list(range(last_time_idx + 1, last_time_idx + 1 + num_new_dates))
        time_idx_map = dict(zip(date_range["timestamp"].to_list(), new_time_idxs))
        
        inference_df = inference_df.with_columns([
            pl.col("timestamp").apply(lambda x: time_idx_map[x]).alias("time_idx")
        ])
        
        # Compute date features
        inference_df = inference_df.with_columns([
            pl.col("timestamp").dt.weekday().alias("day_of_week"),  # Monday=0, Sunday=6
            (pl.col("timestamp").dt.weekday() >= 5).alias("is_weekend"),
            pl.col("timestamp").dt.week().alias("week_of_year"),
            pl.col("timestamp").dt.month().alias("month_of_year"),
            ((pl.col("timestamp").dt.day() - 1) // 7 + 1).alias("week_of_month"),
        ])
        
        # Scale date features to [0, 1]
        inference_df = inference_df.with_columns([
            (pl.col("day_of_week") / 6).alias("day_of_week_scaled"),
            (pl.col("week_of_year") / 52).alias("week_of_year_scaled"),
            (pl.col("month_of_year") / 12).alias("month_of_year_scaled"),
            (pl.col("week_of_month") / 4).alias("week_of_month_scaled"),
        ])
        
        # Encode tickers using the stored encoder
        ticker_df = inference_df.select("ticker").to_pandas()
        ticker_encoded = self.encoder.transform(ticker_df)["ticker"].values
        inference_df = inference_df.with_columns([
            pl.Series(name="ticker_encoded", values=ticker_encoded)
        ])
        
        # Merge static features
        static_features_pl = pl.DataFrame(self.static_features)
        inference_df = inference_df.join(static_features_pl, on="ticker_encoded", how="left")
        
        # For unknown tickers, fill static features with global mean and scale
        global_means = static_features_pl.select([
            pl.col("open_center").mean(),
            pl.col("high_center").mean(),
            pl.col("low_center").mean(),
            pl.col("close_center").mean()
        ]).to_dict(as_series=False)
        global_scales = static_features_pl.select([
            pl.col("open_scale").mean(),
            pl.col("high_scale").mean(),
            pl.col("low_scale").mean(),
            pl.col("close_scale").mean()
        ]).to_dict(as_series=False)
        
        # Fill missing static features
        inference_df = inference_df.with_columns([
            pl.col("open_center").fill_null(global_means["open_center"][0]),
            pl.col("open_scale").fill_null(global_scales["open_scale"][0]),
            pl.col("high_center").fill_null(global_means["high_center"][0]),
            pl.col("high_scale").fill_null(global_scales["high_scale"][0]),
            pl.col("low_center").fill_null(global_means["low_center"][0]),
            pl.col("low_scale").fill_null(global_scales["low_scale"][0]),
            pl.col("close_center").fill_null(global_means["close_center"][0]),
            pl.col("close_scale").fill_null(global_scales["close_scale"][0]),
        ])
        
        # Since we don't have actual values for 'open', 'high', 'low', 'close' in future dates, we can set them to NaN
        inference_df = inference_df.with_columns([
            pl.lit(None).cast(pl.Float64).alias("open"),
            pl.lit(None).cast(pl.Float64).alias("high"),
            pl.lit(None).cast(pl.Float64).alias("low"),
            pl.lit(None).cast(pl.Float64).alias("close"),
            pl.lit(None).cast(pl.Float64).alias("volume"),
        ])
        
        # Normally, 'relative_time_idx' is computed based on the last known 'time_idx' for each ticker
        # For simplicity, we'll assume it starts from 0 for the inference period
        inference_df = inference_df.groupby("ticker").apply(
            lambda df: df.with_columns([
                (pl.col("time_idx") - pl.col("time_idx").min()).alias("relative_time_idx")
            ])
        )
        
        # Ensure all required features are present
        missing_cols = [col for col in self.features if col not in inference_df.columns]
        if missing_cols:
            raise ValueError(f"Missing columns in inference data: {missing_cols}")
        
        console.print("Inference data preparation completed successfully.", style="cyan")
        
        return inference_df
    
    def predict(self, context, model_input):
        pass
    
    def get_data(self):
        return self.data
    
    def save_checkpoint(self, flow, run):
        run_id = run.info.run_id
        with TemporaryDirectory() as tmp:
            path = Path(tmp, f"artifacts-{run_id}")

            with open(path, "wb") as f:
                pickle.dump(self.artifacts, f)

            flow.log_artifact(path)
            flow.log_input(mlflow.data.from_pandas(self.datasets.training.collect().to_pandas()), "training")
            flow.log_input(mlflow.data.from_pandas(self.datasets.validation.collect().to_pandas()), "validation")


if __name__ == "__main__":
    tracking_uri = "http://localhost:8092"
    mlflow.set_tracking_uri(tracking_uri)
    
    with mlflow.start_run() as run:

        experiment = Experiment(name="daily ticker OHLCV", team="psf-core", domain="forecasting", model_architecture="temporal-fusion-transformer")

        data = prepare_data("data/previous-close/sample.csv", training_cutoff="2024-04-01")

        dataset = TimeSeriesDataset(data)

        # dataset.save_checkpoint(mlflow, run)

        trainer = Trainer(
            experiment=experiment,
            model=TemporalFusionTransformer,
        )
