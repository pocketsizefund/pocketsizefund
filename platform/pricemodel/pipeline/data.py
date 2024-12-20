import polars as pl
import numpy as np
from datetime import datetime, timedelta, timezone
import pickle
import os
from category_encoders import OrdinalEncoder
from rich.console import Console
from rich import pretty
from tinygrad import Tensor, dtypes

console = Console()
pretty.install()

MIN_OBSERVATIONS_PER_TICKER = 100
FRESHNESS_DATE = '2013-12-31'
TRAINING_CUTOFF = "2022-04-01"

def load_data(file):
    return pl.read_csv(file).sort(["ticker", "timestamp"])


def prepare_data(data: pl.LazyFrame) -> pl.DataFrame:
    console.print("Starting data preparation...", style="cyan")
    
    console.print("Ensuring proper data types...", style="cyan")
    data = data.with_columns([
        pl.col("timestamp").str.strptime(pl.Datetime).cast(pl.Date),#, "%Y-%m-%d"),
        pl.col("open_price").cast(pl.Float64).alias("open"),
        pl.col("high_price").cast(pl.Float64).alias("high"),
        pl.col("low_price").cast(pl.Float64).alias("low"),
        pl.col("close_price").cast(pl.Float64).alias("close"),
        pl.col("volume").cast(pl.Float64),
        pl.col("ticker").cast(pl.Utf8),
    ])
    
    console.print("Dropping duplicate rows...", style="cyan")
    data = data.unique(subset=["ticker", "timestamp"])
    
    console.print("Filtering tickers with insufficient data...", style="cyan")
    ticker_counts = data.group_by("ticker").agg(pl.count().alias("count"))
    valid_tickers = ticker_counts.filter(
        pl.col("count") >= MIN_OBSERVATIONS_PER_TICKER
    ).select("ticker")
    data = data.filter(pl.col("ticker").is_in(valid_tickers))
    
    assert data.height > 0, "No data left after filtering for minimum observations."
    
    console.print(f"Filtering tickers without data after {FRESHNESS_DATE}...", style="cyan")
    freshness_dt = datetime.strptime(FRESHNESS_DATE, "%Y-%m-%d").replace(tzinfo=timezone.utc)
    latest_timestamps = data.group_by("ticker").agg(pl.col("timestamp").max().alias("max_timestamp"))
    latest_timestamps = latest_timestamps.with_columns([
        pl.col("max_timestamp")
    ])
    valid_tickers = latest_timestamps.filter(
        pl.col("max_timestamp") >= freshness_dt
    )["ticker"]
    data = data.filter(pl.col("ticker").is_in(valid_tickers))
    
    assert data.height > 0, "No data left after filtering for freshness date."
    
    tickers = data.select("ticker").unique()
    min_date = data.select(pl.col("timestamp").min())[0, 0]
    max_date = data.select(pl.col("timestamp").max())[0, 0]
    
    console.print(f"min_date: {min_date}, type: {type(min_date)}", style="cyan")
    console.print(f"max_date: {max_date}, type: {type(max_date)}", style="cyan")
    
    console.print("Generating full date range and performing cross join.", style="cyan")
    full_dates = pl.DataFrame({
        "timestamp": pl.date_range(
            start=min_date,
            end=max_date,
            interval="1d",
            closed="both",
            eager=True  # Ensure eager evaluation
        )
    })
    
    # Proceed with cross join
    full_df = tickers.join(full_dates, how="cross")
    
    # Assign time_idx during cross join
    full_df = full_df.with_columns([
        pl.col("timestamp").rank(method="dense").cast(pl.Int32).alias("time_idx") - 1  # Start from 0
    ])
    
    # Join with original data
    console.print("Joining full dataframe with original data...", style="cyan")
    data = full_df.join(data, on=["ticker", "timestamp"], how="left")
    
    # Sort data
    data = data.sort(["ticker", "timestamp"])
    
    # Fill missing numeric values using interpolation
    console.print("Filling missing numeric values using interpolation...", style="cyan")
    numeric_cols = ["open", "high", "low", "close", "volume"]
    
    data = (
        data
        .group_by("ticker")
        .map_groups(lambda df: df.sort("timestamp")
                            .with_columns([pl.col(col)
                                             .interpolate()
                                             .fill_null(strategy="forward")
                                             .fill_null(strategy="backward") for col in numeric_cols])
               )
    )
    
    for col in numeric_cols:
        missing_count = data.select(pl.col(col).is_null().sum())[0, 0]
        assert missing_count == 0, f"Missing values remain in column {col}."
    
    console.print("Computing date features...", style="cyan")
    data = data.with_columns([
        pl.col("timestamp").dt.weekday().alias("day_of_week"),  # Monday=0, Sunday=6
        (pl.col("timestamp").dt.weekday() >= 5).alias("is_weekend"),
        pl.col("timestamp").dt.week().alias("week_of_year"),
        pl.col("timestamp").dt.month().alias("month_of_year"),
        ((pl.col("timestamp").dt.day() - 1) // 7 + 1).alias("week_of_month"),
    ])
    
    data = data.with_columns([
        (pl.col("day_of_week") / 6).alias("day_of_week_scaled"),
        (pl.col("week_of_year") / 52).alias("week_of_year_scaled"),
        (pl.col("month_of_year") / 12).alias("month_of_year_scaled"),
        (pl.col("week_of_month") / 4).alias("week_of_month_scaled"),
    ])
    
    # Create relative_time_idx per group
    console.print("Computing relative_time_idx per group...", style="cyan")
    data = data.group_by("ticker").map_groups(
        lambda df: df.with_columns([
            (pl.col("time_idx") - pl.col("time_idx").min()).alias("relative_time_idx")
        ])
    )
    
    console.print("Encoding 'ticker' using OrdinalEncoder...", style="cyan")
    ticker_df = data.select("ticker").to_pandas()
    
    # Initialize OrdinalEncoder with handle_unknown='use_encoded_value' and unknown_value=-1
    encoder = OrdinalEncoder(
        cols=["ticker"],
        handle_unknown='use_encoded_value',
        handle_missing='use_encoded_value',
        # unknown_value=-1
    )
    
    # Fit encoder
    encoder.fit(ticker_df)
    
    # Transform and add encoded ticker to data
    ticker_encoded = encoder.transform(ticker_df)["ticker"].values
    data = data.with_columns([
        pl.Series(name="ticker_encoded", values=ticker_encoded)
    ])
    
    # Compute static real variables per ticker
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
    
    # Handle cases where standard deviation is zero
    console.print("Handling cases where standard deviation is zero...", style="cyan")
    static_real_features = static_real_features.with_columns([
        pl.when(pl.col(col) == 0).then(1e-8).otherwise(pl.col(col)).alias(col) for col in [
            "open_scale", "high_scale", "low_scale", "close_scale"
        ]
    ])
    
    # Store static features for later use (e.g., during inference)
    static_features = static_real_features.to_pandas()
    
    # Join static features back to data
    console.print("Joining static features back to data...", style="cyan")
    data = data.join(static_real_features, on="ticker_encoded", how="left")
    
    # Normalize real-valued features
    console.print("Normalizing real-valued features...", style="cyan")
    data = data.with_columns([
        ((pl.col("open") - pl.col("open_center")) / pl.col("open_scale")).alias("open_normalized"),
        ((pl.col("high") - pl.col("high_center")) / pl.col("high_scale")).alias("high_normalized"),
        ((pl.col("low") - pl.col("low_center")) / pl.col("low_scale")).alias("low_normalized"),
        ((pl.col("close") - pl.col("close_center")) / pl.col("close_scale")).alias("close_normalized"),
    ])
    
    # Check for any missing values after normalization
    normalized_cols = ["open_normalized", "high_normalized", "low_normalized", "close_normalized"]
    for col in normalized_cols:
        missing_count = data.select(pl.col(col).is_null().sum())[0, 0]
        assert missing_count == 0, f"Missing values in normalized column {col}."
    
    console.print("Data preparation completed successfully.", style="cyan")
    
    return data, static_features, encoder

def split_data(data: pl.DataFrame) -> tuple:
    console.print("Splitting data into training and validation sets...", style="cyan")
    
    data = data.with_columns([
        pl.col("timestamp").cast(pl.Date)
    ])
    
    training_cutoff = datetime.strptime(TRAINING_CUTOFF, "%Y-%m-%d")
    
    training_data = data.filter(pl.col("timestamp") < training_cutoff)
    validation_data = data.filter(pl.col("timestamp") >= training_cutoff)
    
    training_dates = set(training_data.select("timestamp").unique()["timestamp"].to_list())
    validation_dates = set(validation_data.select("timestamp").unique()["timestamp"].to_list())
    
    assert len(training_dates.intersection(validation_dates)) == 0, "Training and validation sets have overlapping dates."
    assert validation_data.shape[0] > 0, "Missing validation data"
    
    console.print("Data splitting completed successfully.", style="cyan")

    training_data = training_data.select([
        pl.col("ticker_encoded"),
        pl.col("open"),
        pl.col("high"),
        pl.col("low"),
        pl.col("close"),
        pl.col("volume")
    ]).to_numpy()

    validation_data = validation_data.select([
        pl.col("ticker_encoded"),
        pl.col("open"),
        pl.col("high"),
        pl.col("low"),
        pl.col("close"),
        pl.col("volume")
    ]).to_numpy()

    return Tensor(training_data, dtype=dtypes.float), Tensor(validation_data, dtype=dtypes.float)

class TimeSeriesDataset:
    def __init__(self, data: pl.DataFrame, static_features, encoder):
        self.data = data
        self.static_features = static_features
        self.encoder = encoder
        self.features = [
            "ticker_encoded", "relative_time_idx", "time_idx",
            "open_normalized", "high_normalized", "low_normalized", "close_normalized",
            "day_of_week_scaled", "week_of_month_scaled", "month_of_year_scaled", "week_of_year_scaled",
            "is_weekend"
        ]
        self.target = "close_normalized"
        # Checks
        missing_cols = [col for col in self.features + [self.target] if col not in data.columns]
        if missing_cols:
            raise ValueError(f"Missing columns in data: {missing_cols}")
    
    def load_context(self, context):
        # Load artifacts from context.artifacts
        import joblib
        self.static_features = joblib.load(context.artifacts["static_features"])
        self.encoder = joblib.load(context.artifacts["encoder"])
    
    def prepare_inference_data(self, tickers, start_date, end_date):
        console.print("Preparing inference data...", style="cyan")
        # Generate date range
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
        inference_df = inference_df.group_by("ticker").apply(
            lambda df: df.with_columns([
                (pl.col("time_idx") - pl.col("time_idx").min()).alias("relative_time_idx")
            ])
        )
        
        missing_cols = [col for col in self.features if col not in inference_df.columns]
        if missing_cols:
            raise ValueError(f"Missing columns in inference data: {missing_cols}")
        
        console.print("Inference data preparation completed successfully.", style="cyan")
        
        return inference_df
    
    def predict(self, context, model_input):
        # Implement model prediction logic here
        pass  # Placeholder
    
    def get_data(self):
        return self.data
    
    def save(self, path):
        with open(os.path.join(path, "dataset.pkl"), "wb") as f:
            pickle.dump(self.data.to_pandas(), f)
        with open(os.path.join(path, "static_features.pkl"), "wb") as f:
            pickle.dump(self.static_features, f)
        import joblib
        joblib.dump(self.encoder, os.path.join(path, "encoder.pkl"))
