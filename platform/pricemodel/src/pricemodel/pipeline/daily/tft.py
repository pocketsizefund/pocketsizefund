import os
# from prefect import flow, task
from src.pricemodel.pipeline.daily import data
# from prefect.task_runners import ThreadPoolTaskRunner
import pandas as pd
from darts import TimeSeries
from darts.utils.likelihood_models import QuantileRegression
from darts.models import TFTModel
from darts.metrics import mape
import torch
import time
import random


from rich.progress import (
    BarColumn,
    MofNCompleteColumn,
    Progress,
    TextColumn,
    TimeElapsedColumn,
    TimeRemainingColumn,
)


torch.set_float32_matmul_precision("medium")

progress_bar = Progress(
        TextColumn("[progress.percentage]{task.percentage:>3.0f}%"),
        BarColumn(),
        MofNCompleteColumn(),
        TextColumn("•"),
        TimeElapsedColumn(),
        TextColumn("•"),
        TimeRemainingColumn(),
    )

with progress_bar as p:
    for ticker in p.track(tickers):
        previous_close = data.load_previous_close(ticker)
        previous_close = data.transform_previous_close(previous_close)
        data.save_previous_close(ticker, previous_close)





def pipeline():
    tickers = data.load_all_tickers()
    tickers = data.filter_tickers(tickers)


    random.shuffle(tickers)

    progress_bar = Progress(
        TextColumn("[progress.percentage]{task.percentage:>3.0f}%"),
        BarColumn(),
        MofNCompleteColumn(),
        TextColumn("•"),
        TimeElapsedColumn(),
        TextColumn("•"),
        TimeRemainingColumn(),
    )

    with progress_bar as p:
        for ticker in p.track(tickers):
            previous_close = data.load_previous_close(ticker)
            previous_close = data.transform_previous_close(previous_close)
            data.save_previous_close(ticker, previous_close)



# @task
def join_tickers() -> pd.DataFrame:
    files = os.listdir("data/previous-close")
    files = [file for file in files if file.endswith(".csv")]

    dataframes = []
    for file in files:
        dataframes.append(pd.read_csv(f"data/previous-close/{file}").drop(columns=["Unnamed: 0"]))


    data = pd.concat(dataframes, axis=0)
    data["timestamp"] = pd.to_datetime(data["timestamp"])
    return data


# @task
def create_timeseries_dataset(data: pd.DataFrame) -> TimeSeries:
    # fillna_value (Optional[float]) – Optionally, a numeric value to fill missing values (NaNs) with.
    return TimeSeries.from_group_dataframe(data, 
                                           group_cols="ticker", 
                                           time_col="timestamp",
                                           freq="D", 
                                           fill_missing_dates=True,
                                           value_cols=["open", "high", "low", "close", "volume"])

# @task
def transform_tickers(tickers: pd.DataFrame, threshold: str = "2024-09-09") -> pd.DataFrame:
    threshold_date = pd.to_datetime(threshold)
    
    ticker_counts = tickers.groupby('ticker').size()
    
    valid_tickers_count = ticker_counts[ticker_counts >= 100].index
    
    max_timestamps = tickers.groupby('ticker')['timestamp'].max()
    
    valid_tickers_date = max_timestamps[max_timestamps >= threshold_date].index
    
    valid_tickers = set(valid_tickers_count) & set(valid_tickers_date)
    invalid_tickers = set(tickers['ticker'].unique()) - valid_tickers

    print(f"Invalid tickers: {invalid_tickers}")
    
    filtered_tickers = tickers[tickers['ticker'].isin(valid_tickers)]
    
    return filtered_tickers


# @task
def train_test_split(data: list[TimeSeries], split: str = "20240816") -> tuple[list[TimeSeries], list[TimeSeries]]:

    train = []
    test = []

    for series in data:
        trn, tst = series.split_after(pd.Timestamp(split))
        train.append(trn)
        test.append(tst)

    # TODO: any scaling?
    # scaler_air = Scaler(scaler=MaxAbsScaler())
    # air_train_scaled: List[TimeSeries] = scaler_air.fit_transform(air_train)
    # air_test_scaled: List[TimeSeries] = scaler_air.transform(air_test)

    return train, test


# @task
def create_model() -> TFTModel:
    quantiles = [
        0.01,
        0.05,
        0.25,
        0.5,
        0.75,
        0.95,
        0.99,
    ]
    input_chunk_length = 20 
    forecast_horizon = 10
    return TFTModel(
        input_chunk_length=input_chunk_length,
        output_chunk_length=forecast_horizon,
        hidden_size=64,
        lstm_layers=2,
        num_attention_heads=4,
        dropout=0.1,
        use_static_covariates=False,
        batch_size=1024,
        add_relative_index=True,
        n_epochs=100,
        add_encoders=None,
        likelihood=QuantileRegression(
            quantiles=quantiles
        ),  # QuantileRegression is set per default
        # loss_fn=MSELoss(),
        random_state=42,

    )


# @task
def train_model(model: TFTModel, train: list[TimeSeries]) -> TFTModel:
    model.fit(train, verbose=True)

    return model


# @flow
def training_pipeline():
    tickers = join_tickers()
    tickers = transform_tickers(tickers)
    tickers = create_timeseries_dataset(tickers)
    train, test = train_test_split(tickers)

    model = create_model()
    train_model(model, train)
