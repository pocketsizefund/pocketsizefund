import os
from pytorch_forecasting.data import NaNLabelEncoder
from pytorch_forecasting.metrics import QuantileLoss
from pytorch_forecasting.models.temporal_fusion_transformer.tuning import optimize_hyperparameters
import pickle

import lightning.pytorch as pl
import pandas as pd
from lightning.pytorch.callbacks import Callback, EarlyStopping, LearningRateMonitor
from lightning.pytorch.loggers import TensorBoardLogger
from pytorch_forecasting import TemporalFusionTransformer, TimeSeriesDataSet
from pytorch_forecasting.metrics import RMSE
from torch.utils.data import DataLoader
import torch

# import wandb


# import warnings
# warnings.filterwarnings(
#     action='ignore',
#     message='X does not have valid feature names',
#     category=UserWarning,
#     module='sklearn'
# )



pl.seed_everything(42)


def join_tickers() -> pd.DataFrame:
    files = os.listdir("data/previous-close")
    files = [file for file in files if file.endswith(".csv")]

    dataframes = []
    for file in files:
        dataframes.append(pd.read_csv(f"data/previous-close/{file}").drop(columns=["Unnamed: 0"]))


    data = pd.concat(dataframes, axis=0)
    data.reset_index(inplace=True)
    data["timestamp"] = pd.to_datetime(data["timestamp"])
    return data


def transform_tickers(tickers: pd.DataFrame, threshold: str = "2024-09-09") -> pd.DataFrame:
    threshold_date = pd.to_datetime(threshold)
    
    ticker_counts = tickers.groupby('ticker').size()
    
    valid_tickers_count = ticker_counts[ticker_counts >= 50].index
    
    max_timestamps = tickers.groupby('ticker')['timestamp'].max()
    
    valid_tickers_date = max_timestamps[max_timestamps >= threshold_date].index
    
    valid_tickers = set(valid_tickers_count) & set(valid_tickers_date)
    invalid_tickers = set(tickers['ticker'].unique()) - valid_tickers

    print(f"Invalid tickers: {invalid_tickers}")
    
    filtered_tickers = tickers[tickers['ticker'].isin(valid_tickers)]

    filtered_tickers['time_index'] = (filtered_tickers['timestamp'] - filtered_tickers['timestamp'].min()).dt.days

    # TODO: are there tickers that are being loaded as bools? 
    filtered_tickers["ticker"] = filtered_tickers["ticker"].astype("str").astype("category")

    return filtered_tickers[["time_index", "open", "high", "low", "close", "ticker"]]




def create_timeseries_datasets(data: pd.DataFrame) -> TimeSeriesDataSet:
    maximum_prediction_length = 10
    maximum_encoder_length = 24

    training_cutoff = data["time_index"].max() - maximum_prediction_length

    dataset = TimeSeriesDataSet(
        data=data[lambda x: x.time_index <= training_cutoff], 
        time_idx="time_index",
        target=[
            "open",
            "high",
            "low",
            "close",
        ],
        categorical_encoders={"ticker": NaNLabelEncoder().fit(data.ticker)},
        group_ids=["ticker"],
        min_encoder_length=maximum_encoder_length // 2,
        max_encoder_length=maximum_encoder_length,
        min_prediction_length=1,
        max_prediction_length=maximum_prediction_length,
        static_categoricals=["ticker"],
        time_varying_unknown_reals=[
            "open",
            "high",
            "low",
            "close",
        ],
        add_relative_time_idx=True,
        add_target_scales=True,
        add_encoder_length=True,
        allow_missing_timesteps=True,
    )

    validation = TimeSeriesDataSet.from_dataset(dataset, data, predict=True, stop_randomization=True)

    batch_size = 128
    train_dataloader = dataset.to_dataloader(train=True, batch_size=batch_size, num_workers=12)
    val_dataloader = validation.to_dataloader(train=False, batch_size=batch_size * 10, num_workers=12)

    return dataset, train_dataloader, val_dataloader


def save_sample_batch(dataset):
    batch = dataset.to_dataloader(batch_size=1, num_workers=0).dataset.__getitem__(0)

    with open('sample_batch.pkl', 'wb') as f:
        pickle.dump(batch, f)


def hyperparameter_tuning(train, val):
    optimize_hyperparameters(
        train,
        val,
        model_path="optuna_test",
        n_trials=200,
        max_epochs=5,
        gradient_clip_val_range=(0.01, 1.0),
        hidden_size_range=(8, 128),
        hidden_continuous_size_range=(8, 128),
        attention_head_size_range=(1, 4),
        learning_rate_range=(0.001, 0.1),
        dropout_range=(0.1, 0.3),
        trainer_kwargs=dict(limit_train_batches=30),
        reduce_on_plateau_patience=4,
        use_learning_rate_finder=False,
    )

def create_trainer():
    early_stop_callback = EarlyStopping(
        monitor="val_loss",
        min_delta=1e-4,
        patience=10,
        verbose=False,
        mode="min",
    )

    learning_rate_logger = LearningRateMonitor()
    logger = TensorBoardLogger("lightning_logs")

    return pl.Trainer(
        max_epochs=1,
        accelerator="cpu",
        enable_model_summary=True,
        gradient_clip_val=0.1,
        limit_train_batches=50,
        callbacks=[
            learning_rate_logger,
            early_stop_callback,
        ],
        logger=logger,
    )


def create_model(dataset):
    return TemporalFusionTransformer.from_dataset(
        dataset,
        learning_rate=0.03,
        hidden_size=16,
        attention_head_size=2,
        dropout=0.1,
        hidden_continuous_size=8,
        loss=QuantileLoss(),
        log_interval=10,
        # optimizer="adam",
        reduce_on_plateau_patience=4,
    )


def train_model(trainer, model, train_dataloader, val_dataloader):
    trainer.fit(
        model,
        train_dataloaders=train_dataloader,
        val_dataloaders=val_dataloader,
    )

    return trainer


def save_model(trainer):
    best_model_path = trainer.checkpoint_callback.best_model_path

    print(f"Saving model to {best_model_path}")

    with open(best_model_path, "wb") as f:
        pickle.dump(trainer, f)



def pipeline() -> None:
    tickers = transform_tickers(join_tickers())
    dataset, train, val = create_timeseries_datasets(tickers)
    save_sample_batch(dataset)
    model = create_model(dataset)
    trainer = create_trainer()
    trainer = train_model(trainer, model, train, val)
    # save_model(trainer)





class PriceModel:
    """PriceModel holds a trained model for making predictions."""

    def __init__(
        self,
        weights_and_biases_api_key: str = "",
    ) -> None:
        """Initialize the model type to be trained and used."""
        self.weights_and_biases_api_key = weights_and_biases_api_key
        self.batch_size = 128  # set this between 32 to 128
        self.model = None

    def train_model(
        self,
        data: pd.DataFrame,
    ) -> None:
        """Train the model with the input data."""
        data = self._generate_features(data)

        train_dataset = self._generate_input_dataset(data)

        train_dataloader = self._generate_input_dataloader(
            train_dataset=train_dataset,
        )

        validation_dataloader = self._generate_validation_dataloader(
            data=data,
            train_dataset=train_dataset,
        )

        model = TemporalFusionTransformer.from_dataset(
            dataset=train_dataset,
            learning_rate=0.03,
            hidden_size=16,
            attention_head_size=2,
            dropout=0.1,
            hidden_continuous_size=8,
            loss=RMSE(),
            optimizer="Ranger",
            reduce_on_plateau_patience=4,
        )

        del train_dataset

        trainer.fit(
            model=model,
            train_dataloaders=train_dataloader,
            val_dataloaders=validation_dataloader,
        )

        del train_dataloader
        del validation_dataloader

        self.model = model

    def save_model(
        self,
        file_path: str,
    ) -> None:
        """Save trained model to a file."""
        self.trainer.save_checkpoint(file_path)

    def load_model(
        self,
        file_path: str,
    ) -> None:
        """Load trained model from a file."""
        self.model = TemporalFusionTransformer.load_from_checkpoint(file_path)

        self.model.eval()

    def get_predictions(
        self,
        data: pd.DataFrame,
    ) -> pd.DataFrame:
        """Get predictions for the input data."""
        data = self._generate_features(data)

        predict_dataset = self._generate_input_dataset(data)

        predict_dataloader = self._generate_input_dataloader(predict_dataset)

        predictions = self.model.predict(
            data=predict_dataloader,
            mode="raw",
            return_x=True,
        )

        return [x[0] for x in predictions[0][0][0][3].tolist()]  # closing prices

    def _generate_features(
        self,
        data: pd.DataFrame,
    ) -> pd.DataFrame:
        start_date = data["timestamp"].min()
        end_date = data["timestamp"].max()

        data["timestamp"] = pd.to_datetime(data["timestamp"])

        dates = pd.date_range(
            start=start_date,
            end=end_date,
            freq="d",
        )

        time_indexes = range(len(dates))
        dates = pd.DataFrame({"time_index": time_indexes, "date": dates})

        data = data.merge(
            right=dates,
            left_on="timestamp",
            right_on="date",
        )

        data["weekday"] = data["timestamp"].dt.weekday
        data["yearday"] = data["timestamp"].dt.dayofyear

        data["ticker"] = pd.Categorical(data["ticker"])

        return data

    def _generate_input_dataloader(
        self,
        train_dataset: TimeSeriesDataSet,
    ) -> DataLoader:
        return train_dataset.to_dataloader(
            train=True,
            batch_size=self.batch_size,
            num_workers=0,
        )

    def _generate_validation_dataloader(
        self,
        data: pd.DataFrame,
        train_dataset: TimeSeriesDataSet,
    ) -> DataLoader:
        validation = TimeSeriesDataSet.from_dataset(
            dataset=train_dataset,
            data=data,
            predict=True,
            stop_randomization=True,
        )

        return validation.to_dataloader(
            train=False,
            batch_size=self.batch_size,
            num_workers=0,
        )


if __name__ == "__main__":
    pipeline()
