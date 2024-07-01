"""Model module trains on and predicts prices."""

import lightning.pytorch as pl
import pandas as pd
import wandb
from lightning.pytorch.callbacks import Callback, EarlyStopping, LearningRateMonitor
from lightning.pytorch.loggers import TensorBoardLogger
from pytorch_forecasting import TemporalFusionTransformer, TimeSeriesDataSet
from pytorch_forecasting.data import GroupNormalizer, MultiNormalizer
from pytorch_forecasting.metrics import RMSE
from torch.utils.data import DataLoader


class WeightsAndBiasesLogger(Callback):
    def __init__(
        self,
        weights_and_biases_api_key: str = "",
    ) -> None:
        """Initialize the WeightsAndBiasesLogger class."""
        super().__init__()

        wandb.login(
            key=weights_and_biases_api_key,
        )

        wandb.init(project="price-prediction-temporal-fusion-transformer")

    def on_train_epoch_end(
        self,
        trainer: pl.Trainer,
        pl_module: pl.LightningModule,
    ) -> None:
        """Capture and send epoch metrics to Weights and Biases."""
        _ = pl_module

        wandb.log(
            {
                "training_loss": trainer.callback_metrics.get("train_loss", None),
                "validation_loss": trainer.callback_metrics.get("val_loss", None),
            },
        )


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

        pl.seed_everything(42)  # TEMP

        early_stop_callback = EarlyStopping(
            monitor="val_loss",
            min_delta=1e-4,
            patience=10,
            verbose=False,
            mode="min",
        )
        learning_rate_logger = LearningRateMonitor()
        logger = TensorBoardLogger("lightning_logs")

        weights_and_biases_logger = WeightsAndBiasesLogger(
            weights_and_biases_api_key=self.weights_and_biases_api_key,
        )

        trainer = pl.Trainer(
            max_epochs=50,
            accelerator="gpu",
            enable_model_summary=True,
            gradient_clip_val=0.1,
            limit_train_batches=50,  # comment in for training, running valiation every 30 batches
            callbacks=[
                learning_rate_logger,
                early_stop_callback,
                weights_and_biases_logger,
            ],
            logger=logger,
        )

        self.trainer = trainer

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

    def _generate_input_dataset(
        self,
        data: pd.DataFrame,
    ) -> TimeSeriesDataSet:
        maximum_prediction_length = 5
        maximum_encoder_length = 24

        return TimeSeriesDataSet(
            data=data,
            time_idx="time_index",
            target=[
                "open_price",
                "high_price",
                "low_price",
                "close_price",
            ],
            group_ids=["ticker"],
            min_encoder_length=maximum_encoder_length // 2,
            max_encoder_length=maximum_encoder_length,
            min_prediction_length=1,
            max_prediction_length=maximum_prediction_length,
            static_categoricals=["ticker"],
            time_varying_known_reals=["time_index"],
            time_varying_unknown_reals=[
                "open_price",
                "high_price",
                "low_price",
                "close_price",
                "volume",
            ],
            target_normalizer=MultiNormalizer(
                normalizers=[
                    GroupNormalizer(
                        groups=["ticker"],
                        transformation="softplus",
                    ),
                    GroupNormalizer(
                        groups=["ticker"],
                        transformation="softplus",
                    ),
                    GroupNormalizer(
                        groups=["ticker"],
                        transformation="softplus",
                    ),
                    GroupNormalizer(
                        groups=["ticker"],
                        transformation="softplus",
                    ),
                ],
            ),
            add_relative_time_idx=True,
            add_target_scales=True,
            add_encoder_length=True,
            allow_missing_timesteps=True,
        )

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
