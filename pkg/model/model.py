"""Model module trains on and predicts prices."""

<<<<<<< HEAD
import lightning.pytorch as pl
import pandas as pd
from lightning.pytorch.callbacks import EarlyStopping, LearningRateMonitor
from lightning.pytorch.loggers import TensorBoardLogger
from pytorch_forecasting import TemporalFusionTransformer, TimeSeriesDataSet
from pytorch_forecasting.data import GroupNormalizer, MultiNormalizer
from pytorch_forecasting.metrics import RMSE
from torch.utils.data import DataLoader


class Model:
    """Model holds a trained model for making predictions."""
=======
from sagemaker import tensorflow as sagemaker_tensorflow
import pandas
from sklearn import preprocessing
import numpy
import tensorflow
import keras
from keras import models, layers, losses, optimizers, metrics
import wandb


FEATURE_NAMES = tuple([
    "open_price",
    "high_price",
    "low_price",
    "close_price",
    "volume",
])

REQUIRED_COLUMNS = tuple(
    [
        "timestamp",
        "ticker",
    ]
)

WINDOW_INPUT_LENGTH = 30
WINDOW_OUTPUT_LENGTH = 5

CLOSE_PRICE_INDEX = 3


class Model:
    def __init__(
        self,
        artifact_output_path: str,
        weights_and_biases_api_key: str,
        notes: str = "",
    ) -> None:
        self.feature_names = FEATURE_NAMES
        self.required_columns = REQUIRED_COLUMNS
        self.window_input_length = WINDOW_INPUT_LENGTH
        self.window_output_length = WINDOW_OUTPUT_LENGTH
        self.label_count = 1

        self.artifact_output_path = artifact_output_path
        # explicit authorization instead of local secrets.env reference
        self.weights_and_biases_api_key = weights_and_biases_api_key
        self.notes = notes
>>>>>>> 9e24e11 (ruff - double quotes preferred)

    def __init__(self) -> None:
        """Initialize the model type to be trained and used."""
        self.batch_size = 128 # set this between 32 to 128
        self.model = None
<<<<<<< HEAD
=======
        self.scalers = None

    def preprocess_training_features(
        self,
        data: pandas.DataFrame,
        splits: tuple[float, float, float] = (0.7, 0.2, 0.1),
    ) -> dict[str, any]:
        data_grouped_by_ticker = self._clean_and_group_data(data)

        scalers: dict[int, preprocessing.MinMaxScaler] = {}

        scaled_training_data: list[numpy.ndarray] = []
        scaled_validating_data: list[numpy.ndarray] = []
        scaled_testing_data: list[numpy.ndarray] = []

        for ticker, ticker_data in data_grouped_by_ticker.items():
            count = len(ticker_data)

            if count < self.window_input_length + self.window_output_length:
                continue

            first_split = int(count * splits[0])
            second_split = int(count * (splits[0] + splits[1]))

            training_data = ticker_data[:first_split]
            validating_data = ticker_data[first_split:second_split]
            testing_data = ticker_data[second_split:]

            scaler = preprocessing.MinMaxScaler(feature_range=(0, 1))

            scaled_training_data.append(scaler.fit_transform(
                X=training_data.values,
            ))

            scaled_validating_data.append(scaler.transform(
                X=validating_data.values,
            ))

            scaled_testing_data.append(scaler.transform(
                X=testing_data.values,
            ))

            scalers[ticker] = scaler

        training_datasets = list(map(lambda x: self._create_dataset(
            data=x,
        ), scaled_training_data))

        validating_datasets = list(map(lambda x: self._create_dataset(
            data=x,
        ), scaled_validating_data))

        testing_datasets = list(map(lambda x: self._create_dataset(
            data=x,
        ), scaled_testing_data))

        training_dataset = training_datasets[0]
        for dataset in training_datasets[1:]:
            training_dataset = training_dataset.concatenate(dataset)

        validating_dataset = validating_datasets[0]
        for dataset in validating_datasets[1:]:
            validating_dataset = validating_dataset.concatenate(dataset)

        testing_dataset = testing_datasets[0]
        for dataset in testing_datasets[1:]:
            testing_dataset = testing_dataset.concatenate(dataset)

        return {
            "data": {
                "training": training_dataset,
                "validating": validating_dataset,
                "testing": testing_dataset,
            },
            "scalers": scalers,
        }

    def _create_dataset(
        self,
        data: numpy.ndarray,
    ) -> tensorflow.data.Dataset:
        dataset = keras.utils.timeseries_dataset_from_array(
            data=data,
            targets=None,
            sequence_length=self.window_output_length+self.window_input_length,
            sequence_stride=1,
            shuffle=True,
            batch_size=32,
        )

        windowed_dataset = dataset.map(
            lambda x: self._split_window(x)
        )

        return windowed_dataset

    def _split_window(
        self,
        data: tensorflow.Tensor,
    ) -> tensorflow.data.Dataset:
        input_slice = slice(0, self.window_input_length)
        labels_slice = slice(self.window_input_length, None)

        inputs = data[:, input_slice, :]
        labels = data[:, labels_slice, :]

        labels = tensorflow.stack(
            [labels[:, :, CLOSE_PRICE_INDEX]],
            axis=-1,
        )

        inputs.set_shape([None, self.window_input_length, None])
        labels.set_shape([None, self.window_output_length, None])

        return (inputs, labels)

    def preprocess_predicting_features(
        self,
        data: pandas.DataFrame,
        scalers: dict[str, preprocessing.MinMaxScaler],
    ) -> dict[str, tensorflow.data.Dataset]:
        data_grouped_by_ticker = self._clean_and_group_data(data)

        predicting_datasets: dict[str, tensorflow.data.Dataset] = {}

        for ticker, ticker_data in data_grouped_by_ticker.items():
            count = len(ticker_data)

            if count < self.window_input_length:
                continue

            if ticker not in scalers:
                continue

            scaled_ticker_data = scalers[ticker].transform(
                X=ticker_data.values,
            )

            dataset = keras.utils.timeseries_dataset_from_array(
                data=scaled_ticker_data,
                targets=None,
                sequence_length=self.window_input_length,
                shuffle=True,
            )

            predicting_datasets[ticker] = dataset

        return predicting_datasets

    def _clean_and_group_data(
        self,
        data: pandas.DataFrame,
    ) -> dict[str, pandas.DataFrame]:
        data.dropna(
            inplace=True,
        )

        timestamp_column = self.required_columns[0]
        ticker_column = self.required_columns[1]

        data.drop_duplicates(
            subset=[
                timestamp_column,
                ticker_column,
            ],
            inplace=True,
        )

        data = data.filter(
            items=self.required_columns + self.feature_names,
            axis=1,
        )

        data.set_index(
            keys=timestamp_column,
            inplace=True,
        )

        data_grouped_by_ticker = {
            str(ticker): ticker_group.drop(
                columns=[ticker_column],
            )
            for ticker, ticker_group
            in data.groupby(
                by=ticker_column,
                dropna=True,
            )
        }

        return data_grouped_by_ticker
>>>>>>> 9e24e11 (ruff - double quotes preferred)

    def train_model(
        self,
        data: pd.DataFrame,
    ) -> None:
        """Train the model with the input data."""
        data = self._generate_features(data)

<<<<<<< HEAD
        train_dataset = self._generate_input_dataset(data)

        train_dataloader = self._generate_input_dataloader(
            train_dataset=train_dataset,
=======
        wandb.init(
            project="basic-lstm",
            config={
                "epochs": epochs,
            },
            notes=self.notes,
>>>>>>> 9e24e11 (ruff - double quotes preferred)
        )

        validation_dataloader = self._generate_validation_dataloader(
            data=data,
            train_dataset=train_dataset,
        )

        pl.seed_everything(42) # TEMP

        early_stop_callback = EarlyStopping(
            monitor="val_loss",
            min_delta=1e-4,
            patience=10,
            verbose=False,
            mode="min",
        )
        learning_rate_logger = LearningRateMonitor()  # log the learning rate
        logger = TensorBoardLogger("lightning_logs")  # logging results to a tensorboard

        trainer = pl.Trainer(
            max_epochs=50,
            accelerator="gpu",
            enable_model_summary=True,
            gradient_clip_val=0.1,
            limit_train_batches=50,  # comment in for training, running valiation every 30 batches
            callbacks=[
                learning_rate_logger,
                early_stop_callback,
            ],
<<<<<<< HEAD
            logger=logger,
=======
            name="basic_lstm",
>>>>>>> 9e24e11 (ruff - double quotes preferred)
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
            log_interval=10,
            optimizer="Ranger",
            reduce_on_plateau_patience=4,
        )

<<<<<<< HEAD
        trainer.fit(
            model,
            train_dataloaders=train_dataloader,
            val_dataloaders=validation_dataloader,
        )

        self.model = model
=======
        history = self.model.fit(
            x=features["training"],
            epochs=epochs,
            validation_data=features["validating"],
        )

        loss = history.history["loss"]
        mean_absolute_error = history.history["mean_absolute_error"]
        validation_loss = history.history["val_loss"]
        validation_mean_absolute_error = history.history["val_mean_absolute_error"]

        for index in range(epochs):
            wandb.log({
                "training loss": loss[index],
                "training mean absolute error": mean_absolute_error[index],
                "validation loss": validation_loss[index],
                "validation mean absolute error": validation_mean_absolute_error[index],
            })

        return {
            "model": self.model,
            "metrics": history.history,
        }

    def evaluate_model(
        self,
        data: tensorflow.data.Dataset,
    ) -> dict[str, any]:
        evaluation = self.model.evaluate(
            x=data,
            return_dict=True,
            verbose=0,
        )

        return {
            "loss": evaluation["loss"],
            "mean_absolute_error": evaluation["mean_absolute_error"],
        }
>>>>>>> 9e24e11 (ruff - double quotes preferred)

    def save_model(
        self,
        file_path: str,
    ) -> None:
<<<<<<< HEAD
        """Save trained model to a file."""
        self.trainer.save_checkpoint(file_path)
=======
        model.save(
            filepath=os.path.join(self.artifact_output_path, "lstm.keras"),
        )

    def save_scalers(
        self,
        scalers: dict[str, preprocessing.MinMaxScaler],
    ) -> None:
        with open(
            file=os.path.join(self.artifact_output_path, "scalers.pkl"),
            mode="wb",
        ) as scalers_file:
            pickle.dump(
                obj=scalers,
                file=scalers_file,
            )

    def save_data(
        self,
        name: str,
        data: tensorflow.data.Dataset,
    ) -> None:
        data.save(
            path=os.path.join(self.artifact_output_path, name),
            compression="GZIP",
        )
>>>>>>> 9e24e11 (ruff - double quotes preferred)

    def load_model(
        self,
        file_path: str,
    ) -> None:
<<<<<<< HEAD
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

        return self.model.predict(predict_dataloader)

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
=======
        self.model = models.load_model(
            filepath=os.path.join(self.artifact_output_path, "lstm.keras"),
        )

    def load_scalers(
        self,
    ) -> None:
        scalers_file_path = os.path.join(
            self.artifact_output_path,
            "scalers.pkl",
        )

        with open(scalers_file_path, "rb") as scalers_file:
            self.scalers = pickle.load(file=scalers_file)
>>>>>>> 9e24e11 (ruff - double quotes preferred)

        data["ticker"] = pd.Categorical(data["ticker"])

        return data

    def _generate_input_dataset(
        self,
<<<<<<< HEAD
        data: pd.DataFrame,
    ) -> TimeSeriesDataSet:
        maximum_prediction_length = 5
        maximum_encoder_length = 24
=======
        features: dict[str, pandas.DataFrame],
    ) -> dict[str, list[float]]:
        if not self.model or not self.scalers:
            raise Exception("no model or scalers")
>>>>>>> 9e24e11 (ruff - double quotes preferred)

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
            train_dataset,
            data,
            predict=True,
            stop_randomization=True,
        )

        return validation.to_dataloader(
            train=False,
            batch_size=self.batch_size,
            num_workers=0,
        )

class Client:
    """Client invokes a trained model to make predictions."""

    def __init__(
        self,
        model_endpoint_name: str,
    ) -> None:
        """Initialize the client to make predictions."""
        self.model_endpoint_name = model_endpoint_name

    def get_predictions(
        self,
<<<<<<< HEAD
    ) -> pd.DataFrame:
        """Get predictions for the input data."""
        return self.predictor.predict()
=======
        data: pandas.DataFrame,
    ) -> any:
        data["timestamp"] = data["timestamp"].astype(str)

        predictions = self.predictor.predict(
            data=data.to_dict(orient="records"),
        )

        return predictions
>>>>>>> 9e24e11 (ruff - double quotes preferred)
