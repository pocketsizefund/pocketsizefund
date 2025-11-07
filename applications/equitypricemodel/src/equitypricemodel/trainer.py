import os
from typing import cast

import polars as pl
from equitypricemodel.tide_data import Data
from equitypricemodel.tide_model import Model

training_data_input_path = os.path.join(  # noqa: PTH118
    os.getenv("SM_CHANNEL_TRAIN", "/opt/ml/input/data/train"),
    "filtered_tft_training_data.parquet",
)

model_output_path = os.getenv("SM_MODEL_DIR", "/opt/ml/model")


configuration = {
    "architecture": "TiDE",
    "learning_rate": 0.003,
    "epoch_count": 20,
    "validation_split": 0.8,
    "input_length": 35,
    "output_length": 7,
    "hidden_size": 64,
    "num_encoder_layers": 2,
    "num_decoder_layers": 2,
    "dropout_rate": 0.1,
}

training_data = pl.read_parquet(training_data_input_path)


data = Data()

data.preprocess_and_set_data(data=training_data)


dimensions = data.get_dimensions()

train_batches = data.get_batches(
    data_type="train",
    validation_split=configuration["validation_split"],
    input_length=configuration["input_length"],
    output_length=configuration["output_length"],
)

sample_batch = train_batches[0]

batch_size = sample_batch["encoder_continuous_features"].shape[0]

# calculate each component's flattened size - days * features (e.g. 35 * 7)
encoder_continuous_size = (
    sample_batch["encoder_continuous_features"].reshape(batch_size, -1).shape[1]
)
encoder_categorical_size = (
    sample_batch["encoder_categorical_features"].reshape(batch_size, -1).shape[1]
)
decoder_categorical_size = (
    sample_batch["decoder_categorical_features"].reshape(batch_size, -1).shape[1]
)
static_categorical_size = (
    sample_batch["static_categorical_features"].reshape(batch_size, -1).shape[1]
)

input_size = cast(
    "int",
    encoder_continuous_size
    + encoder_categorical_size
    + decoder_categorical_size
    + static_categorical_size,
)

model = Model(
    input_size=input_size,
    hidden_size=configuration["hidden_size"],
    num_encoder_layers=configuration["num_encoder_layers"],
    num_decoder_layers=configuration["num_decoder_layers"],
    output_length=configuration["output_length"],
    dropout_rate=configuration["dropout_rate"],
    quantiles=[0.1, 0.5, 0.9],
)


losses = model.train(
    train_batches=train_batches,
    epochs=configuration["epoch_count"],
    learning_rate=configuration["learning_rate"],
)

model.save(directory_name=model_output_path)
