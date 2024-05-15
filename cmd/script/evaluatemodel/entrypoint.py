import argparse
import json
import os
import pickle
from datetime import datetime

import tensorflow as tf
from matplotlib import pyplot as plt

from pkg.config import config
from pkg.model import model
from pkg.storage import storage

parser = argparse.ArgumentParser()

parser.add_argument(
    "--s3-artifacts-bucket-name",
    type=str,
    dest="s3_artifacts_bucket_name",
)

parser.add_argument(
    "--model-name",
    type=str,
    dest="model_name",
)

arguments = parser.parse_args()

storage_client = storage.Client(
    s3_data_bucket_name="s3_data_bucket_name",
    s3_artifacts_bucket_name=arguments.s3_artifacts_bucket_name,
)

storage_client.download_model_artifacts(
    model_name=arguments.model_name,
)

model_model = model.Model(
    artifact_output_path=".",
    weights_and_biases_api_key="",
)

model_model.load_model()

testing_data = tf.data.Dataset.load(
    path="./testing_data",
    compression="GZIP",
)

testing_metrics = model_model.evaluate_model(
    data=testing_data,
)

training_metrics_file = open("./metrics.pkl", "rb")

training_metrics = pickle.load(training_metrics_file)

metrics = {
    "testing": testing_metrics,
    "training": training_metrics,
}

now = datetime.now(tz=config.TIMEZONE).strftime("%Y%m%d%H%M%S")

os.makedirs(f"metrics/{now}")

metrics_file = open(f"metrics/{now}/metrics.json", "w")
json.dump(metrics, metrics_file)

loss = training_metrics["loss"]
mean_absolute_error = training_metrics["mean_absolute_error"]
validation_loss = training_metrics["val_loss"]
validation_mean_absolute_error = training_metrics["val_mean_absolute_error"]

epochs = range(1, len(loss) + 1)

plt.figure(figsize=(10, 6))

plt.subplot(2, 1, 1)
plt.plot(epochs, loss, "bo-", label="Training Loss")
plt.plot(epochs, validation_loss, "ro-", label="Validation Loss")
plt.title("Training and Validation Loss")
plt.xlabel("Epochs")
plt.ylabel("Loss")
plt.legend()

plt.subplot(2, 1, 2)
plt.plot(epochs, mean_absolute_error, "bo-", label="Training MAE")
plt.plot(
    epochs,
    validation_mean_absolute_error,
    "ro-",
    label="Validation MAE",
)
plt.title("Training and Validation Mean Absolute Error")
plt.xlabel("Epochs")
plt.ylabel("MAE")
plt.legend()

plt.tight_layout()

plt.savefig(f"metrics/{now}/plot.png")
