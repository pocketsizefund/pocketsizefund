import argparse
import pickle
from datetime import datetime
import os
import json

import tensorflow
from matplotlib import pyplot

from pkg.storage import storage
from pkg.model import model


parser = argparse.ArgumentParser()

parser.add_argument(
    '--s3-artifacts-bucket-name',
    type=str,
    dest='s3_artifacts_bucket_name',
)

parser.add_argument(
    '--key-name',
    type=str,
    dest='key_name',
)

arguments = parser.parse_args()

storage_client = storage.Client(
    s3_data_bucket_name='s3_data_bucket_name',
    s3_artifacts_bucket_name=arguments.s3_artifacts_bucket_name,
)

storage_client.download_model_artifacts(
    key=arguments.key_name,
)

model_model = model.Model(
    artifact_output_path='.',
)

model_model.load_model()

testing_data = tensorflow.data.Dataset.load(
    path='./testing_data',
    compression='GZIP',
)

testing_metrics = model_model.evaluate_model(
    data=testing_data,
)

training_metrics_file = open('./metrics.pkl', 'rb')

training_metrics = pickle.load(training_metrics_file)

metrics = {
    'testing': testing_metrics,
    'training': training_metrics,
}

now = datetime.now().strftime('%Y%m%d%H%M%S')

os.makedirs('metrics/{}'.format(now))

metrics_file = open('metrics/{}/metrics.json'.format(now), 'w')
json.dump(metrics, metrics_file)

loss = training_metrics["loss"]
mean_absolute_error = training_metrics["mean_absolute_error"]
validation_loss = training_metrics["val_loss"]
validation_mean_absolute_error = training_metrics["val_mean_absolute_error"]

epochs = range(1, len(loss) + 1)

pyplot.figure(figsize=(10, 6))

pyplot.subplot(2, 1, 1)
pyplot.plot(epochs, loss, 'bo-', label='Training Loss')
pyplot.plot(epochs, validation_loss, 'ro-', label='Validation Loss')
pyplot.title('Training and Validation Loss')
pyplot.xlabel('Epochs')
pyplot.ylabel('Loss')
pyplot.legend()

pyplot.subplot(2, 1, 2)
pyplot.plot(epochs, mean_absolute_error, 'bo-', label='Training MAE')
pyplot.plot(
    epochs,
    validation_mean_absolute_error,
    'ro-',
    label='Validation MAE',
)
pyplot.title('Training and Validation Mean Absolute Error')
pyplot.xlabel('Epochs')
pyplot.ylabel('MAE')
pyplot.legend()

pyplot.tight_layout()

pyplot.savefig('metrics/{}/plot.png'.format(now))
