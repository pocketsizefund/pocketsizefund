import argparse
import tarfile
import io
import os
import json
from datetime import datetime
import pickle

import boto3
from keras import models
import tensorflow
from matplotlib import pyplot


parser = argparse.ArgumentParser(
    prog='model evaluation script',
    description='evaluate the lstm model locally',
)

parser.add_argument(
    '--bucket-name',
    type=str,
    dest='bucket_name',
)

parser.add_argument(
    '--key-name',
    type=str,
    dest='key_name',
)

arguments = parser.parse_args()

s3_client = boto3.client('s3')

response = s3_client.get_object(
    Bucket=arguments.bucket_name,
    Key=arguments.key_name,
)

compressed_data = response['Body'].read()

compressed_file = tarfile.open(
    fileobj=io.BytesIO(compressed_data),
    mode='r:gz',
)

compressed_file.extractall()

current_directory = os.getcwd()

model = models.load_model(
    filepath='./lstm.keras',
)

testing_data = tensorflow.data.Dataset.load(
    path='./testing_data',
    compression='GZIP',
)

evaluation = model.evaluate(
    x=testing_data,
    return_dict=True,
    verbose=0,
)

testing_metrics: dict[str, any] = {
    'loss': evaluation['loss'],
    'mean_absolute_error': evaluation['mean_absolute_error'],
}

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
