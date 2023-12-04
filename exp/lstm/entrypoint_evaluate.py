import argparse
import tarfile
import io
import pickle
import os
import json
from datetime import datetime

import boto3
from keras import models


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


def convert_integer_to_ticker(integer: int) -> str:
    return integer.to_bytes((integer.bit_length() + 7) // 8, 'little').decode()


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

testing_data_file = open('testing_data.pkl', 'rb')

testing_data = pickle.load(testing_data_file)

ticker_metrics: dict[str, any] = {}

count: int = len(testing_data.keys())

average_metrics: dict[str, float] = {
    'average_loss': 0.0,
    'average_accuracy': 0.0,
    'average_mse': 0.0,
    'average_mae': 0.0,
}

for ticker, ticker_data in testing_data.items():
    testing_input_data = ticker_data['input']
    testing_output_data = ticker_data['output']

    testing_input_data = testing_input_data.reshape(
        testing_input_data.shape[0],
        1,
        testing_input_data.shape[1],
    )

    testing_output_data = testing_output_data.reshape(
        testing_output_data.shape[0],
        1,
        testing_output_data.shape[1],
    )

    evaluation = model.evaluate(
        x=testing_input_data,
        y=testing_output_data,
        return_dict=True,
        verbose=0,
    )

    ticker_metrics[convert_integer_to_ticker(ticker)] = evaluation

    average_metrics['average_loss'] += evaluation['loss']
    average_metrics['average_accuracy'] += evaluation['accuracy']
    average_metrics['average_mse'] += evaluation['mse']
    average_metrics['average_mae'] += evaluation['mae']


average_metrics['average_loss'] /= count
average_metrics['average_accuracy'] /= count
average_metrics['average_mse'] /= count
average_metrics['average_mae'] /= count

now = datetime.now().strftime('%Y%m%d%H%M%S')

all_metrics: dict[str, any] = {
    'ticker_metrics': ticker_metrics,
    'average_metrics': average_metrics,
}

metrics_file = open('metrics/{}_metrics.json'.format(now), 'w')
json.dump(all_metrics, metrics_file)
