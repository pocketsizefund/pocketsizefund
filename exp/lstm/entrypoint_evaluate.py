import argparse
import tarfile
import io
import os
import json
from datetime import datetime

import boto3
from keras import models
import tensorflow


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

metrics: dict[str, any] = {
    'loss': evaluation['loss'],
    'mean_absolute_error': evaluation['mean_absolute_error'],
}

now = datetime.now().strftime('%Y%m%d%H%M%S')

metrics_file = open('metrics/{}_metrics.json'.format(now), 'w')
json.dump(metrics, metrics_file)
