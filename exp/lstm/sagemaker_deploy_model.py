import argparse

import boto3
from sagemaker import tensorflow

from pkg.config import config


parser = argparse.ArgumentParser(
    prog='sagemaker deploy script',
    description='deploy the lstm model on sagemaker',
)

parser.add_argument(
    '--environment',
    type=str,
    dest='environment',
)

parser.add_argument(
    '--model-data',
    type=str,
    dest='model_data',
)

parser.add_argument(
    '--iam-role',
    type=str,
    dest='iam_role',
)

parser.add_argument(
    '--model-image-uri',
    type=str,
    dest='model_image_uri',
)

arguments = parser.parse_args()

samconfig_file = config.SAMConfig(
    'samconfig.toml',
    config.ENVIRONMENT_DEVELOPMENT,
)

sagemaker_client = boto3.client('sagemaker')

endpoint_name = 'pocketsizefund-{}-lstm'.format(arguments.environment)

endpoints = sagemaker_client.list_endpoints(
    NameContains=endpoint_name,
)

for endpoint in endpoints['Endpoints']:
    if endpoint['EndpointName'] == endpoint_name:
        sagemaker_client.delete_endpoint_config(
            EndpointConfigName=endpoint_name,
        )

        sagemaker_client.delete_endpoint(
            EndpointName=endpoint_name,
        )

model = tensorflow.TensorFlowModel(
    model_data=arguments.model_data,
    role=arguments.iam_role,
    image_uri=arguments.model_image_uri,
    env={
        'S3_DATA_BUCKET_NAME': samconfig_file.get_parameter('S3DataBucketName'),
        'ALPACA_API_KEY': samconfig_file.get_parameter('AlpacaAPIKey'),
        'ALPACA_API_SECRET': samconfig_file.get_parameter('AlpacaAPISecret'),
        'MODEL_DIR': '/opt/ml/model',
    }
)

predictor = model.deploy(
    initial_instance_count=1,
    instance_type='ml.m4.xlarge',
    endpoint_name=endpoint_name,
)
