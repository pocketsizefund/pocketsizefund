import sys

import boto3
from sagemaker import tensorflow

from pkg.config import config


model_data = sys.argv[1]
role = sys.argv[2]
image_uri = sys.argv[3]

samconfig_file = config.SAMConfig(
    'samconfig.toml',
    config.ENVIRONMENT_DEVELOPMENT,
)

sagemaker_client = boto3.client('sagemaker')

endpoints = sagemaker_client.list_endpoints(
    NameContains='pocketsizefund-lstm',
)

for endpoint in endpoints['Endpoints']:
    if endpoint['EndpointName'] == 'pocketsizefund-lstm':
        sagemaker_client.delete_endpoint_config(
            EndpointConfigName='pocketsizefund-lstm',
        )

        sagemaker_client.delete_endpoint(
            EndpointName='pocketsizefund-lstm',
        )

model = tensorflow.TensorFlowModel(
    model_data=model_data,
    role=role,
    image_uri=image_uri,
    env={
        'S3_DATA_BUCKET_NAME': samconfig_file.get_parameter('S3DataBucketName'),
        'ALPHA_VANTAGE_API_KEY': samconfig_file.get_parameter('AlphaVantageAPIKey'),
        'ALPACA_API_KEY': samconfig_file.get_parameter('AlpacaAPIKey'),
        'ALPACA_API_SECRET': samconfig_file.get_parameter('AlpacaAPISecret'),
        'MODEL_DIR': '/opt/ml/model',
    }
)

predictor = model.deploy(
    initial_instance_count=1,
    instance_type='ml.m5.large',
    endpoint_name='pocketsizefund-lstm',
)
