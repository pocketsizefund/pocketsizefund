"""Deploy model artifact to SageMaker endpoint."""  # noqa: INP001
import argparse

import boto3
from sagemaker import tensorflow
from sagemaker.serverless import serverless_inference_config

from pkg.config import config

parser = argparse.ArgumentParser()

parser.add_argument(
    "--environment",
    type=str,
    dest="environment",
)

parser.add_argument(
    "--model-data",
    type=str,
    dest="model_data",
)

parser.add_argument(
    "--iam-role",
    type=str,
    dest="iam_role",
)

parser.add_argument(
    "--model-image-uri",
    type=str,
    dest="model_image_uri",
)

arguments = parser.parse_args()

samconfig_file = config.SAMConfig(
    "samconfig.toml",
    config.ENVIRONMENT_DEVELOPMENT,
)

sagemaker_client = boto3.client("sagemaker")

<<<<<<< HEAD
endpoint_name = f"pocketsizefund-{arguments.environment}-lstm"
=======
endpoint_name = "pocketsizefund-{}-lstm".format(arguments.environment)
>>>>>>> 9e24e11 (ruff - double quotes preferred)

try:
    sagemaker_client.delete_endpoint_config(
        EndpointConfigName=endpoint_name,
    )

    sagemaker_client.delete_endpoint(
        EndpointName=endpoint_name,
    )

except Exception as exception:  # noqa: BLE001
    print(f"exception: {exception}")  # noqa: T201

model = tensorflow.TensorFlowModel(
    model_data=arguments.model_data,
    role=arguments.iam_role,
    image_uri=arguments.model_image_uri,
    env={
        "ALPACA_API_KEY": samconfig_file.get_parameter("AlpacaAPIKey"),
        "ALPACA_API_SECRET": samconfig_file.get_parameter("AlpacaAPISecret"),
        "MODEL_DIR": "/opt/ml/model",
<<<<<<< HEAD
    },
=======
    }
>>>>>>> 9e24e11 (ruff - double quotes preferred)
)

predictor = model.deploy(
    initial_instance_count=1,
    instance_type="ml.m4.xlarge",
    endpoint_name=endpoint_name,
    serverless_inference_config=serverless_inference_config.ServerlessInferenceConfig(
        memory_size_in_mb=2048,  # default
    ),
)

cloudwatch_client = boto3.client("logs")

<<<<<<< HEAD
endpoint_name = f"pocketsizefund-{arguments.environment}-lstm"

log_groups = cloudwatch_client.describe_log_groups(
    logGroupNamePrefix=f"/aws/sagemaker/Endpoints/{endpoint_name}",
=======
endpoint_name = "pocketsizefund-{}-lstm".format(arguments.environment)

log_groups = cloudwatch_client.describe_log_groups(
    logGroupNamePrefix="/aws/sagemaker/Endpoints/{}".format(endpoint_name),
>>>>>>> 9e24e11 (ruff - double quotes preferred)
)["logGroups"]

for log_group in log_groups:
    cloudwatch_client.response = cloudwatch_client.put_retention_policy(
        logGroupName=log_group["logGroupName"],
        retentionInDays=3,
    )
