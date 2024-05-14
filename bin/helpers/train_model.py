import argparse
import os

from sagemaker import tensorflow
import wandb

from pkg.config import config
from pkg.trade import trade


parser = argparse.ArgumentParser()

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

parser.add_argument(
    "--s3-data-bucket-name",
    type=str,
    dest="s3_data_bucket_name",
)

parser.add_argument(
    "--s3-artifacts-bucket-name",
    type=str,
    dest="s3_artifacts_bucket_name",
)

parser.add_argument(
    "--epochs",
    type=int,
    default=10,
    required=False,
    dest="epochs",
)

parser.add_argument(
    "--days",
    type=int,
    default=500,  # approximately two years of trading days
    required=False,
    dest="days",
)

arguments = parser.parse_args()

notes = input("enter notes (optional): ")

samconfig_file = config.SAMConfig(
    file_path="samconfig.toml",
    environment=config.ENVIRONMENT_DEVELOPMENT,
)

trade_client = trade.Client(
    darqube_api_key=samconfig_file.get_parameter("DarqubeAPIKey"),
    alpaca_api_key=samconfig_file.get_parameter("AlpacaAPIKey"),
    alpaca_api_secret=samconfig_file.get_parameter("AlpacaAPISecret"),
    is_paper=True,
)

available_tickers: list[str] = trade_client.get_available_tickers()

os.environ["WANDB_API_KEY"] = samconfig_file.get_parameter(
    "WeightsAndBiasesAPIKey",
)

wandb.sagemaker_auth(path="cmd/script/trainmodel")

estimator = tensorflow.TensorFlow(
    entry_point="",
    role=arguments.iam_role,
    instance_count=1,
    instance_type="ml.m4.xlarge",
    source_dir="",
    image_uri=arguments.model_image_uri,
    model_dir="/opt/ml/model",  # this is the required value
    hyperparameters={
        "s3-data-bucket-name": arguments.s3_data_bucket_name,
        "s3-artifacts-bucket-name": arguments.s3_artifacts_bucket_name,
        "epochs": arguments.epochs,
        "days": arguments.days,
        "available-tickers": ",".join(available_tickers),
        "notes": notes,
    },
    output_path="s3://{}/models".format(arguments.s3_artifacts_bucket_name),
)

estimator.fit()
