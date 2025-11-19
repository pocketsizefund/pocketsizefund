import os

import boto3
from loguru import logger
from sagemaker.estimator import Estimator
from sagemaker.inputs import TrainingInput
from sagemaker.session import Session

application_name = os.getenv("APPLICATION_NAME", "")

logger.info(f"Starting training job for application: {application_name}")

session = boto3.Session(profile_name=os.getenv("AWS_PROFILE", ""))

sagemaker_session = Session(boto_session=session)

image_uri = ""
s3_data_path = ""
if application_name == "equitypricemodel":
    image_uri = os.getenv("AWS_ECR_EQUITY_PRICE_MODEL_TRAINER_IMAGE_ARN", "")
    s3_data_path = os.getenv("AWS_S3_EQUITY_PRICE_MODEL_TRAINING_DATA_PATH", "")


estimator = Estimator(
    image_uri=image_uri,
    role=os.getenv("AWS_SAGEMAKER_ROLE_ARN", ""),
    instance_count=1,
    instance_type="ml.t3.xlarge",
    sagemaker_session=sagemaker_session,
    output_path=os.getenv("AWS_S3_EQUITY_PRICE_MODEL_ARTIFACT_OUTPUT_PATH", ""),
)

training_data_input = TrainingInput(
    s3_data=s3_data_path,
    content_type="application/x-parquet",
    input_mode="File",
)

estimator.fit({"train": training_data_input})
