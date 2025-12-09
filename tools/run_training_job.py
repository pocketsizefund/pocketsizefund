import os

import boto3
from loguru import logger
from sagemaker.estimator import Estimator
from sagemaker.inputs import TrainingInput
from sagemaker.session import Session

application_name = os.getenv("APPLICATION_NAME", "")

logger.info(f"Starting training job for application: {application_name}")


def run_training_job() -> None:
    application_name = os.getenv("APPLICATION_NAME")
    if not application_name:
        message = "APPLICATION_NAME environment variable is required"
        raise ValueError(message)

    logger.info("starting_training_job", application_name=application_name)

    aws_profile = os.getenv("AWS_PROFILE")
    if not aws_profile:
        message = "AWS_PROFILE environment variable is required"
        raise ValueError(message)

    session = boto3.Session(profile_name=aws_profile)
    sagemaker_session = Session(boto_session=session)

    if application_name == "equitypricemodel":
        image_uri = os.getenv("AWS_ECR_EQUITY_PRICE_MODEL_TRAINER_IMAGE_ARN", "")
        s3_data_path = os.getenv("AWS_S3_EQUITY_PRICE_MODEL_TRAINING_DATA_PATH", "")
        sagemaker_role = os.getenv("AWS_SAGEMAKER_ROLE_ARN", "")
        output_path = os.getenv("AWS_S3_EQUITY_PRICE_MODEL_ARTIFACT_OUTPUT_PATH", "")

        if not all([image_uri, s3_data_path, sagemaker_role, output_path]):
            message = (
                "Missing required environment variables for equitypricemodel training"
            )
            raise ValueError(message)
    else:
        message = f"Unsupported application name: {application_name}"
        raise ValueError(message)

    estimator = Estimator(
        image_uri=image_uri,
        role=sagemaker_role,
        instance_count=1,
        instance_type="ml.t3.xlarge",
        sagemaker_session=sagemaker_session,
        output_path=output_path,
    )

    training_data_input = TrainingInput(
        s3_data=s3_data_path,
        content_type="application/x-parquet",
        input_mode="File",
    )

    estimator.fit({"train": training_data_input})


if __name__ == "__main__":
    run_training_job()
