import os
import sys

import boto3
import structlog
from sagemaker.estimator import Estimator
from sagemaker.inputs import TrainingInput
from sagemaker.session import Session

logger = structlog.get_logger()


def run_training_job(  # noqa: PLR0913
    application_name: str,
    aws_profile: str,
    image_uri: str,
    s3_data_path: str,
    sagemaker_role: str,
    output_path: str,
) -> None:
    logger.info("Starting training job", application_name=application_name)

    try:
        session = boto3.Session(profile_name=aws_profile)
        sagemaker_session = Session(boto_session=session)
    except Exception as e:
        logger.exception(
            "Error creating SageMaker session",
            error=f"{e}",
            application_name=application_name,
        )
        raise RuntimeError from e

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

    try:
        estimator.fit({"train": training_data_input})
    except Exception as e:
        logger.exception(
            "Error during training job",
            error=f"{e}",
            application_name=application_name,
        )
        raise RuntimeError from e


if __name__ == "__main__":
    application_name = os.getenv("APPLICATION_NAME", "")
    aws_profile = os.getenv("AWS_PROFILE", "")
    image_uri = os.getenv("AWS_ECR_EQUITY_PRICE_MODEL_TRAINER_IMAGE_ARN", "")
    s3_data_path = os.getenv("AWS_S3_EQUITY_PRICE_MODEL_TRAINING_DATA_PATH", "")
    sagemaker_role = os.getenv("AWS_SAGEMAKER_ROLE_ARN", "")
    output_path = os.getenv("AWS_S3_EQUITY_PRICE_MODEL_ARTIFACT_OUTPUT_PATH", "")

    environment_variables = {
        "APPLICATION_NAME": application_name,
        "AWS_PROFILE": aws_profile,
        "AWS_ECR_EQUITY_PRICE_MODEL_TRAINER_IMAGE_ARN": image_uri,
        "AWS_S3_EQUITY_PRICE_MODEL_TRAINING_DATA_PATH": s3_data_path,
        "AWS_SAGEMAKER_ROLE_ARN": sagemaker_role,
        "AWS_S3_EQUITY_PRICE_MODEL_ARTIFACT_OUTPUT_PATH": output_path,
    }

    for value in environment_variables.values():
        if not value:
            logger.error(
                "Missing required environment variable(s)",
                **environment_variables,
            )
            sys.exit(1)

    try:
        run_training_job(
            application_name=application_name,
            aws_profile=aws_profile,
            image_uri=image_uri,
            s3_data_path=s3_data_path,
            sagemaker_role=sagemaker_role,
            output_path=output_path,
        )
    except Exception as e:
        logger.exception(
            "Training job failed",
            error=f"{e}",
            application_name=application_name,
        )
        sys.exit(1)
