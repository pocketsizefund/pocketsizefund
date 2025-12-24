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
    trainer_image_uri: str,
    s3_data_path: str,
    iam_sagemaker_role_arn: str,
    s3_artifact_path: str,
    iam_development_role_arn: str,
) -> None:
    logger.info("Starting training job", application_name=application_name)

    try:
        sts = boto3.client("sts")
        assume_role_response = sts.assume_role(
            RoleArn=iam_development_role_arn,
            RoleSessionName="sagemaker-training-job-session",
        )
        credentials = assume_role_response["Credentials"]

        session = boto3.Session(
            aws_access_key_id=credentials["AccessKeyId"],
            aws_secret_access_key=credentials["SecretAccessKey"],
            aws_session_token=credentials["SessionToken"],
        )

        sagemaker_session = Session(boto_session=session)

    except Exception as e:
        logger.exception(
            "Error creating SageMaker session",
            error=f"{e}",
            application_name=application_name,
        )
        raise RuntimeError from e

    estimator = Estimator(
        image_uri=trainer_image_uri,
        role=iam_sagemaker_role_arn,
        instance_count=1,
        instance_type="ml.t3.xlarge",
        sagemaker_session=sagemaker_session,
        output_path=s3_artifact_path,
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
    trainer_image_uri = os.getenv("AWS_ECR_EQUITY_PRICE_MODEL_TRAINER_IMAGE_ARN", "")
    s3_data_path = os.getenv("AWS_S3_EQUITY_PRICE_MODEL_TRAINING_DATA_PATH", "")
    iam_sagemaker_role_arn = os.getenv("AWS_IAM_SAGEMAKER_ROLE_ARN", "")
    s3_artifact_path = os.getenv("AWS_S3_EQUITY_PRICE_MODEL_ARTIFACT_OUTPUT_PATH", "")
    iam_development_role_arn = os.getenv("AWS_IAM_DEVELOPMENT_ROLE_ARN", "")

    environment_variables = {
        "APPLICATION_NAME": application_name,
        "AWS_ECR_EQUITY_PRICE_MODEL_TRAINER_IMAGE_ARN": trainer_image_uri,
        "AWS_S3_EQUITY_PRICE_MODEL_TRAINING_DATA_PATH": s3_data_path,
        "AWS_IAM_SAGEMAKER_ROLE_ARN": iam_sagemaker_role_arn,
        "AWS_S3_EQUITY_PRICE_MODEL_ARTIFACT_OUTPUT_PATH": s3_artifact_path,
        "AWS_IAM_DEVELOPMENT_ROLE_ARN": iam_development_role_arn,
    }

    missing_environment_variables = [
        key for key, value in environment_variables.items() if not value
    ]
    if missing_environment_variables:
        logger.error(
            "Missing required environment variables",
            missing_environment_variables=missing_environment_variables,
            **environment_variables,
        )
        sys.exit(1)

    try:
        run_training_job(
            application_name=application_name,
            trainer_image_uri=trainer_image_uri,
            s3_data_path=s3_data_path,
            iam_sagemaker_role_arn=iam_sagemaker_role_arn,
            s3_artifact_path=s3_artifact_path,
            iam_development_role_arn=iam_development_role_arn,
        )

    except Exception as e:
        logger.exception(
            "Training job failed",
            error=f"{e}",
            application_name=application_name,
        )
        sys.exit(1)
