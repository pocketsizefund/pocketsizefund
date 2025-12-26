import os
import sys
import tarfile

import boto3
import structlog

logger = structlog.get_logger()


def download_model_artifacts(
    application_name: str,
    artifacts_bucket: str,
) -> None:
    logger.info("Downloading model artifact", application_name=application_name)

    try:
        s3_client = boto3.client("s3")

    except Exception as e:
        logger.exception(
            "Error creating S3 client",
            error=f"{e}",
            application_name=application_name,
        )
        raise RuntimeError from e

    try:
        file_objects = s3_client.list_objects_v2(
            Bucket=artifacts_bucket,
            Prefix=f"artifacts/{application_name}",
        )
    except Exception as e:
        logger.exception("Error listing objects", bucket=artifacts_bucket, error=f"{e}")
        raise RuntimeError from e

    options = set()

    for file_object in file_objects.get("Contents", []):
        file_object_name = file_object["Key"]

        file_object_name_parts = file_object_name.split("/")

        if len(file_object_name_parts) < 2:  # noqa: PLR2004
            logger.warning("Skipping malformed path", path=file_object_name)
            continue

        options.add(file_object_name_parts[1])

    if not options:
        logger.error("No artifacts found", application_name=application_name)
        raise RuntimeError

    logger.info("Available artifacts", options=options)

    selected_option = input("Select an artifact to download: ")

    if selected_option not in options:
        logger.error(
            "Invalid selection",
            selected_option=selected_option,
            valid_options=options,
        )
        raise RuntimeError

    logger.info("Selected artifact", selected_option=selected_option)

    target_path = f"artifacts/{selected_option}/output/model.tar.gz"
    destination_directory = f"applications/{application_name}/src/{application_name}/"
    destination_path = os.path.join(destination_directory, "model.tar.gz")  # noqa: PTH118

    os.makedirs(destination_directory, exist_ok=True)  # noqa: PTH103

    try:
        s3_client.download_file(
            Bucket=artifacts_bucket,
            Key=target_path,
            Filename=destination_path,
        )
    except Exception as e:
        logger.exception(
            "Error downloading file",
            bucket=artifacts_bucket,
            key=target_path,
            error=f"{e}",
        )
        raise RuntimeError from e

    try:
        with tarfile.open(destination_path, "r:gz") as tar:
            tar.extractall(path=destination_directory, filter="data")
    except Exception as e:
        logger.exception(
            "Error extracting tar file",
            path=destination_path,
            error=f"{e}",
        )
        raise RuntimeError from e

    logger.info("Artifact downloaded and extracted successfully")


if __name__ == "__main__":
    application_name = os.getenv("APPLICATION_NAME", "")
    artifacts_bucket = os.getenv("AWS_S3_ARTIFACTS_BUCKET_NAME", "")

    environment_variables = {
        "APPLICATION_NAME": application_name,
        "AWS_S3_ARTIFACTS_BUCKET_NAME": artifacts_bucket,
    }
    for environment_variable in [
        application_name,
        artifacts_bucket,
    ]:
        if not environment_variable:
            logger.error(
                "Missing required environment variable(s)",
                **environment_variables,
            )
            sys.exit(1)

    try:
        download_model_artifacts(
            application_name=application_name,
            artifacts_bucket=artifacts_bucket,
        )
    except Exception as e:
        logger.exception("Failed to download model artifacts", error=f"{e}")
        sys.exit(1)
