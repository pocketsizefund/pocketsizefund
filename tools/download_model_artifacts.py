import os
import tarfile

import boto3
import structlog

logger = structlog.get_logger()

application_name = os.getenv("APPLICATION_NAME")


logger.info("Downloading model artifact", application_name=application_name)

session = boto3.Session(profile_name=os.getenv("AWS_PROFILE", ""))

s3_client = session.client("s3")

artifacts_bucket = os.getenv("AWS_S3_ARTIFACTS_BUCKET_NAME", "")

file_objects = s3_client.list_objects_v2(
    Bucket=artifacts_bucket,
    Prefix=f"artifacts/{application_name}",
)

options = set()

for file_object in file_objects.get("Contents", []):
    file_object_name = file_object["Key"]

    file_object_name_parts = file_object_name.split("/")

    options.add(file_object_name_parts[1])

logger.info("Available artifacts", options=options)

selected_option = input("Select an artifact to download: ")

logger.info("Selected artifact", selected_option=selected_option)

target_path = f"artifacts/{selected_option}/output/model.tar.gz"
destination_directory = f"applications/{application_name}/src/{application_name}/"
destination_path = os.path.join(destination_directory, "model.tar.gz")  # noqa: PTH118

s3_client.download_file(
    Bucket=artifacts_bucket,
    Key=target_path,
    Filename=destination_path,
)

with tarfile.open(destination_path, "r:gz") as tar:
    tar.extractall(path=destination_directory, filter="data")

logger.info("Artifact downloaded and extracted successfully")
