import os

from dotenv import load_dotenv
from sagemaker.estimator import Estimator
from sagemaker.inputs import TrainingInput
from sagemaker.session import Session

sagemaker_session = Session()

load_dotenv()

estimator = Estimator(
    image_uri=os.getenv("AWS_ECR_EQUITY_PRICE_MODEL_TRAINING_IMAGE_ARN", ""),
    role=os.getenv("AWS_SAGEMAKER_ROLE_ARN", ""),
    instance_count=1,
    instance_type="ml.t3.xlarge",
    sagemaker_session=sagemaker_session,
    output_path=os.getenv("AWS_S3_EQUITY_PRICE_MODEL_ARTIFACT_OUTPUT_PATH", ""),
)

training_data_input = TrainingInput(
    s3_data=os.getenv("AWS_S3_EQUITY_PRICE_MODEL_TRAINING_DATA_PATH", ""),
    content_type="application/x-parquet",
    input_mode="File",
)

estimator.fit({"train": training_data_input})
