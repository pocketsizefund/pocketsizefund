import argparse

from sagemaker import tensorflow


parser = argparse.ArgumentParser(
    prog='sagemaker training script',
    description='train the lstm model on sagemaker',
)

parser.add_argument(
    '--iam-role',
    type=str,
    dest='iam_role',
)

parser.add_argument(
    '--model-image-uri',
    type=str,
    dest='model_image_uri',
)

parser.add_argument(
    '--s3-data-bucket-name',
    type=str,
    dest='s3_data_bucket_name',
)

parser.add_argument(
    '--s3-artifacts-bucket-name',
    type=str,
    dest='s3_artifacts_bucket_name',
)

arguments = parser.parse_args()

estimator = tensorflow.TensorFlow(
    entry_point='',
    role=arguments.iam_role,
    instance_count=1,
    instance_type='ml.m4.xlarge',
    source_dir='',
    image_uri=arguments.model_image_uri,
    model_dir='/opt/ml/model',  # this is the required value
    hyperparameters={
        's3-data-bucket-name': arguments.s3_data_bucket_name,
    },
    output_path='s3://{}/models'.format(arguments.s3_artifacts_bucket_name),
)

estimator.fit()
