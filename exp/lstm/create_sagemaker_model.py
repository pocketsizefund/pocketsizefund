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


arguments = parser.parse_args()

# hyperparameters
training_split = 0.8
epochs = 1

estimator = tensorflow.TensorFlow(
    entry_point='exp/lstm/train_lstm.py',
    role=arguments.iam_role,
    instance_count=1,
    instance_type='ml.m5.large',
    source_dir='.',
    image_uri=arguments.model_image_uri,
    model_dir='/opt/ml/model',
    hyperparameters={
        's3-data-bucket-name': arguments.s3_data_bucket_name,
        'training-split': training_split,
        'epochs': epochs,
    },
)

estimator.fit()
