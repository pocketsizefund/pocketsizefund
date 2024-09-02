import boto3

AWS_REGION = "us-east-1"
CLUSTER_NAME = "pocketsizefund"
CLUSTER_TAGS = {"name": "pocketsizefund", "domain": "fund"}
VPC_CIDR = "10.0.0.0/16"
AVAILABILITY_ZONES = ["us-east-1a", "us-east-1b", "us-east-1c"]
SUBNET_CIDRS = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
NODE_GROUP_MIN_SIZE = 3
NODE_GROUP_MAX_SIZE = 6
NODE_GROUP_DESIRED_SIZE = 3
NODE_GROUP_INSTANCE_TYPE = "t3.medium"

ec2_client = boto3.client('ec2', region_name=AWS_REGION)
eks_client = boto3.client('eks', region_name=AWS_REGION)
iam_client = boto3.client('iam', region_name=AWS_REGION)
