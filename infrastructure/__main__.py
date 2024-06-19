import base64
import os

import pulumi
import pulumi_awsx as awsx
import pulumi_eks as eks
import pulumi_kubernetes as k8s

darqube_api_key = os.getenv("DARQUBE_API_KEY")
darqube_api_key = base64.b64encode(darqube_api_key.encode()).decode()

alpaca_api_key = os.getenv("ALPACA_API_KEY")
alpaca_api_key = base64.b64encode(alpaca_api_key.encode()).decode()

alpaca_api_secret = os.getenv("ALPACA_API_SECRET")
alpaca_api_secret = base64.b64encode(alpaca_api_secret.encode()).decode()

alpha_vantage_api_key = os.getenv("ALPHA_VANTAGE_API_KEY")
alpha_vantage_api_key = base64.b64encode(alpha_vantage_api_key.encode()).decode()

edgar_user_agent = os.getenv("EDGAR_USER_AGENT")
edgar_user_agent = base64.b64encode(edgar_user_agent.encode()).decode()

model_file_name = os.getenv("MODEL_FILE_NAME")
model_file_name = base64.b64encode(model_file_name.encode()).decode()

config = pulumi.Config()
min_cluster_size = config.get_int("minClusterSize", 3)
max_cluster_size = config.get_int("maxClusterSize", 6)
desired_cluster_size = config.get_int("desiredClusterSize", 3)
eks_node_instance_type = config.get("eksNodeInstanceType", "t3.medium")
vpc_network_cidr = config.get("vpcNetworkCidr", "10.0.0.0/16")

eks_vpc = awsx.ec2.Vpc("eks-vpc", enable_dns_hostnames=True, cidr_block=vpc_network_cidr)

eks_cluster = eks.Cluster(
    "eks-cluster",
    vpc_id=eks_vpc.vpc_id,
    public_subnet_ids=eks_vpc.public_subnet_ids,
    private_subnet_ids=eks_vpc.private_subnet_ids,
    instance_type=eks_node_instance_type,
    desired_capacity=desired_cluster_size,
    min_size=min_cluster_size,
    max_size=max_cluster_size,
    node_associate_public_ip_address=False,
    endpoint_private_access=False,
    endpoint_public_access=True,
)


darqube_api_key = base64.b64encode(darqube_api_key.encode()).decode()
alpaca_api_key = base64.b64encode(alpaca_api_key.encode()).decode()
alpaca_api_secret = base64.b64encode(alpaca_api_secret.encode()).decode()
alpha_vantage_api_key = base64.b64encode(alpha_vantage_api_key.encode()).decode()
edgar_user_agent = base64.b64encode(edgar_user_agent.encode()).decode()
model_file_name = base64.b64encode(model_file_name.encode()).decode()


def create_secret(name, data, namespace="default"):
    return k8s.core.v1.Secret(
        name,
        metadata=k8s.meta.v1.ObjectMetaArgs(
            name=name,
            namespace=namespace,
        ),
        data=data,
        type="Opaque",
    )


secrets = create_secret(
    "platform",
    {
        "darqube_api_key": darqube_api_key,
        "alpaca_api_key": alpaca_api_key,
        "alpaca_api_secret": alpaca_api_secret,
        "alpha_vantage_api_key": alpha_vantage_api_key,
        "edgar_user_agent": edgar_user_agent,
        "model_file_name": model_file_name,
    },
)

pulumi.export("kubeconfig", eks_cluster.kubeconfig)
pulumi.export("vpcId", eks_vpc.vpc_id)
