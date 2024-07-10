
import pulumi
import pulumi_awsx as awsx
import pulumi_eks as eks
import pulumi_kubernetes as k8s

NAMESPACES = ["development", "paper", "live"]

config = pulumi.Config()
min_cluster_size = config.get_int("minClusterSize", 3)
max_cluster_size = config.get_int("maxClusterSize", 6)
desired_cluster_size = config.get_int("desiredClusterSize", 3)
eks_node_instance_type = config.get("eksNodeInstanceType", "t3.medium")
vpc_network_cidr = config.get("vpcNetworkCidr", "10.0.0.0/16")

eks_vpc = awsx.ec2.Vpc("eks-vpc",
    enable_dns_hostnames=True,
    cidr_block=vpc_network_cidr,
    nat_gateways=awsx.ec2.NatGatewayConfigurationArgs(
        strategy=awsx.ec2.NatGatewayStrategy.ONE_PER_AZ,
    ),
)

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

kubeconfig = eks_cluster.kubeconfig
k8s_provider = k8s.Provider("k8s-provider", kubeconfig=kubeconfig)




for namespace in NAMESPACES:
    k8s.core.v1.Namespace(
        namespace,
        metadata={"name": namespace},
        opts=pulumi.ResourceOptions(provider=k8s_provider),
    )


    k8s.core.v1.Secret(f"platform-secret-{namespace}",
        metadata=k8s.meta.v1.ObjectMetaArgs(
            name="platform",
            namespace=namespace,
        ),
        string_data={
            "username": "admin",
            "password": "supersecret",
        },
        opts=pulumi.ResourceOptions(provider=k8s_provider),
    )


pulumi.export("kubeconfig", eks_cluster.kubeconfig)
pulumi.export("vpcId", eks_vpc.vpc_id)
