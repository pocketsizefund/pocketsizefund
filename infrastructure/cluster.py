import json

import pulumi
import pulumi_aws as aws
import pulumi_eks as eks
import pulumi_kubernetes as k8s
from tags import common_tags


def create_kubernetes_cluster(
    cluster_role: aws.iam.Role,
    node_role: aws.iam.Role,
) -> eks.Cluster:
    virtual_private_cloud = aws.ec2.Vpc(
        resource_name="pocketsizefund-virtual-private-cloud",
        cidr_block="10.0.0.0/16",
        enable_dns_hostnames=True,
        enable_dns_support=True,
        tags=common_tags,
    )

    internet_gateway = aws.ec2.InternetGateway(
        resource_name="pocketsizefund-internet-gateway",
        vpc_id=virtual_private_cloud.id,
        tags=common_tags,
    )

    public_route_table = aws.ec2.RouteTable(
        resource_name="pocketsizefund-public-route-table",
        vpc_id=virtual_private_cloud.id,
        routes=[
            {
                "cidr_block": "0.0.0.0/0",
                "gateway_id": internet_gateway.id,
            }
        ],
        tags=common_tags,
    )

    availability_zones = aws.get_availability_zones(
        state="available",
        filters=[
            {
                "name": "region-name",
                "values": ["us-east-1"],
            }
        ],
    ).names[:2]

    public_subnets: list[aws.ec2.Subnet] = []
    for i, availability_zone in enumerate(availability_zones):
        subnet = aws.ec2.Subnet(
            resource_name=f"pocketsizefund-public-subnet-{i}",
            vpc_id=virtual_private_cloud.id,
            cidr_block=f"10.0.{i}.0/24",
            availability_zone=availability_zone,
            map_public_ip_on_launch=True,
            tags=common_tags,
        )

        aws.ec2.RouteTableAssociation(
            resource_name=f"pocketsizefund-public-route-table-association-{i}",
            subnet_id=subnet.id,
            route_table_id=public_route_table.id,
        )

        public_subnets.append(subnet)

    nat_elastic_ip = aws.ec2.Eip(
        resource_name="pocketsizefund-nat-elastic-ip",
        domain="vpc",
        tags=common_tags,
    )

    nat_gateway = aws.ec2.NatGateway(
        resource_name="pocketsizefund-nat-gateway",
        allocation_id=nat_elastic_ip.id,
        subnet_id=public_subnets[0].id,
        opts=pulumi.ResourceOptions(depends_on=[internet_gateway]),
        tags=common_tags,
    )

    private_route_table = aws.ec2.RouteTable(
        resource_name="pocketsizefund-private-route-table",
        vpc_id=virtual_private_cloud.id,
        routes=[
            {
                "cidr_block": "0.0.0.0/0",
                "nat_gateway_id": nat_gateway.id,
            }
        ],
        tags=common_tags,
    )

    private_subnets: list[aws.ec2.Subnet] = []
    for i, availability_zone in enumerate(availability_zones):
        subnet = aws.ec2.Subnet(
            resource_name=f"pocketsizefund-private-subnet-{i}",
            vpc_id=virtual_private_cloud.id,
            cidr_block=f"10.0.{i + 10}.0/24",
            availability_zone=availability_zone,
            tags=common_tags,
        )

        aws.ec2.RouteTableAssociation(
            resource_name=f"pocketsizefund-private-route-table-association-{i}",
            subnet_id=subnet.id,
            route_table_id=private_route_table.id,
        )
        private_subnets.append(subnet)

    cluster = eks.Cluster(
        resource_name="pocketsizefund-cluster",
        desired_capacity=2,
        min_size=1,
        max_size=3,
        instance_type="t3.small",
        instance_role=node_role,
        service_role=cluster_role,
        vpc_id=virtual_private_cloud.id,
        private_subnet_ids=[subnet.id for subnet in private_subnets],
        public_subnet_ids=[subnet.id for subnet in public_subnets],
        tags=common_tags,
    )

    pulumi.export("KUBECONFIG", cluster.kubeconfig)
    pulumi.export("CLUSTER_NAME", cluster.pulumi_resource_type)
    pulumi.export("VPC_ID", virtual_private_cloud.id)

    return cluster


def create_kubernetes_provider(cluster: eks.Cluster) -> k8s.Provider:
    return k8s.Provider(
        resource_name="pocketsizefund-kubernetes-provider",
        kubeconfig=cluster.kubeconfig.apply(json.dumps),
        opts=pulumi.ResourceOptions(
            depends_on=[cluster],
            custom_timeouts=pulumi.CustomTimeouts(
                create="10m",
                update="10m",
                delete="10m",
            ),
        ),
    )


def update_kubernetes_cluster_access(
    cluster_role: aws.iam.Role,
    node_role: aws.iam.Role,
    kubernetes_provider: k8s.Provider,
    pulumi_user_arn: pulumi.Output[str],
    root_user_arn: pulumi.Output[str],
) -> k8s.core.v1.ConfigMap:
    return k8s.core.v1.ConfigMap(
        resource_name="pocketsizefund-aws-auth",
        metadata=k8s.meta.v1.ObjectMetaArgs(
            name="aws-auth",  # name required by EKS
            namespace="kube-system",
            annotations={
                "pulumi.com/patchForce": "true",
            },
        ),
        data={
            "mapRoles": pulumi.Output.concat(
                "- rolearn: ",
                node_role.arn,
                "\n",
                "  username: system:node:{{EC2PrivateDNSName}}\n",
                "  groups:\n",
                "    - system:bootstrappers\n",
                "    - system:nodes\n",
                "- rolearn: ",
                cluster_role.arn,
                "\n",
                "  username: system:master\n",
                "  groups:\n",
                "    - system:masters\n",
            ),
            "mapUsers": pulumi.Output.concat(
                "- userarn: ",
                pulumi_user_arn,
                "\n",
                "  username: pulumi-user\n",
                "  groups:\n",
                "    - system:masters\n",
                "- userarn: ",
                root_user_arn,
                "\n",
                "  username: root-user\n",
                "  groups:\n",
                "    - system:masters\n",
            ),
        },
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            replace_on_changes=["*"],
            depends_on=[cluster_role, node_role],
        ),
    )
