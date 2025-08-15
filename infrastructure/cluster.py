import json

import pulumi
import pulumi_aws as aws
import pulumi_eks as eks
import pulumi_kubernetes as k8s
from tags import pulumi_tags


def create_kubernetes_cluster(
    virtual_private_cloud: aws.ec2.Vpc,
    private_subnets: list[aws.ec2.Subnet],
    kubernetes_cluster_role: aws.iam.Role,
    kubernetes_node_role: aws.iam.Role,
) -> eks.Cluster:
    return eks.Cluster(
        resource_name="pocketsizefund-kubernetes-cluster",
        desired_capacity=2,
        min_size=1,
        max_size=3,
        instance_type="t3.small",
        service_role=kubernetes_cluster_role,
        instance_role=kubernetes_node_role,
        vpc_id=virtual_private_cloud.id,
        subnet_ids=[subnet.id for subnet in private_subnets],
        opts=pulumi.ResourceOptions(
            depends_on=[
                virtual_private_cloud,
                *private_subnets,
                kubernetes_cluster_role,
                kubernetes_node_role,
            ],
        ),
        tags=pulumi_tags,
    )


def create_kubernetes_provider(kubernetes_cluster: eks.Cluster) -> k8s.Provider:
    return k8s.Provider(
        resource_name="pocketsizefund-kubernetes-provider",
        kubeconfig=kubernetes_cluster.kubeconfig.apply(json.dumps),
        opts=pulumi.ResourceOptions(
            replace_on_changes=["kubeconfig"],
            custom_timeouts=pulumi.CustomTimeouts(
                create="10m",
                update="10m",
                delete="10m",
            ),
            depends_on=[kubernetes_cluster],
        ),
    )


def update_kubernetes_cluster_access(
    kubernetes_provider: k8s.Provider,
    kubernetes_cluster_role: aws.iam.Role,
    kubernetes_node_role: aws.iam.Role,
    pulumi_user_arn: pulumi.Output[str],
    root_user_arn: pulumi.Output[str],
) -> k8s.core.v1.ConfigMap:
    map_roles = pulumi.Output.json_dumps(
        [
            {
                "rolearn": kubernetes_node_role.arn,
                "username": "system:node:{{EC2PrivateDNSName}}",
                "groups": [
                    "system:bootstrappers",
                    "system:nodes",
                ],
            },
            {
                "rolearn": kubernetes_cluster_role.arn,
                "username": "system:master",
                "groups": [
                    "system:masters",
                ],
            },
        ]
    )

    map_users = pulumi.Output.json_dumps(
        [
            {
                "userarn": pulumi_user_arn,
                "username": "pulumi-user",
                "groups": ["system:masters"],
            },
            {
                "userarn": root_user_arn,
                "username": "root-user",
                "groups": ["system:masters"],
            },
        ]
    )

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
            "mapRoles": map_roles,
            "mapUsers": map_users,
        },
        opts=pulumi.ResourceOptions(
            replace_on_changes=["*"],
            provider=kubernetes_provider,
            depends_on=[
                kubernetes_provider,
                kubernetes_cluster_role,
                kubernetes_node_role,
            ],
        ),
    )


def create_kubernetes_cluster_role() -> aws.iam.Role:
    assume_role_policy = {
        "Version": "2012-10-17",
        "Statement": [
            {
                "Effect": "Allow",
                "Principal": {"Service": ["eks.amazonaws.com"]},
                "Action": "sts:AssumeRole",
            }
        ],
    }

    cluster_role = aws.iam.Role(
        resource_name="pocketsizefund-cluster-role",
        description="Role for EKS cluster to manage resources",
        name="pocketsizefund-cluster-role",
        assume_role_policy=json.dumps(assume_role_policy),
        tags=pulumi_tags,
    )

    aws.iam.RolePolicyAttachment(
        resource_name="pocketsizefund-cluster-policy",
        role=cluster_role.name,
        policy_arn="arn:aws:iam::aws:policy/AmazonEKSClusterPolicy",
    )

    return cluster_role


def create_kubernetes_node_role() -> aws.iam.Role:
    assume_role_policy = {
        "Version": "2012-10-17",
        "Statement": [
            {
                "Effect": "Allow",
                "Principal": {"Service": ["ec2.amazonaws.com"]},
                "Action": "sts:AssumeRole",
            }
        ],
    }

    node_role = aws.iam.Role(
        resource_name="pocketsizefund-node-role",
        description="Role for EKS worker nodes to manage resources",
        name="pocketsizefund-node-role",
        assume_role_policy=json.dumps(assume_role_policy),
        tags=pulumi_tags,
    )

    aws.iam.RolePolicyAttachment(
        resource_name="pocketsizefund-node-policy",
        role=node_role.name,
        policy_arn="arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy",
    )

    aws.iam.RolePolicyAttachment(
        resource_name="pocketsizefund-node-role-ecr-policy",
        role=node_role.name,
        policy_arn="arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly",
    )

    aws.iam.RolePolicyAttachment(
        resource_name="pocketsizefund-node-role-cni-policy",
        role=node_role.name,
        policy_arn="arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy",
    )

    return node_role
