import pulumi_aws as aws
from tags import common_tags


def create_cluster_role() -> aws.iam.Role:
    cluster_role = aws.iam.Role(
        resource_name="pocketsizefund-cluster-role",
        description="Role for EKS cluster to manage resources",
        name="pocketsizefund-cluster-role",
        assume_role_policy="""{
            "Version": "2012-10-17",
            "Statement": [
                {
                    "Effect": "Allow",
                    "Principal": {
                        "Service": [
                            "eks.amazonaws.com"
                        ]
                    },
                    "Action": "sts:AssumeRole"
                }
            ]
        }""",
        tags=common_tags,
    )

    aws.iam.RolePolicyAttachment(
        resource_name="pocketsizefund-cluster-role-eks-cluster-policy",
        role=cluster_role.name,
        policy_arn="arn:aws:iam::aws:policy/AmazonEKSClusterPolicy",
    )

    return cluster_role


def create_node_role() -> aws.iam.Role:
    node_role = aws.iam.Role(
        resource_name="pocketsizefund-node-role",
        description="Role for EKS worker nodes to manage resources",
        name="pocketsizefund-node-role",
        assume_role_policy="""{
            "Version": "2012-10-17",
            "Statement": [
                {
                    "Effect": "Allow",
                    "Principal": {
                        "Service": "ec2.amazonaws.com"
                    },
                    "Action": "sts:AssumeRole"
                }
            ]
        }""",
        tags=common_tags,
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
