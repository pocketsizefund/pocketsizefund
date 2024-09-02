from invoke import task
from rich.console import Console

from steps import CLUSTER_NAME, NODE_GROUP_DESIRED_SIZE, NODE_GROUP_INSTANCE_TYPE, NODE_GROUP_MAX_SIZE, NODE_GROUP_MIN_SIZE, eks_client, iam_client
import json

console = Console()

@task
def create(c, subnet_ids):
    console.print("[blue]Creating Node Group...[/blue]")
    trust_relationship = {
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
    }
    node_role = iam_client.create_role(
        RoleName=f"{CLUSTER_NAME}-node-role",
        AssumeRolePolicyDocument=json.dumps(trust_relationship)
    )
    iam_client.attach_role_policy(
        RoleName=f"{CLUSTER_NAME}-node-role",
        PolicyArn="arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy"
    )
    iam_client.attach_role_policy(
        RoleName=f"{CLUSTER_NAME}-node-role",
        PolicyArn="arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy"
    )
    iam_client.attach_role_policy(
        RoleName=f"{CLUSTER_NAME}-node-role",
        PolicyArn="arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly"
    )

    try:
        node_group = eks_client.create_nodegroup(
            clusterName=CLUSTER_NAME,
            nodegroupName=f"{CLUSTER_NAME}-node-group",
            nodeRole=node_role['Role']['Arn'],
            subnets=subnet_ids,
            instanceTypes=[NODE_GROUP_INSTANCE_TYPE],
            scalingConfig={
                'minSize': NODE_GROUP_MIN_SIZE,
                'maxSize': NODE_GROUP_MAX_SIZE,
                'desiredSize': NODE_GROUP_DESIRED_SIZE
            }
        )
        console.print(f"[green]Node Group creation initiated: {node_group['nodegroup']['nodegroupName']}[/green]")

        waiter = eks_client.get_waiter('nodegroup_active')
        waiter.wait(clusterName=CLUSTER_NAME, nodegroupName=f"{CLUSTER_NAME}-node-group")
        console.print("[green]Node Group is now active[/green]")
    except eks_client.exceptions.ClientError as e:
        console.print(f"[red]Error creating Node Group: {str(e)}[/red]")
        raise


@task
def delete(c):
    console.print("[blue]Deleting Node Group...[/blue]")
    try:
        eks_client.delete_nodegroup(clusterName=CLUSTER_NAME, nodegroupName=f"{CLUSTER_NAME}-node-group")
        waiter = eks_client.get_waiter('nodegroup_deleted')
        waiter.wait(clusterName=CLUSTER_NAME, nodegroupName=f"{CLUSTER_NAME}-node-group")
        console.print("[green]Node Group deleted[/green]")
    except eks_client.exceptions.ResourceNotFoundException:
        console.print("[yellow]Node Group not found[/yellow]")


