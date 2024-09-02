import subprocess
import json
from invoke import task
from rich.console import Console
from rich.progress import Progress
from steps import CLUSTER_NAME, AWS_REGION
from steps import eks_client, iam_client

console = Console()

@task
def create(c, subnet_ids):
    console.print("[blue]Creating EKS Cluster...[/blue]")
    trust_relationship = {
        "Version": "2012-10-17",
        "Statement": [
            {
                "Effect": "Allow",
                "Principal": {
                    "Service": "eks.amazonaws.com"
                },
                "Action": "sts:AssumeRole"
            }
        ]
    }

    cluster_role = iam_client.create_role(
        RoleName=f"{CLUSTER_NAME}-cluster-role",
        AssumeRolePolicyDocument=json.dumps(trust_relationship)
    )
    iam_client.attach_role_policy(
        RoleName=f"{CLUSTER_NAME}-cluster-role",
        PolicyArn="arn:aws:iam::aws:policy/AmazonEKSClusterPolicy"
    )
    iam_client.attach_role_policy(
        RoleName=f"{CLUSTER_NAME}-cluster-role",
        PolicyArn="arn:aws:iam::aws:policy/AmazonEKSVPCResourceController"
    )

    cluster = eks_client.create_cluster(
        name=CLUSTER_NAME,
        roleArn=cluster_role['Role']['Arn'],
        resourcesVpcConfig={
            'subnetIds': subnet_ids
        }
    )
    console.print(f"[green]EKS Cluster creation initiated: [/green][bold]{cluster['cluster']['name']}[/bold]")
    return cluster

@task
def wait_for_cluster(c):
    console.print(f"[blue]Waiting for EKS Cluster [/blue][bold]{CLUSTER_NAME}[/bold][blue] to be active...[/blue]")
    with Progress() as progress:
        waiter = eks_client.get_waiter('cluster_active')
        waiter.wait(name=CLUSTER_NAME)
    console.print("[green]EKS Cluster is now active[/green]")

@task
def delete(c):
    console.print("[blue]Deleting EKS Cluster...[/blue]")
    try:
        eks_client.delete_cluster(name=CLUSTER_NAME)
        with Progress() as progress:
            waiter = eks_client.get_waiter('cluster_deleted')
            waiter.wait(name=CLUSTER_NAME)
        console.print(f"[green]EKS Cluster ${CLUSTER_NAME} deleted[/green]")
    except eks_client.exceptions.ResourceNotFoundException:
        console.print("[yellow]EKS Cluster not found[/yellow]")


@task
def sync(c):
    console.print("[blue]Syncing EKS Cluster...[/blue]")
    subprocess.run(["aws", "eks", "update-kubeconfig", "--name", CLUSTER_NAME, "--region", AWS_REGION])
