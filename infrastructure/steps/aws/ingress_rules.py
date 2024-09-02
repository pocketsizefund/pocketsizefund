from invoke import task
from rich.console import Console

from steps import CLUSTER_NAME
from steps import eks_client, ec2_client

console = Console()

@task
def create(c, vpc_id):
    console.print("[blue]Checking for existing VPC...[/blue]")

    security_group = ec2_client.create_security_group(
        GroupName=f"{CLUSTER_NAME}-cluster-sg",
        Description="Security group for EKS cluster",
        VpcId=vpc_id
    )
    security_group_id = security_group['GroupId']
    console.print(f"[green]Security group created: [/green][bold]{security_group_id}[/bold]")

    try:
        cluster_info = eks_client.describe_cluster(name=CLUSTER_NAME)
        control_plane_sg_id = cluster_info['cluster']['resourcesVpcConfig']['clusterSecurityGroupId']

        ec2_client.authorize_security_group_ingress(
            GroupId=security_group_id,
            IpPermissions=[
                {
                    'IpProtocol': 'tcp',
                    'FromPort': 10250,
                    'ToPort': 10250,
                    'UserIdGroupPairs': [{'GroupId': control_plane_sg_id}]
                }
            ]
        )
        console.print(f"[green]Added ingress rule for port 10250 from control plane security group[/green]")
    except eks_client.exceptions.ResourceNotFoundException:
        console.print("[yellow]EKS cluster not found. Skipping security group ingress rule.[/yellow]")
    except Exception as e:
        console.print(f"[red]Error adding security group ingress rule: [/red][bold]{str(e)}[/bold]")

    return vpc_id
