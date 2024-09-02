from invoke import task
from rich.console import Console
from steps import CLUSTER_NAME, eks_client

console = Console()

@task
def create(c):
    console.print("[blue]Adding CloudWatch observability addon...[/blue]")
    try:
        response = eks_client.create_addon(
            clusterName=CLUSTER_NAME,
            addonName='amazon-cloudwatch-observability',
            resolveConflicts='OVERWRITE'
        )
        console.print(f"[green]CloudWatch observability addon creation initiated: {response['addon']['addonName']}[/green]")
        with console.status("[green]Waiting for CloudWatch observability addon to become active...[/green]", spinner="dots"):
            waiter = eks_client.get_waiter('addon_active')
            waiter.wait(
                clusterName=CLUSTER_NAME,
                addonName='amazon-cloudwatch-observability'
            )
        console.print("[blue]CloudWatch observability addon is now active[/blue]")
    except eks_client.exceptions.ResourceNotFoundException:
        console.print("[red]EKS Cluster not found[/red]")
    except eks_client.exceptions.InvalidParameterException as e:
        console.print(f"[red]Invalid parameter: {str(e)}[/red]")
    except eks_client.exceptions.ClientError as e:
        console.print(f"[red]Error adding CloudWatch observability addon: {str(e)}[/red]")

@task
def delete(c):
    console.print("[blue]Deleting CloudWatch observability addon...[/blue]")
    try:
        eks_client.delete_addon(
            clusterName=CLUSTER_NAME,
            addonName='amazon-cloudwatch-observability'
        )
        console.print("[green]CloudWatch observability addon deletion initiated[/green]")

        with console.status("[green]Waiting for CloudWatch observability addon to be deleted...[/green]", spinner="dots"):
            waiter = eks_client.get_waiter('addon_deleted')
            waiter.wait(
                clusterName=CLUSTER_NAME,
                addonName='amazon-cloudwatch-observability'
            )
        console.print("[blue]CloudWatch observability addon has been deleted[/blue]")
    except eks_client.exceptions.ResourceNotFoundException:
        console.print("[yellow]CloudWatch observability addon not found[/yellow]")
    except eks_client.exceptions.ClientError as e:
        console.print(f"[red]Error deleting CloudWatch observability addon: {str(e)}[/red]")
