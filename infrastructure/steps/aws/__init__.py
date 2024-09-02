from invoke import task
from steps.aws import vpc, subnet, route_table, eks, node_group, iam
from steps import SUBNET_CIDRS, AVAILABILITY_ZONES
from rich.console import Console

console = Console()

@task
def create(c):
    vpc_id = vpc.create(c)
    subnet_ids = []
    for i, (az, cidr) in enumerate(zip(AVAILABILITY_ZONES, SUBNET_CIDRS)):
        subnet_id = subnet.create(c, vpc_id, cidr, az, i)
        subnet_ids.append(subnet_id)
    route_table.create(c, vpc_id, subnet_ids)
    eks.create(c, subnet_ids)
    eks.wait_for_cluster(c)
    try:
        node_group.create(c, subnet_ids)
        console.print("[blue]Node group created successfully![/blue]")
    except Exception as e:
        console.print(f"[red]Error creating node group: {str(e)}[/red]")
        console.print("[yellow]Setup incomplete. Please check the error and try again.[/yellow]")
        return
    console.print("[blue]Setup complete![/blue]")



@task
def delete(c):
    node_group.delete(c)
    eks.delete(c)
    iam.delete(c)
    vpc.delete(c)
    console.print("[green]Cleanup complete![/green]")
