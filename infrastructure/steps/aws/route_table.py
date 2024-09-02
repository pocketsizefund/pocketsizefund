from invoke import task
from steps import CLUSTER_NAME
from steps import ec2_client
from rich.console import Console

console = Console()

@task
def create(c, vpc_id, subnet_ids):
    console.print("[blue]Creating Route Table...[/blue]")
    route_table = ec2_client.create_route_table(VpcId=vpc_id)
    rt_id = route_table['RouteTable']['RouteTableId']
    for subnet_id in subnet_ids:
        console.print(f"[green]Associating route table [/green][bold]{rt_id}[/bold][green] with subnet {subnet_id}[/green]")
        ec2_client.associate_route_table(RouteTableId=rt_id, SubnetId=subnet_id)
    ec2_client.create_tags(Resources=[rt_id], Tags=[{'Key': 'Name', 'Value': f"{CLUSTER_NAME}-route-table"}])
    console.log(f"[blue]Route Table [/blue][bold]{rt_id}[/bold][blue] created[/blue]")


