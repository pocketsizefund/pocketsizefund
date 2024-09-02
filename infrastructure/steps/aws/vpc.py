from invoke import task
from rich.progress import Progress
from rich.console import Console

from steps import CLUSTER_NAME, VPC_CIDR
from steps import ec2_client

console = Console()

@task
def create(c):
    console.print("[blue]Checking for existing VPC...[/blue]")
    existing_vpcs = ec2_client.describe_vpcs(Filters=[{'Name': 'tag:Name', 'Values': [f"{CLUSTER_NAME}-vpc"]}])['Vpcs']
    if existing_vpcs:
        vpc_id = existing_vpcs[0]['VpcId']
        console.print(f"[green]Existing VPC found: {vpc_id}[/green]")
        return vpc_id
    
    console.print("[green]Creating new VPC...[/green]")
    vpc = ec2_client.create_vpc(CidrBlock=VPC_CIDR)
    vpc_id = vpc['Vpc']['VpcId']
    ec2_client.create_tags(Resources=[vpc_id], Tags=[{'Key': 'Name', 'Value': f"{CLUSTER_NAME}-vpc"}])
    ec2_client.modify_vpc_attribute(VpcId=vpc_id, EnableDnsHostnames={'Value': True})
    ec2_client.modify_vpc_attribute(VpcId=vpc_id, EnableDnsSupport={'Value': True})
    console.print(f"[green]New VPC created: [/green][bold]{vpc_id}[/bold]")
    return vpc_id

@task
def delete(c):
    console.print("[blue]Deleting VPC resources...[/blue]")
    vpcs = ec2_client.describe_vpcs(Filters=[{'Name': 'tag:Name', 'Values': [f"{CLUSTER_NAME}-vpc"]}])['Vpcs']
    if not vpcs:
        console.print("[yellow]VPC not found[/yellow]")
        return

    vpc_id = vpcs[0]['VpcId']

    addresses = ec2_client.describe_addresses(Filters=[{'Name': 'domain', 'Values': ['vpc']}])
    console.print(f"[green]Releasing [/green][bold]{len(addresses['Addresses'])}[/bold][green] addresses[/green]")
    for address in addresses['Addresses']:
        if 'AssociationId' in address:
            console.print(f"[green]Disassociating address [/green][bold]{address['PublicIp']}[/bold]")
            ec2_client.disassociate_address(AssociationId=address['AssociationId'])
        console.print(f"[green]Releasing address [/green][bold]{address['PublicIp']}[/bold]")
        ec2_client.release_address(AllocationId=address['AllocationId'])

    instances = ec2_client.describe_instances(Filters=[{'Name': 'vpc-id', 'Values': [vpc_id]}])
    instance_ids = [instance['InstanceId'] for reservation in instances['Reservations'] for instance in reservation['Instances']]
    if instance_ids:
        console.print(f"[green]Terminating [/green][bold]{len(instance_ids)}[/bold][green] instances[/green]")
        ec2_client.terminate_instances(InstanceIds=instance_ids)
        waiter = ec2_client.get_waiter('instance_terminated')
        with Progress() as progress:
            progress.add_task("[cyan]Waiting for instances to terminate...", total=None)
            waiter.wait(InstanceIds=instance_ids)
    else:
        console.print("[green]No instances to terminate[/green]")

    nat_gateways = ec2_client.describe_nat_gateways(Filters=[{'Name': 'vpc-id', 'Values': [vpc_id]}])
    for nat_gateway in nat_gateways['NatGateways']:
        console.print(f"[green]Releasing [/green][bold]{nat_gateway['NatGatewayId']}[/bold]")
        ec2_client.delete_nat_gateway(NatGatewayId=nat_gateway['NatGatewayId'])

    waiter = ec2_client.get_waiter('nat_gateway_deleted')
    with Progress() as progress:
        progress.add_task("[cyan]Waiting for NAT Gateways to be deleted...", total=None)
        for nat_gateway in nat_gateways['NatGateways']:
            waiter.wait(NatGatewayIds=[nat_gateway['NatGatewayId']])

    subnets = ec2_client.describe_subnets(Filters=[{'Name': 'vpc-id', 'Values': [vpc_id]}])['Subnets']
    for subnet in subnets:
        console.print(f"[green]Deleting Subnet [/green][bold]{subnet['SubnetId']}[/bold]")
        ec2_client.delete_subnet(SubnetId=subnet['SubnetId'])

    route_tables = ec2_client.describe_route_tables(Filters=[{'Name': 'vpc-id', 'Values': [vpc_id]}])['RouteTables']
    for rt in route_tables:
        if not rt.get('Associations') or not any(assoc.get('Main', False) for assoc in rt['Associations']):
            console.print(f"[green]Deleting Route Table [/green][bold]{rt['RouteTableId']}[/bold]")
            for assoc in rt.get('Associations', []):
                ec2_client.disassociate_route_table(AssociationId=assoc['RouteTableAssociationId'])
            ec2_client.delete_route_table(RouteTableId=rt['RouteTableId'])

    security_groups = ec2_client.describe_security_groups(Filters=[{'Name': 'vpc-id', 'Values': [vpc_id]}])['SecurityGroups']
    for sg in security_groups:
        if sg['GroupName'] != 'default':
            console.print(f"[green]Deleting Security Group [/green][bold]{sg['GroupId']}[/bold]")
            ec2_client.delete_security_group(GroupId=sg['GroupId'])

    ec2_client.delete_vpc(VpcId=vpc_id)
    console.print(f"[green]Deleting VPC [/green][bold]{vpc_id}[/bold]")
    console.print("[blue]VPC and associated resources deleted[/blue]")


