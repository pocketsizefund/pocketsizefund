import pulumi
import pulumi_aws as aws
from tags import pulumi_tags


def create_virtual_private_cloud() -> aws.ec2.Vpc:
    return aws.ec2.Vpc(
        resource_name="pocketsizefund-vpc",
        cidr_block="10.0.0.0/16",
        enable_dns_support=True,
        enable_dns_hostnames=True,
        tags=pulumi_tags,
    )


def create_internet_gateway(
    virtual_private_cloud: aws.ec2.Vpc,
) -> aws.ec2.InternetGateway:
    return aws.ec2.InternetGateway(
        resource_name="pocketsizefund-internet-gateway",
        vpc_id=virtual_private_cloud.id,
        opts=pulumi.ResourceOptions(depends_on=[virtual_private_cloud]),
        tags=pulumi_tags,
    )


def create_elastic_ip(virtual_private_cloud: aws.ec2.Vpc) -> aws.ec2.Eip:
    return aws.ec2.Eip(
        resource_name="pocketsizefund-elastic-ip",
        opts=pulumi.ResourceOptions(
            depends_on=[virtual_private_cloud],
        ),
        tags=pulumi_tags,
    )


def create_nat_gateway(
    elastic_ip: aws.ec2.Eip,
    public_subnet: aws.ec2.Subnet,
) -> aws.ec2.NatGateway:
    return aws.ec2.NatGateway(
        resource_name="pocketsizefund-nat-gateway",
        allocation_id=elastic_ip.id,
        subnet_id=public_subnet.id,
        tags=pulumi_tags,
        opts=pulumi.ResourceOptions(
            depends_on=[
                elastic_ip,
                public_subnet,
            ],
        ),
    )


def create_route_table(
    virtual_private_cloud: aws.ec2.Vpc,
    internet_gateway: aws.ec2.InternetGateway | None = None,
    nat_gateway: aws.ec2.NatGateway | None = None,
) -> aws.ec2.RouteTable:
    depends_on: list[pulumi.Resource] = [virtual_private_cloud]
    if internet_gateway:
        depends_on.append(internet_gateway)
    if nat_gateway:
        depends_on.append(nat_gateway)

    if internet_gateway and nat_gateway:
        message = "Cannot specify both internet_gateway and nat_gateway"
        raise ValueError(message)
    if not internet_gateway and not nat_gateway:
        message = "Must specify either internet_gateway or nat_gateway"
        raise ValueError(message)

    visibility = "public" if internet_gateway else "private"

    return aws.ec2.RouteTable(
        resource_name=f"pocketsizefund-{visibility}-route-table",
        vpc_id=virtual_private_cloud.id,
        routes=[
            aws.ec2.RouteTableRouteArgs(
                cidr_block="0.0.0.0/0",
                gateway_id=internet_gateway.id if internet_gateway else None,
                nat_gateway_id=nat_gateway.id if nat_gateway else None,
            )
        ],
        opts=pulumi.ResourceOptions(depends_on=depends_on),
        tags=pulumi_tags,
    )


def create_subnet(
    virtual_private_cloud: aws.ec2.Vpc,
    route_table: aws.ec2.RouteTable,
    availability_zone: str,
    subnet_number: int,
    visibility: str = "public",
) -> aws.ec2.Subnet:
    minimum_subnet_number = 0
    maximum_subnet_number = 255

    if not minimum_subnet_number <= subnet_number <= maximum_subnet_number:
        message = f"subnet_number must be between 0 and 255, got {subnet_number}"
        raise ValueError(message)

    visibility = visibility.lower()

    subnet = aws.ec2.Subnet(
        resource_name=f"pocketsizefund-{visibility}-subnet-{subnet_number}",
        vpc_id=virtual_private_cloud.id,
        cidr_block=f"10.0.{subnet_number}.0/24",
        availability_zone=availability_zone,
        map_public_ip_on_launch=visibility == "public",
        tags=pulumi_tags,
        opts=pulumi.ResourceOptions(
            depends_on=[
                virtual_private_cloud,
                route_table,
            ],
        ),
    )

    aws.ec2.RouteTableAssociation(
        resource_name=f"pocketsizefund-{visibility}-route-table-subnet-association-{subnet_number}",
        subnet_id=subnet.id,
        route_table_id=route_table.id,
        opts=pulumi.ResourceOptions(
            depends_on=[
                virtual_private_cloud,
                subnet,
                route_table,
            ],
        ),
    )

    return subnet
