import pulumi
import pulumi_aws as aws
from tags import pulumi_tags


def create_application_load_balancer_security_group(
    virtual_private_cloud: aws.ec2.Vpc,
) -> aws.ec2.SecurityGroup:
    return aws.ec2.SecurityGroup(
        resource_name="pocketsizefund-alb-security-group",
        vpc_id=virtual_private_cloud.id,
        ingress=[
            aws.ec2.SecurityGroupIngressArgs(
                protocol="tcp",
                from_port=80,
                to_port=80,
                cidr_blocks=["0.0.0.0/0"],
            ),
            aws.ec2.SecurityGroupIngressArgs(
                protocol="tcp",
                from_port=443,
                to_port=443,
                cidr_blocks=["0.0.0.0/0"],
            ),
        ],
        egress=[
            aws.ec2.SecurityGroupEgressArgs(
                protocol="-1",
                from_port=0,
                to_port=0,
                cidr_blocks=["0.0.0.0/0"],
            )
        ],
        opts=pulumi.ResourceOptions(
            depends_on=[virtual_private_cloud],
        ),
        tags=pulumi_tags,
    )


def create_application_load_balancer(
    application_load_balancer_security_group: aws.ec2.SecurityGroup,
    public_subnets: list[aws.ec2.Subnet],
) -> aws.lb.LoadBalancer:
    return aws.lb.LoadBalancer(
        resource_name="pocketsizefund-alb",
        internal=False,
        load_balancer_type="application",
        security_groups=[application_load_balancer_security_group.id],
        subnets=[subnet.id for subnet in public_subnets],
        opts=pulumi.ResourceOptions(
            depends_on=[
                application_load_balancer_security_group,
                *public_subnets,
            ],
        ),
        tags=pulumi_tags,
    )


def create_application_load_balancer_target_group(
    virtual_private_cloud: aws.ec2.Vpc,
    application_load_balancer: aws.lb.LoadBalancer,
) -> aws.lb.TargetGroup:
    return aws.lb.TargetGroup(
        resource_name="pocketsizefund-tg",
        port=8080,  # match service port
        protocol="HTTP",
        target_type="ip",
        vpc_id=virtual_private_cloud.id,
        health_check=aws.lb.TargetGroupHealthCheckArgs(
            enabled=True,
            healthy_threshold=3,
            unhealthy_threshold=3,
            interval=60,
            path="/health",
            port="8080",
            protocol="HTTP",
            timeout=10,
        ),
        opts=pulumi.ResourceOptions(
            replace_on_changes=["*"],
            depends_on=[
                virtual_private_cloud,
                application_load_balancer,
            ],
        ),
        tags=pulumi_tags,
    )


def create_application_load_balancer_listener(
    application_load_balancer: aws.lb.LoadBalancer,
    application_load_balancer_target_group: aws.lb.TargetGroup,
) -> aws.lb.Listener:
    return aws.lb.Listener(
        resource_name="pocketsizefund-listener",
        load_balancer_arn=application_load_balancer.arn,
        port=80,  # publicly exposed port
        protocol="HTTP",
        default_actions=[
            aws.lb.ListenerDefaultActionArgs(
                type="forward",
                target_group_arn=application_load_balancer_target_group.arn,
            )
        ],
        opts=pulumi.ResourceOptions(
            depends_on=[
                application_load_balancer,
                application_load_balancer_target_group,
            ],
        ),
        tags=pulumi_tags,
    )
