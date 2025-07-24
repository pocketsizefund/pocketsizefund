import json

import pulumi
import pulumi_aws as aws
from tags import pulumi_tags


def create_virtual_private_cloud_link(
    application_load_balancer_security_group: aws.ec2.SecurityGroup,
    public_subnets: list[aws.ec2.Subnet],
) -> aws.apigatewayv2.VpcLink:
    return aws.apigatewayv2.VpcLink(
        resource_name="pocketsizefund-api-gateway-vpc-link",
        name="pocketsizefund-vpc-link",
        security_group_ids=[application_load_balancer_security_group.id],
        subnet_ids=[subnet.id for subnet in public_subnets],
        opts=pulumi.ResourceOptions(
            depends_on=[
                application_load_balancer_security_group,
                *public_subnets,
            ]
        ),
        tags=pulumi_tags,
    )


def create_api_gateway(
    application_load_balancer_security_group: aws.ec2.SecurityGroup,
) -> aws.apigatewayv2.Api:
    api_gateway = aws.apigatewayv2.Api(
        resource_name="pocketsizefund-api-gateway",
        protocol_type="HTTP",
        route_selection_expression="$request.method $request.path",
        opts=pulumi.ResourceOptions(
            depends_on=[application_load_balancer_security_group],
        ),
        tags=pulumi_tags,
    )

    aws.apigatewayv2.Stage(
        resource_name="pocketsizefund-api-gateway-stage",
        api_id=api_gateway.id,
        name="production",
        auto_deploy=True,
        opts=pulumi.ResourceOptions(
            depends_on=[
                api_gateway,
                application_load_balancer_security_group,
            ],
        ),
        tags=pulumi_tags,
    )

    return api_gateway


def create_knative_service_api_gateway_integrations(
    service_name: str,
    endpoint_information: list[dict[str, str]],
    api_gateway: aws.apigatewayv2.Api,
    application_load_balancer_listener: aws.lb.Listener,
    vpc_link: aws.apigatewayv2.VpcLink,
) -> None:
    for endpoint in endpoint_information:
        endpoint_path = endpoint["path"].strip("/").replace("/", "-")
        endpoint_path = "-".join(filter(None, endpoint_path.split("-")))
        endpoint_method_lower = endpoint["method"].lower()
        endpoint_method_upper = endpoint["method"].upper()

        integration = aws.apigatewayv2.Integration(
            resource_name=f"pocketsizefund-{service_name}-{endpoint_path}-{endpoint_method_lower}-api-gateway-integration",
            api_id=api_gateway.id,
            integration_type="HTTP_PROXY",
            integration_uri=application_load_balancer_listener.arn,
            integration_method=endpoint_method_upper,
            connection_type="VPC_LINK",
            connection_id=vpc_link.id,
            opts=pulumi.ResourceOptions(
                depends_on=[
                    api_gateway,
                    application_load_balancer_listener,
                    vpc_link,
                ],
            ),
        )

        aws.apigatewayv2.Route(
            resource_name=f"pocketsizefund-{service_name}-{endpoint_path}-{endpoint_method_lower}-api-gateway-route",
            api_id=api_gateway.id,
            route_key=f"{endpoint_method_upper} /{endpoint_path}",
            target=integration.id.apply(
                lambda integration_id: f"integrations/{integration_id}"
            ),
            opts=pulumi.ResourceOptions(
                depends_on=[
                    api_gateway,
                    application_load_balancer_listener,
                    vpc_link,
                    integration,
                ],
            ),
        )


def create_api_access_iam_role(
    api_gateway: aws.apigatewayv2.Api,
    pulumi_user_arn: pulumi.Output[str],
    endpoint_information: list[dict[str, str]],
) -> aws.iam.Role:
    api_access_iam_role = aws.iam.Role(
        resource_name="pocketsizefund-api-access-role",
        name="pocketsizefund-api-access-role",
        description="Pocket Size Fund API access role",
        assume_role_policy=pulumi_user_arn.apply(
            lambda arn: json.dumps(
                {
                    "Version": "2012-10-17",
                    "Statement": [
                        {
                            "Effect": "Allow",
                            "Principal": {"AWS": f"{arn}"},
                            "Action": "sts:AssumeRole",
                        }
                    ],
                }
            )
        ),
        opts=pulumi.ResourceOptions(
            depends_on=[api_gateway],
        ),
        tags=pulumi_tags,
    )

    aws.iam.RolePolicy(
        resource_name="pocketsizefund-api-access-role-policy",
        role=api_access_iam_role.id,
        policy=api_gateway.arn.apply(
            lambda arn: json.dumps(
                {
                    "Version": "2012-10-17",
                    "Statement": [
                        {
                            "Effect": "Allow",
                            "Action": "execute-api:Invoke",
                            "Resource": [
                                f"{arn}/*/{t['method'].upper()}/{t['path'].lstrip('/')}"
                                for t in endpoint_information
                            ],
                        }
                    ],
                }
            )
        ),
        opts=pulumi.ResourceOptions(
            depends_on=[
                api_gateway,
                api_access_iam_role,
            ],
        ),
    )

    return api_access_iam_role
