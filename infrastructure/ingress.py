import json

import pulumi
import pulumi_aws as aws
import pulumi_eks as eks
import pulumi_kubernetes as k8s
from pulumi.config import Config
from tags import common_tags

configuration = Config()


def create_alb_controller_role(cluster: eks.Cluster) -> aws.iam.Role:
    policy_document = pulumi.Output.all(
        oidc_provider_arn=cluster.core.oidc_provider.arn,
        oidc_provider_url=cluster.core.oidc_provider.url,
    ).apply(
        lambda args: json.dumps(
            {
                "Version": "2012-10-17",
                "Statement": [
                    {
                        "Effect": "Allow",
                        "Principal": {"Federated": args["oidc_provider_arn"]},
                        "Action": "sts:AssumeRoleWithWebIdentity",
                        "Condition": {
                            "StringEquals": {
                                f"{args['oidc_provider_url'].replace('https://', '')}:sub": "system:serviceaccount:kube-system:aws-load-balancer-controller",  # noqa: E501
                                f"{args['oidc_provider_url'].replace('https://', '')}:aud": "sts.amazonaws.com",  # noqa: E501
                            }
                        },
                    }
                ],
            }
        )
    )

    alb_controller_role = aws.iam.Role(
        resource_name="pocketsizefund-alb-controller-role",
        name="pocketsizefund-alb-controller-role",
        assume_role_policy=policy_document,
        tags=common_tags,
    )

    aws.iam.RolePolicyAttachment(
        resource_name="pocketsizefund-alb-controller-policy",
        role=alb_controller_role.name,
        policy_arn="arn:aws:iam::aws:policy/ElasticLoadBalancingFullAccess",
    )

    return alb_controller_role


def create_alb_controller(
    kubernetes_provider: k8s.Provider,
    cluster: eks.Cluster,
    alb_controller_role: aws.iam.Role,
) -> k8s.helm.v3.Release:
    alb_controller_service_account = k8s.core.v1.ServiceAccount(
        resource_name="pocketsizefund-alb-controller-service-account",
        metadata=k8s.meta.v1.ObjectMetaArgs(
            name="aws-load-balancer-controller",
            namespace="kube-system",
            annotations={
                "eks.amazonaws.com/role-arn": alb_controller_role.arn,
            },
        ),
        opts=pulumi.ResourceOptions(provider=kubernetes_provider),
    )

    return k8s.helm.v3.Release(
        resource_name="pocketsizefund-alb-controller",
        name="aws-load-balancer-controller",
        chart="aws-load-balancer-controller",
        namespace="kube-system",
        repository_opts=k8s.helm.v3.RepositoryOptsArgs(
            repo="https://aws.github.io/eks-charts"
        ),
        values={
            "clusterName": cluster.name,  # type: ignore
            "serviceAccount": {
                "create": False,
                "name": "aws-load-balancer-controller",
            },
            "region": configuration.get("aws:region") or "us-east-1",
            "vpcId": cluster.eks_cluster.vpc_config.vpc_id,
        },
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            depends_on=[alb_controller_service_account],
        ),
    )


def create_service_ingress(
    kubernetes_provider: k8s.Provider,
    service_name: str,
    cluster: eks.Cluster,
    certificate_arn: pulumi.Output[str] | None = None,
    depends_on: list[pulumi.Resource] | None = None,
) -> k8s.networking.v1.Ingress:
    annotations = {
        "kubernetes.io/ingress.class": "alb",
        "alb.ingress.kubernetes.io/scheme": "internet-facing",
        "alb.ingress.kubernetes.io/target-type": "pod",
        "alb.ingress.kubernetes.io/load-balancer-name": f"pocketsizefund-{service_name}",  # noqa: E501
        "alb.ingress.kubernetes.io/subnets": cluster.public_subnet_ids.apply(  # type: ignore
            lambda subnets: ",".join(subnets)
        ),
    }

    if certificate_arn:
        annotations.update(
            {
                "alb.ingress.kubernetes.io/listen-ports": '[{"HTTP": 80}, {"HTTPS": 443}]',  # noqa: E501
                "alb.ingress.kubernetes.io/certificate-arn": certificate_arn,
                "alb.ingress.kubernetes.io/ssl-redirect": "443",
            }
        )
    else:
        annotations["alb.ingress.kubernetes.io/listen-ports"] = '[{"HTTP": 80}]'

    return k8s.networking.v1.Ingress(
        resource_name=f"pocketsizefund-{service_name}-ingress",
        metadata=k8s.meta.v1.ObjectMetaArgs(
            name=f"{service_name}-ingress",
            namespace="default",
            annotations=annotations,
        ),
        spec=k8s.networking.v1.IngressSpecArgs(
            rules=[
                k8s.networking.v1.IngressRuleArgs(
                    http=k8s.networking.v1.HTTPIngressRuleValueArgs(
                        paths=[
                            k8s.networking.v1.HTTPIngressPathArgs(
                                path="/",
                                path_type="Prefix",
                                backend=k8s.networking.v1.IngressBackendArgs(
                                    service=k8s.networking.v1.IngressServiceBackendArgs(
                                        name=service_name,
                                        port=k8s.networking.v1.ServiceBackendPortArgs(
                                            number=80
                                        ),
                                    )
                                ),
                            )
                        ]
                    )
                )
            ]
        ),
        opts=pulumi.ResourceOptions(
            provider=kubernetes_provider,
            depends_on=depends_on or [],
        ),
    )


def create_self_signed_certificate() -> aws.acm.Certificate:
    return aws.acm.Certificate(
        resource_name="pocketsizefund-self-signed-cert",
        domain_name="*.amazonaws.com",
        validation_method="DNS",
        subject_alternative_names=["*.elb.amazonaws.com"],
        tags=common_tags,
    )


def create_api_gateway_with_auth(
    service_name: str,
    target_url: pulumi.Output[str],
) -> aws.apigatewayv2.Api:
    api = aws.apigatewayv2.Api(
        resource_name=f"pocketsizefund-{service_name}-api",
        name=f"pocketsizefund-{service_name}",
        protocol_type="HTTP",
        cors_configuration=aws.apigatewayv2.ApiCorsConfigurationArgs(
            allow_origins=["*"],  # reduce allowed origins in production
            allow_methods=["GET", "POST", "DELETE"],
            allow_headers=["Content-Type", "Authorization", "Host"],
            max_age=86400,
        ),
        tags=common_tags,
    )

    integration = aws.apigatewayv2.Integration(
        resource_name=f"pocketsizefund-{service_name}-integration",
        api_id=api.id,
        integration_type="HTTP_PROXY",
        integration_method="ANY",
        integration_uri=target_url,
        connection_type="INTERNET",
    )

    aws.apigatewayv2.Route(
        resource_name=f"pocketsizefund-{service_name}-route",
        api_id=api.id,
        route_key="ANY /{proxy+}",
        target=integration.id.apply(
            lambda integration_id: f"integrations/{integration_id}"
        ),
        authorization_type="AWS_IAM",
    )

    aws.apigatewayv2.Stage(
        resource_name=f"pocketsizefund-{service_name}-stage",
        api_id=api.id,
        name="$default",
        auto_deploy=True,
        default_route_settings=aws.apigatewayv2.StageDefaultRouteSettingsArgs(
            throttling_burst_limit=100,
            throttling_rate_limit=50,
        ),
        tags=common_tags,
    )

    pulumi.export(f"{service_name.upper()}_API_GATEWAY_URL", api.api_endpoint)

    return api


def create_api_access_policy(
    api_gateway: aws.apigatewayv2.Api,
    service_name: str,
) -> aws.iam.Policy:
    policy_document = api_gateway.arn.apply(
        lambda arn: json.dumps(
            {
                "Version": "2012-10-17",
                "Statement": [
                    {
                        "Effect": "Allow",
                        "Action": ["execute-api:Invoke"],
                        "Resource": f"{arn}/$default/*",
                    }
                ],
            }
        )
    )

    return aws.iam.Policy(
        resource_name=f"pocketsizefund-{service_name}-api-access",
        name=f"pocketsizefund-{service_name}-api-access",
        description=f"Policy for accessing {service_name} API Gateway",
        policy=policy_document,
        tags=common_tags,
    )
