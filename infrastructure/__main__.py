import json
import tomllib
from pathlib import Path

import pulumi
import pulumi_aws as aws
from api import (
    create_api_access_iam_role,
    create_api_gateway,
    create_knative_service_api_gateway_integrations,
    create_virtual_private_cloud_link,
)
from cluster import (
    create_kubernetes_cluster,
    create_kubernetes_cluster_role,
    create_kubernetes_node_role,
    create_kubernetes_provider,
    update_kubernetes_cluster_access,
)
from images import build_image
from ingress import (
    create_application_load_balancer,
    create_application_load_balancer_listener,
    create_application_load_balancer_security_group,
    create_application_load_balancer_target_group,
)
from keys import create_duckdb_user_access_key
from pulumi.config import Config
from services import (
    create_knative_broker,
    create_knative_eventing_core,
    create_knative_schedule,
    create_knative_service,
    create_knative_serving_core,
    create_knative_trigger,
    create_service_environment_variables,
)
from vpc import (
    create_elastic_ip,
    create_internet_gateway,
    create_nat_gateway,
    create_route_table,
    create_subnet,
    create_virtual_private_cloud,
)

configuration = Config()

virtual_private_cloud = create_virtual_private_cloud()

internet_gateway = create_internet_gateway(
    virtual_private_cloud=virtual_private_cloud,
)

public_route_table = create_route_table(
    virtual_private_cloud=virtual_private_cloud,
    internet_gateway=internet_gateway,
)

aws_region = configuration.get("aws:region") or "us-east-1"

availability_zones = aws.get_availability_zones(
    state="available",
    filters=[
        {
            "name": "region-name",
            "values": [aws_region],
        }
    ],
).names[:3]

public_subnets = [
    create_subnet(
        virtual_private_cloud=virtual_private_cloud,
        route_table=public_route_table,
        availability_zone=availability_zones[i],
        subnet_number=i + 1,  # 1-3
        visibility="public",
    )
    for i in range(len(availability_zones))
]

elastic_ip = create_elastic_ip(virtual_private_cloud=virtual_private_cloud)

nat_gateway = create_nat_gateway(
    elastic_ip=elastic_ip,
    public_subnet=public_subnets[0],  # one NAT instance for cost efficiency
)

private_route_table = create_route_table(
    virtual_private_cloud=virtual_private_cloud,
    nat_gateway=nat_gateway,
)


private_subnets = [
    create_subnet(
        virtual_private_cloud=virtual_private_cloud,
        route_table=private_route_table,
        availability_zone=availability_zones[i],
        subnet_number=i + 4,  # 4-6
        visibility="private",
    )
    for i in range(len(availability_zones))
]


kubernetes_cluster_role = create_kubernetes_cluster_role()

kubernetes_node_role = create_kubernetes_node_role()

kubernetes_cluster = create_kubernetes_cluster(
    virtual_private_cloud=virtual_private_cloud,
    private_subnets=private_subnets,
    kubernetes_cluster_role=kubernetes_cluster_role,
    kubernetes_node_role=kubernetes_node_role,
)

kubernetes_provider = create_kubernetes_provider(kubernetes_cluster=kubernetes_cluster)

cluster_access_configuration = update_kubernetes_cluster_access(
    kubernetes_provider=kubernetes_provider,
    kubernetes_cluster_role=kubernetes_cluster_role,
    kubernetes_node_role=kubernetes_node_role,
    pulumi_user_arn=configuration.require_secret("AWS_IAM_PULUMI_USER_ARN"),
    root_user_arn=configuration.require_secret("AWS_IAM_ROOT_USER_ARN"),
)

knative_serving_core = create_knative_serving_core(
    kubernetes_provider=kubernetes_provider,
)

knative_eventing_core = create_knative_eventing_core(
    kubernetes_provider=kubernetes_provider,
)

knative_broker = create_knative_broker(
    kubernetes_provider=kubernetes_provider,
    knative_eventing_core=knative_eventing_core,
)

duckdb_user_access_key = create_duckdb_user_access_key(
    data_bucket_name=configuration.require_secret("AWS_S3_DATA_BUCKET_NAME"),
)

service_environment_variables = create_service_environment_variables(
    inputs=[
        ("ALPACA_API_KEY", configuration.require_secret("ALPACA_API_KEY")),
        ("ALPACA_API_SECRET", configuration.require_secret("ALPACA_API_SECRET")),
        (
            "AWS_S3_DATA_BUCKET_NAME",
            configuration.require_secret("AWS_S3_DATA_BUCKET_NAME"),
        ),
        ("POLYGON_API_KEY", configuration.require_secret("POLYGON_API_KEY")),
        ("AWS_IAM_DUCKDB_USER_ACCESS_KEY_ID", duckdb_user_access_key.id),
        ("AWS_IAM_DUCKDB_USER_ACCESS_KEY_SECRET", duckdb_user_access_key.secret),
        ("AWS_REGION", aws_region),
    ],
)

application_load_balancer_security_group = (
    create_application_load_balancer_security_group(
        virtual_private_cloud=virtual_private_cloud,
    )
)

application_load_balancer = create_application_load_balancer(
    application_load_balancer_security_group=application_load_balancer_security_group,
    public_subnets=public_subnets,
)

virtual_private_cloud_link = create_virtual_private_cloud_link(
    application_load_balancer_security_group=application_load_balancer_security_group,
    public_subnets=public_subnets,
)

api_gateway = create_api_gateway(
    application_load_balancer_security_group=application_load_balancer_security_group,
)

target_group = create_application_load_balancer_target_group(
    virtual_private_cloud=virtual_private_cloud,
    application_load_balancer=application_load_balancer,
)

listener = create_application_load_balancer_listener(
    application_load_balancer=application_load_balancer,
    application_load_balancer_target_group=target_group,
)

try:
    with Path("pyproject.toml").open("rb") as f:
        project_data = tomllib.load(f)
        version = project_data.get("project", {}).get("version")

except (FileNotFoundError, tomllib.TOMLDecodeError, ValueError) as e:
    message = f"Failed to read version from infrastructure pyproject.toml: {e}"
    raise RuntimeError(message) from e

username = configuration.require_secret("DOCKERHUB_USERNAME")
password = configuration.require_secret("DOCKERHUB_PASSWORD")

datamanager_image = build_image(
    service_name="datamanager",
    service_version=version,
    dockerhub_username=username,
    dockerhub_password=password,
)

datamanager_knative_service = create_knative_service(
    kubernetes_provider=kubernetes_provider,
    service_name="datamanager",
    image=datamanager_image,
    application_load_balancer_service_target_group=target_group,
    knative_serving_core=knative_serving_core,
    environment_variables=service_environment_variables,
)

endpoint_information = [
    {"path": "/health", "method": "GET"},
    {"path": "/equity-bars", "method": "GET"},
    {"path": "/equity-bars/fetch", "method": "POST"},
    {"path": "/equity-bars", "method": "DELETE"},
]

create_knative_service_api_gateway_integrations(
    service_name="datamanager",
    endpoint_information=endpoint_information,
    api_gateway=api_gateway,
    application_load_balancer_listener=listener,
    vpc_link=virtual_private_cloud_link,
)

api_access_iam_role = create_api_access_iam_role(
    api_gateway=api_gateway,
    pulumi_user_arn=configuration.require_secret("AWS_IAM_PULUMI_USER_ARN"),
    endpoint_information=endpoint_information,
)

predictionengine_image = build_image(
    service_name="predictionengine",
    service_version=version,
    dockerhub_username=username,
    dockerhub_password=password,
)

predictionengine_knative_service = create_knative_service(
    kubernetes_provider=kubernetes_provider,
    service_name="predictionengine",
    image=predictionengine_image,
    application_load_balancer_service_target_group=target_group,
    knative_serving_core=knative_serving_core,
    environment_variables=service_environment_variables,
)

positionmanager_image = build_image(
    service_name="positionmanager",
    service_version=version,
    dockerhub_username=username,
    dockerhub_password=password,
)

positionmanager_knative_service = create_knative_service(
    kubernetes_provider=kubernetes_provider,
    service_name="positionmanager",
    image=positionmanager_image,
    application_load_balancer_service_target_group=target_group,
    knative_serving_core=knative_serving_core,
    environment_variables=service_environment_variables,
)

open_positions_from_predictions_trigger = create_knative_trigger(
    kubernetes_provider=kubernetes_provider,
    source_service_name="predictionengine",
    source_attribute_type="application.predictionengine.predictions.created",
    target_service_name="positionmanager",
    knative_eventing_core=knative_eventing_core,
)

midnight_data_fetch_schedule = create_knative_schedule(
    kubernetes_provider=kubernetes_provider,
    target_service_name="datamanager",
    target_path="/equity-bars/fetch",
    cron_schedule="0 0 * * *",
    knative_eventing_core=knative_eventing_core,
)

monday_morning_open_positions_schedule = create_knative_schedule(
    kubernetes_provider=kubernetes_provider,
    target_service_name="predictionengine",
    target_path="/predictions/create",
    cron_schedule="0 10 * * 1",
    knative_eventing_core=knative_eventing_core,
)

friday_evening_close_positions_schedule = create_knative_schedule(
    kubernetes_provider=kubernetes_provider,
    target_service_name="positionmanager",
    target_path="/positions/close",
    cron_schedule="0 13 * * 5",
    knative_eventing_core=knative_eventing_core,
)


pulumi.export("DATAMANAGER_SERVICE_IMAGE", datamanager_image.ref)

pulumi.export(
    "PREDICTIONENGINE_SERVICE_IMAGE",
    predictionengine_image.ref,
)

pulumi.export(
    "POSITIONMANAGER_SERVICE_IMAGE",
    positionmanager_image.ref,
)

pulumi.export(
    "AWS_EKS_CLUSTER_NAME",
    kubernetes_cluster.eks_cluster.name.apply(lambda cluster_name: f"{cluster_name}"),
)

pulumi.export(
    "AWS_EKS_KUBECONFIG",
    kubernetes_cluster.kubeconfig.apply(json.dumps),
)

pulumi.export(
    "AWS_VIRTUAL_PRIVATE_CLOUD_ID",
    virtual_private_cloud.id.apply(lambda vpc_id: f"{vpc_id}"),
)

pulumi.export(
    "AWS_API_GATEWAY_ACCESS_IAM_ROLE_ARN",
    api_access_iam_role.arn.apply(lambda arn: f"{arn}"),
)

pulumi.export(
    "AWS_API_GATEWAY_ENDPOINT_URL",
    api_gateway.api_endpoint.apply(lambda endpoint: f"{endpoint}/production"),
)
