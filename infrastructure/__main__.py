import tomllib
from pathlib import Path

import pulumi
from cluster import (
    create_kubernetes_cluster,
    create_kubernetes_provider,
    update_kubernetes_cluster_access,
)
from environment_variables import create_environment_variables
from images import build_image
from ingress import (
    create_alb_controller,
    create_alb_controller_role,
    create_api_access_policy,
    create_api_gateway_with_auth,
    create_service_ingress,
)
from keys import create_duckdb_user_access_key
from monitors import create_prometheus_scraper
from publishers_subscribers import (
    create_knative_broker,
    create_knative_eventing_core,
    create_knative_schedule,
    create_knative_service,
    create_knative_serving_core,
    create_knative_trigger,
)
from pulumi.config import Config
from roles import (
    create_cluster_role,
    create_node_role,
)

configuration = Config()

cluster_role = create_cluster_role()

node_role = create_node_role()

kubernetes_cluster = create_kubernetes_cluster(cluster_role, node_role)

kubernetes_provider = create_kubernetes_provider(kubernetes_cluster)

cluster_access_config = update_kubernetes_cluster_access(
    cluster_role=cluster_role,
    node_role=node_role,
    kubernetes_provider=kubernetes_provider,
    pulumi_user_arn=configuration.require_secret("AWS_EKS_IAM_PULUMI_USER_ARN"),
    root_user_arn=configuration.require_secret("AWS_EKS_IAM_ROOT_USER_ARN"),
)

alb_controller_role = create_alb_controller_role(kubernetes_cluster)

alb_controller = create_alb_controller(
    kubernetes_provider=kubernetes_provider,
    cluster=kubernetes_cluster,
    alb_controller_role=alb_controller_role,
)

knative_serving_core = create_knative_serving_core(kubernetes_provider)

knative_eventing_core = create_knative_eventing_core(kubernetes_provider)

knative_broker = create_knative_broker(
    kubernetes_provider=kubernetes_provider,
    knative_eventing_core=knative_eventing_core,
)

try:
    with Path("pyproject.toml").open("rb") as f:
        project_data = tomllib.load(f)
        version = project_data.get("project", {}).get("version")

except (FileNotFoundError, tomllib.TOMLDecodeError, ValueError) as e:
    message = f"Failed to read version from infrastructure pyproject.toml: {e}"
    raise RuntimeError(message) from e

datamanager_image = build_image(
    service_name="datamanager",
    service_version=version,
)


duckdb_user_access_key = create_duckdb_user_access_key()

environment_variables = create_environment_variables(duckdb_user_access_key)

datamanager_service = create_knative_service(
    kubernetes_provider=kubernetes_provider,
    service_name="datamanager",
    image_reference=datamanager_image.ref,
    environment_variables=environment_variables,
    depends_on=[knative_serving_core],
)

predictionengine_image = build_image(
    service_name="predictionengine",
    service_version=version,
)

predictionengine_service = create_knative_service(
    kubernetes_provider=kubernetes_provider,
    service_name="predictionengine",
    image_reference=predictionengine_image.ref,
    environment_variables=environment_variables,
    depends_on=[knative_serving_core],
)

positionmanager_image = build_image(
    service_name="positionmanager",
    service_version=version,
)

positionmanager_service = create_knative_service(
    kubernetes_provider=kubernetes_provider,
    service_name="positionmanager",
    image_reference=positionmanager_image.ref,
    environment_variables=environment_variables,
    depends_on=[knative_serving_core],
)

open_positions_from_predictions_trigger = create_knative_trigger(
    kubernetes_provider=kubernetes_provider,
    source_service_name="predictionengine",
    source_attribute_type="application.predictionengine.predictions.created",
    target_service_name="positionmanager",
    depends_on=[predictionengine_service, positionmanager_service, knative_broker],
)

midnight_data_fetch_schedule = create_knative_schedule(
    kubernetes_provider=kubernetes_provider,
    target_service_name="datamanager",
    target_path="/equity-bars/fetch",
    cron_schedule="0 0 * * *",
    depends_on=[datamanager_service, knative_eventing_core],
)

monday_morning_open_positions_schedule = create_knative_schedule(
    kubernetes_provider=kubernetes_provider,
    target_service_name="predictionengine",
    target_path="/predictions/create",
    cron_schedule="0 10 * * 1",
    depends_on=[predictionengine_service, knative_eventing_core],
)

friday_evening_close_positions_schedule = create_knative_schedule(
    kubernetes_provider=kubernetes_provider,
    target_service_name="positionmanager",
    target_path="/positions/close",
    cron_schedule="0 13 * * 5",
    depends_on=[positionmanager_service, knative_eventing_core],
)

cluster_monitoring_scraper = create_prometheus_scraper(
    workspace_arn=configuration.require_secret("AWS_PROMETHEUS_WORKSPACE_ARN"),
    cluster=kubernetes_cluster,
)

datamanager_ingress = create_service_ingress(
    kubernetes_provider=kubernetes_provider,
    service_name="datamanager",
    cluster=kubernetes_cluster,
    depends_on=[alb_controller, datamanager_service],
)

datamanager_alb_url = datamanager_ingress.status.load_balancer.ingress[
    0
].hostname.apply(lambda hostname: f"http://{hostname}")

datamanager_api = create_api_gateway_with_auth(
    service_name="datamanager",
    target_url=datamanager_alb_url,
)

datamanager_api_access_policy = create_api_access_policy(
    api_gateway=datamanager_api,
    service_name="datamanager",
)

pulumi.export("DATAMANAGER_ALB_URL", datamanager_alb_url)
pulumi.export("DATAMANAGER_API_GATEWAY_URL", datamanager_api.api_endpoint)
pulumi.export("DATAMANAGER_API_ACCESS_POLICY_ARN", datamanager_api_access_policy.arn)
