import pulumi
from pulumi_gcp.projects import IAMMember, Service
from pulumi_gcp.serviceaccount import Account

PROJECT = pulumi.Config("gcp").require("project")
REGION = pulumi.Config("gcp").require("region")

cloudrun = Service(
    resource_name="enable-run",
    project=PROJECT,
    service="run.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)

eventarc = Service(
    resource_name="enable-eventarc",
    project=PROJECT,
    service="eventarc.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)

secretmanager = Service(
    resource_name="enable-secretmanager",
    project=PROJECT,
    service="secretmanager.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)

pubsub_api = Service(
    resource_name="enable-pubsub",
    project=PROJECT,
    service="pubsub.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)

container_registry = Service(
    resource_name="enable-containerregistry",
    project=PROJECT,
    service="containerregistry.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)

cloudbuild = Service(
    resource_name="enable-cloudbuild",
    project=PROJECT,
    service="cloudbuild.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)

monitoring_api = Service(
    resource_name="enable-monitoring",
    project=PROJECT,
    service="monitoring.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)

cloudscheduler_api = Service(
    resource_name="enable-cloudscheduler",
    project=PROJECT,
    service="cloudscheduler.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)


platform_service_account = Account(
    resource_name="platform-service-account",
    account_id="platform",
    display_name="Platform-Wide Cloud Run Service Account",
)


IAMMember(
    resource_name="pubsub-token-access",
    project=PROJECT,
    role="roles/pubsub.subscriber",
    member=platform_service_account.email.apply(
        lambda e: f"serviceAccount:{e}",
    ),  # ty: ignore[missing-argument]
)

IAMMember(
    resource_name="platform-service-account-owner",
    project=PROJECT,
    role="roles/owner",
    member=platform_service_account.email.apply(lambda e: f"serviceAccount:{e}"),  # type: ignore
)
