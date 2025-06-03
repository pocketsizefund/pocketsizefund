import pulumi
from pulumi_gcp.projects import IAMMember, Service
from pulumi_gcp.serviceaccount import Account

PROJECT = pulumi.Config("gcp").require("project")
REGION = pulumi.Config("gcp").require("region")

cloudrun = Service(
    "enable-run",
    project=PROJECT,
    service="run.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)
eventarc = Service(
    "enable-eventarc",
    project=PROJECT,
    service="eventarc.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)
secretmanager = Service(
    "enable-secretmanager",
    project=PROJECT,
    service="secretmanager.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)

pubsub_api = Service(
    "enable-pubsub",
    project=PROJECT,
    service="pubsub.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)

container_registry = Service(
    "enable-containerregistry",
    project=PROJECT,
    service="containerregistry.googleapis.com",
    disable_dependent_services=True,
    disable_on_destroy=True,
)


platform_service_account = Account(
    "platform-service-account",
    account_id="platform",
    display_name="Platform-Wide Cloud Run Service Account",
)

IAMMember(
    "pubsub-token-access",
    project=PROJECT,
    role="roles/pubsub.subscriber",
    member=platform_service_account.email.apply(
        lambda e: f"serviceAccount:{e}",
    ),  # ty: ignore[missing-argument]
)
