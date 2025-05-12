import pulumi
from pulumi_gcp.projects import Service, IAMMember

PROJECT = pulumi.Config("gcp").require("project")
REGION = pulumi.Config("gcp").get("region") or "us-central1"

config = pulumi.Config()

Service("enable-run", project=PROJECT, service="run.googleapis.com")
Service("enable-eventarc", project=PROJECT, service="eventarc.googleapis.com")
Service("enable-secretmanager", project=PROJECT, service="secretmanager.googleapis.com")
Service("enable-pubsub", project=PROJECT, service="pubsub.googleapis.com")

# Use an existing service account instead of creating a new one
# This avoids the 409 conflict errors
platform_sa_email = f"platform@{PROJECT}.iam.gserviceaccount.com"

# NOTE: Owner permissions must be manually added through Google Cloud Console
# due to "SOLO_MUST_INVITE_OWNERS" restriction

IAMMember(
    "pubsub-token-access",
    project=PROJECT,
    role="roles/pubsub.subscriber",
    member=f"serviceAccount:{platform_sa_email}",
)
