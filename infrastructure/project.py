import pulumi
from pulumi_gcp.projects import Service, IAMMember
from pulumi_gcp.serviceaccount import Account

PROJECT = pulumi.Config("gcp").require("project")
REGION = pulumi.Config("gcp").require("region")

config = pulumi.Config()


Service("enable-run", project=PROJECT, service="run.googleapis.com")
Service("enable-eventarc", project=PROJECT, service="eventarc.googleapis.com")
Service("enable-secretmanager", project=PROJECT, service="secretmanager.googleapis.com")
Service("enable-pubsub", project=PROJECT, service="pubsub.googleapis.com")

service_account = Account(
    "platform-service-acct",
    account_id="platform",
    display_name="Cloud Run Price Model Service Account",
)

IAMMember(
    "pubsub-token-access",
    project=PROJECT,
    role="roles/pubsub.subscriber",
    member=service_account.email.apply(lambda e: f"serviceAccount:{e}"),
)
