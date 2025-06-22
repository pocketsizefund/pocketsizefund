import project
from pulumi import Config
from pulumi_gcp import storage

config = Config()

data_bucket_name = config.require_secret("DATA_BUCKET_NAME")

storage.BucketIAMMember(
    "platform-object-administrator-access",
    bucket=data_bucket_name,
    role="roles/storage.objectAdmin",
    member=project.platform_service_account.email.apply(
        lambda e: f"serviceAccount:{e}"
    ),
)

grafana_dashboards_bucket = storage.Bucket(
    "grafana-dashboards-bucket",
    name=config.require_secret("GRAFANA_DASHBOARDS_BUCKET_NAME"),
    location=project.REGION,
    uniform_bucket_level_access=True,
)

storage.BucketIAMMember(
    "grafana-dashboards-bucket-object-administrator-access",
    bucket=grafana_dashboards_bucket.name,
    role="roles/storage.objectAdmin",
    member=project.platform_service_account.email.apply(
        lambda e: f"serviceAccount:{e}"
    ),
)
