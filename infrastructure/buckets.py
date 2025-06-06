import project
from pulumi import Config
from pulumi_gcp import storage

config = Config()

production_data_bucket = storage.Bucket(
    "production-data-bucket",
    name=config.require_secret("production_data_bucket_name"),
    location=project.REGION,
    uniform_bucket_level_access=True,
)


storage.BucketIAMMember(
    "platform-object-admin-access",
    bucket=production_data_bucket.name,
    role="roles/storage.objectAdmin",
    member=project.platform_service_account.email.apply(
        lambda e: f"serviceAccount:{e}"
    ),
)
