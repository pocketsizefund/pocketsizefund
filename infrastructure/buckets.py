from pulumi import Config
from pulumi_gcp import storage
import project


config = Config()

production_data_bucket = storage.Bucket(
    "production-data-bucket",
    name=config.require_secret("production_data_bucket_name"),
    location=project.REGION,
    uniform_bucket_level_access=True,
)

grafana_dashboards_bucket = storage.Bucket(
    "grafana-dashboards",
    location=project.REGION,
    uniform_bucket_level_access=True,
)
