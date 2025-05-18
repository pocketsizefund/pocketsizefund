from pulumi import Config
from pulumi_gcp import storage
import project


config = Config()

prod_bucket_name = config.require_secret("prod_bucket_name")

prod_data_bucket = storage.Bucket(
    "prod-data-bucket",
    name=prod_bucket_name,
    location=project.REGION,
    uniform_bucket_level_access=True,
)
