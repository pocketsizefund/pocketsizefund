import pulumi
import pulumi_aws as aws
from pulumi.config import Config

configuration = Config()


alpaca_api_key = configuration.require_secret("ALPACA_API_KEY")
alpaca_api_secret = configuration.require_secret("ALPACA_API_SECRET")
data_bucket_name = configuration.require_secret("DATA_BUCKET_NAME")
polygon_api_key = configuration.require_secret("POLYGON_API_KEY")
duckdb_access_key = configuration.require_secret("DUCKDB_ACCESS_KEY")
duckdb_secret = configuration.require_secret("DUCKDB_SECRET")
aws_region = configuration.get("aws:region") or "us-east-1"


def create_environment_variables(
    duckdb_user_access_key: aws.iam.AccessKey,
) -> pulumi.Output[dict[str, str]]:
    return pulumi.Output.all(
        [
            ("ALPACA_API_KEY", alpaca_api_key),
            ("ALPACA_API_SECRET", alpaca_api_secret),
            ("DATA_BUCKET_NAME", data_bucket_name),
            ("POLYGON_API_KEY", polygon_api_key),
            ("DUCKDB_ACCESS_KEY", duckdb_access_key),
            ("DUCKDB_SECRET", duckdb_secret),
            ("AWS_REGION", aws_region),
            ("DUCKDB_USER_ACCESS_KEY_ID", duckdb_user_access_key.id),
            ("DUCKDB_USER_ACCESS_KEY_SECRET", duckdb_user_access_key.secret),
        ]
    ).apply(lambda secrets: dict(secrets))
