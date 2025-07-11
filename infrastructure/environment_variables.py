import pulumi
from pulumi.config import Config

configuration = Config()


alpaca_api_key = configuration.require_secret("ALPACA_API_KEY")
alpaca_api_secret = configuration.require_secret("ALPACA_API_SECRET")
data_bucket_name = configuration.require_secret("DATA_BUCKET_NAME")
polygon_api_key = configuration.require_secret("POLYGON_API_KEY")
duckdb_access_key = configuration.require_secret("DUCKDB_ACCESS_KEY")
duckdb_secret = configuration.require_secret("DUCKDB_SECRET")

environment_variables = pulumi.Output.all(
    [
        ("ALPACA_API_KEY", alpaca_api_key),
        ("ALPACA_API_SECRET", alpaca_api_secret),
        ("DATA_BUCKET_NAME", data_bucket_name),
        ("POLYGON_API_KEY", polygon_api_key),
        ("DUCKDB_ACCESS_KEY", duckdb_access_key),
        ("DUCKDB_SECRET", duckdb_secret),
    ]
).apply(lambda secrets: dict(secrets))
