from project import PROJECT, platform_service_account
from pulumi import Output, export
from pulumi.config import Config
from pulumi_gcp.cloudrun import ServiceTemplateSpecContainerEnvArgs
from pulumi_gcp.storage import HmacKey

config = Config()

ENVIRONMENT_VARIABLE = ServiceTemplateSpecContainerEnvArgs


def create_environment_variable(
    name: str, value: str | Output[str]
) -> ENVIRONMENT_VARIABLE:
    return ServiceTemplateSpecContainerEnvArgs(name=name, value=value)


GCP_PROJECT = create_environment_variable(
    name="GCP_PROJECT", value=config.require_secret("GCP_PROJECT")
)

ALPACA_API_KEY = create_environment_variable(
    name="ALPACA_API_KEY", value=config.require_secret("ALPACA_API_KEY")
)
ALPACA_API_SECRET = create_environment_variable(
    name="ALPACA_API_SECRET", value=config.require_secret("ALPACA_API_SECRET")
)

DATA_BUCKET_NAME = create_environment_variable(
    name="DATA_BUCKET_NAME", value=config.require_secret("DATA_BUCKET_NAME")
)
POLYGON_API_KEY = create_environment_variable(
    name="POLYGON_API_KEY", value=config.require_secret("POLYGON_API_KEY")
)

hmac_key: HmacKey = HmacKey(
    "platform-duckdb-hmac",
    service_account_email=platform_service_account.email,
    project=PROJECT,
)

DUCKDB_ACCESS_KEY = create_environment_variable(
    name="DUCKDB_ACCESS_KEY", value=hmac_key.access_id
)
DUCKDB_SECRET = create_environment_variable(name="DUCKDB_SECRET", value=hmac_key.secret)

export("DUCKDB_ACCESS_KEY", hmac_key.access_id)
export("DUCKDB_SECRET", hmac_key.secret)
