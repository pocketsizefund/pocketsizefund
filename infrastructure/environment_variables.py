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

ALPACA_API_KEY_ID = create_environment_variable(
    name="ALPACA_API_KEY_ID", value=config.require_secret("ALPACA_API_KEY_ID")
)
ALPACA_API_SECRET_KEY = create_environment_variable(
    name="ALPACA_API_SECRET_KEY", value=config.require_secret("ALPACA_API_SECRET_KEY")
)

DATA_BUCKET = create_environment_variable(
    name="DATA_BUCKET", value=config.require_secret("DATA_BUCKET")
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

export("duckdb_access_key", hmac_key.access_id)
export("duckdb_secret", hmac_key.secret)
