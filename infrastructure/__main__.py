import buckets  # noqa: F401
import monitoring  # noqa: F401
import project
import pulumi_std as std
from environment_variables import (
    ALPACA_API_KEY,
    ALPACA_API_SECRET,
    DATA_BUCKET_NAME,
    DUCKDB_ACCESS_KEY,
    DUCKDB_SECRET,
    GCP_PROJECT,
    POLYGON_API_KEY,
    create_environment_variable,
)
from project import platform_service_account
from pulumi import ResourceOptions, export
from pulumi_gcp import cloudscheduler
from services import create_service

datamanager_service = create_service(
    name="datamanager",
    environment_variables=[
        ALPACA_API_KEY,
        ALPACA_API_SECRET,
        GCP_PROJECT,
        DATA_BUCKET_NAME,
        DUCKDB_ACCESS_KEY,
        DUCKDB_SECRET,
        POLYGON_API_KEY,
    ],
)

DATAMANAGER_BASE_URL = create_environment_variable(
    name="DATAMANAGER_BASE_URL",
    value=datamanager_service.statuses[0].url,
)

positionmanager_service = create_service(
    name="positionmanager",
    environment_variables=[
        ALPACA_API_KEY,
        ALPACA_API_SECRET,
        DATAMANAGER_BASE_URL,
    ],
)

POSITIONMANAGER_BASE_URL = create_environment_variable(
    name="POSITIONMANAGER_BASE_URL",
    value=positionmanager_service.statuses[0].url,
)

predictionengine_service = create_service(
    name="predictionengine",
    environment_variables=[
        DATAMANAGER_BASE_URL,
        POSITIONMANAGER_BASE_URL,
    ],
)

PREDICTIONENGINE_BASE_URL = create_environment_variable(
    name="PREDICTIONENGINE_BASE_URL",
    value=predictionengine_service.statuses[0].url,
)

eventtrigger_service = create_service(
    name="eventtrigger",
    environment_variables=[
        DATAMANAGER_BASE_URL,
        POSITIONMANAGER_BASE_URL,
        PREDICTIONENGINE_BASE_URL,
    ],
)

datamanager_data_fetch = cloudscheduler.Job(
    resource_name="datamanager-data-fetch",
    description="Fetch prior day data for storage",
    schedule="0 0 * * 1-5",
    time_zone="America/New_York",
    http_target=cloudscheduler.JobHttpTargetArgs(
        uri=eventtrigger_service.statuses[0].url.apply(lambda url: f"{url}/trigger"),
        http_method="POST",
        body=std.base64encode(input='{"event": "fetch_data"}').result,
        oidc_token=cloudscheduler.JobHttpTargetOidcTokenArgs(
            service_account_email=platform_service_account.email
        ),
    ),
    opts=ResourceOptions(depends_on=[project.cloudscheduler_api]),
)

predictionengine_create_positions = cloudscheduler.Job(
    resource_name="predictionengine-create-positions",
    description="Generate predictions and create positions",
    schedule="0 10 * * 1",
    time_zone="America/New_York",
    http_target=cloudscheduler.JobHttpTargetArgs(
        uri=eventtrigger_service.statuses[0].url.apply(lambda url: f"{url}/trigger"),
        http_method="POST",
        body=std.base64encode(input='{"event": "create_positions"}').result,
        oidc_token=cloudscheduler.JobHttpTargetOidcTokenArgs(
            service_account_email=platform_service_account.email
        ),
    ),
    opts=ResourceOptions(depends_on=[project.cloudscheduler_api]),
)


positionmanager_close_positions = cloudscheduler.Job(
    resource_name="positionmanager-close-positions",
    description="Close open positions",
    schedule="0 15 * * 5",
    time_zone="America/New_York",
    http_target=cloudscheduler.JobHttpTargetArgs(
        uri=eventtrigger_service.statuses[0].url.apply(lambda url: f"{url}/trigger"),
        http_method="POST",
        body=std.base64encode(input='{"event": "close_positions"}').result,
        oidc_token=cloudscheduler.JobHttpTargetOidcTokenArgs(
            service_account_email=platform_service_account.email
        ),
    ),
    opts=ResourceOptions(depends_on=[project.cloudscheduler_api]),
)

export(
    name="DATAMANAGER_BASE_URL",
    value=datamanager_service.statuses[0].url,
)

export(
    name="DATAMANAGER_METRICS_URL",
    value=datamanager_service.statuses[0].url.apply(lambda url: f"{url}/metrics"),
)

export(
    name="POSITIONMANAGER_METRICS_URL",
    value=positionmanager_service.statuses[0].url.apply(lambda url: f"{url}/metrics"),
)

export(
    name="EVENTTRIGGER_BASE_URL",
    value=eventtrigger_service.statuses[0].url,
)
