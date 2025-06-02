import base64
from pulumi_gcp import pubsub, cloudscheduler
from project import platform_service_account
from environment_variables import (
    create_environment_variable,
    ALPACA_API_KEY_ID,
    ALPACA_API_SECRET_KEY,
    GCP_PROJECT,
    DATA_BUCKET,
    DUCKDB_ACCESS_KEY,
    DUCKDB_SECRET,
    POLYGON_API_KEY,
)
from services import create_service
import topics  # noqa: F401
import buckets  # noqa: F401  # registers Pulumi `production_data_bucket` resource


datamanager_service = create_service(
    name="datamanager",
    envs=[
        ALPACA_API_KEY_ID,
        ALPACA_API_SECRET_KEY,
        GCP_PROJECT,
        DATA_BUCKET,
        DUCKDB_ACCESS_KEY,
        DUCKDB_SECRET,
        POLYGON_API_KEY,
    ],
)

DATAMANAGER_BASE_URL = create_environment_variable(
    "DATAMANAGER_BASE_URL", datamanager_service.statuses[0].url
)

predictionengine_service = create_service(
     "predictionengine", envs=[DATAMANAGER_BASE_URL]
)
    "predictionengine", envs=[DATAMANAGER_BASE_URL]
)


positionmanager_service = create_service(
    "positionmanager",
    envs=[
        ALPACA_API_KEY_ID,
        ALPACA_API_SECRET_KEY,
        DATAMANAGER_BASE_URL,
        # MINIMUM_PORTFOLIO_TICKERS,  # 20
        # MAXIMUM_PORTFOLIO_TICKERS,  # 20
    ],
)


datamanager_subscription = pubsub.Subscription(
    "datamanager-subscription",
    topic=topics.datamanager_ping.id,
    push_config=pubsub.SubscriptionPushConfigArgs(
        push_endpoint=datamanager_service.statuses[0].url,
        oidc_token=pubsub.SubscriptionPushConfigOidcTokenArgs(
            service_account_email=platform_service_account.email
        ),
    ),
)

datamanager_job = cloudscheduler.Job(
    "datamanager-job",
    schedule="0 0 * * *",
    time_zone="UTC",
    pubsub_target=cloudscheduler.JobPubsubTargetArgs(
        topic_name=topics.datamanager_ping.id,
        data=base64.b64encode(b"{}").decode("utf-8"),
    ),
)
