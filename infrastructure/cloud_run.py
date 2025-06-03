from pulumi_gcp import cloudrun, pubsub, cloudscheduler
import base64
from pulumi import Config
import project
import topics
import buckets

config = Config()

alpaca_api_key = config.require_secret("ALPACA_API_KEY_ID")
alpaca_api_secret = config.require_secret("ALPACA_API_SECRET_KEY")
duckdb_access_key = config.require_secret("DUCKDB_ACCESS_KEY")
duckdb_secret = config.require_secret("DUCKDB_SECRET")


datamanager_service = cloudrun.Service(
    "datamanager",
    location=project.REGION,
    template=cloudrun.ServiceTemplateArgs(
        spec=cloudrun.ServiceTemplateSpecArgs(
            service_account_name=project.platform_service_account.email,
            containers=[
                cloudrun.ServiceTemplateSpecContainerArgs(
                    image="pocketsizefund/datamanager:latest",
                    envs=[
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="ALPACA_API_KEY_ID",
                            value=alpaca_api_key,
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="ALPACA_API_SECRET_KEY",
                            value=alpaca_api_secret,
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="GCP_PROJECT",
                            value=project.PROJECT,
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="DATA_BUCKET",
                            value=buckets.production_data_bucket.name,
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="DUCKDB_ACCESS_KEY",
                            value=duckdb_access_key,
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="DUCKDB_SECRET",
                            value=duckdb_secret,
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="DATA_BUCKET",
                            value=buckets.production_data_bucket.name,
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="DUCKDB_ACCESS_KEY", value=duckdb_access_key
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="DUCKDB_SECRET",
                            value=duckdb_secret,
                        ),
                    ],
                ),
            ],
        ),
    ),
)

subscription = pubsub.Subscription(
    "datamanager-subscription",
    topic=topics.datamanager_ping.id,
    push_config=pubsub.SubscriptionPushConfigArgs(
        push_endpoint=datamanager_service.statuses[0].url,
        oidc_token=pubsub.SubscriptionPushConfigOidcTokenArgs(
            service_account_email=project.platform_service_account.email,
        ),
    ),
)

job = cloudscheduler.Job(
    "datamanager-job",
    schedule="0 0 * * *",
    time_zone="UTC",
    pubsub_target=cloudscheduler.JobPubsubTargetArgs(
        topic_name=topics.datamanager_ping.id,
        data=base64.b64encode(b"{}").decode("utf-8"),
    ),
)

positionmanager_service = cloudrun.Service(
    "positionmanager",
    location=project.REGION,
    template=cloudrun.ServiceTemplateArgs(
        spec=cloudrun.ServiceTemplateSpecArgs(
            service_account_name=project.platform_service_account.email,
            containers=[
                cloudrun.ServiceTemplateSpecContainerArgs(
                    image="pocketsizefund/positionmanager:latest",
                    envs=[
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="ALPACA_API_KEY",
                            value=config.require_secret("ALPACA_API_KEY"),
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="ALPACA_API_SECRET",
                            value=config.require_secret("ALPACA_API_SECRET"),
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="ALPACA_PAPER",
                            value=config.get("ALPACA_PAPER") or "true",
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="DATAMANAGER_BASE_URL",
                            value=datamanager_service.statuses[0].url,
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="MINIMUM_PORTFOLIO_TICKERS",
                            value=config.get("MINIMUM_PORTFOLIO_TICKERS") or "5",
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="MAXIMUM_PORTFOLIO_TICKERS",
                            value=config.get("MAXIMUM_PORTFOLIO_TICKERS") or "20",
                        ),
                    ],
                )
            ],
        )
    ),
)

predictionengine_service = cloudrun.Service(
    "predictionengine",
    location=project.REGION,
    template=cloudrun.ServiceTemplateArgs(
        spec=cloudrun.ServiceTemplateSpecArgs(
            service_account_name=project.platform_service_account.email,
            containers=[
                cloudrun.ServiceTemplateSpecContainerArgs(
                    image="pocketsizefund/predictionengine:latest",
                    envs=[
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="DATAMANAGER_BASE_URL",
                            value=datamanager_service.statuses[0].url,
                        )
                    ],
                )
            ],
        )
    ),
)
