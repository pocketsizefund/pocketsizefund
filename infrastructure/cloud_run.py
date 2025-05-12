from pulumi_gcp import cloudrun, pubsub, cloudscheduler
import base64
from pulumi import Config
import topics

config = Config()
gcp_config = Config("gcp")
region = gcp_config.get("region") or "us-central1"
project = gcp_config.require("project")

alpaca_api_key = config.require_secret("ALPACA_API_KEY_ID")
alpaca_api_secret = config.require_secret("ALPACA_API_SECRET_KEY")

# Use existing service account instead of creating a new one
platform_sa_email = f"platform@{project}.iam.gserviceaccount.com"

service = cloudrun.Service(
    "platformservice",
    location=region,
    template=cloudrun.ServiceTemplateArgs(
        spec=cloudrun.ServiceTemplateSpecArgs(
            service_account_name=platform_sa_email,
            containers=[
                cloudrun.ServiceTemplateSpecContainerArgs(
                    image="pocketsizefund/platform:latest",
                    args=["--period=1"],
                    envs=[
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="ALPACA_API_KEY_ID", value=alpaca_api_key
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="ALPACA_API_SECRET_KEY", value=alpaca_api_secret
                        ),
                    ],
                )
            ],
        ),
    ),
)

sub = pubsub.Subscription(
    "platform-sub",
    topic=topics.platform_ping.id,
    push_config=pubsub.SubscriptionPushConfigArgs(
        push_endpoint=service.statuses[0].url,
        oidc_token=pubsub.SubscriptionPushConfigOidcTokenArgs(
            service_account_email=platform_sa_email
        ),
    ),
)

job = cloudscheduler.Job(
    "platform-job",
    schedule="0 * * * *",
    time_zone="UTC",
    region=region,
    pubsub_target=cloudscheduler.JobPubsubTargetArgs(
        topic_name=topics.platform_ping.id,
        data=base64.b64encode(b"{}").decode("utf-8"),
    ),
)
