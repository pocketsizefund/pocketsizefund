from pulumi_gcp import serviceaccount, cloudrun, pubsub, cloudscheduler
import base64
from pulumi import Config
import topics

config = Config()

alpaca_api_key = config.require_secret("ALPACA_API_KEY_ID")
alpaca_api_secret = config.require_secret("ALPACA_API_SECRET_KEY")


service_account = serviceaccount.Account(
    "platform-sa",
    account_id="platform-sa",
    display_name="platform cloud run service account",
)


service = cloudrun.Service(
    "platformservice",
    location="us-central1",
    template=cloudrun.ServiceTemplateArgs(
        spec=cloudrun.ServiceTemplateSpecArgs(
            service_account_name=service_account.email,
            containers=[
                cloudrun.ServiceTemplateSpecContainerArgs(
                    image="pocketsizefund/platform:latest",
                    args=["--period=1"],
                    envs=[
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="ALPACA_API_KEY_ID", value=api_key
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="ALPACA_API_SECRET_KEY", value=api_secret
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
            service_account_email=service_account.email
        ),
    ),
)

job = cloudscheduler.Job(
    "platform-job",
    schedule="0 * * * *",
    time_zone="UTC",
    pubsub_target=cloudscheduler.JobPubsubTargetArgs(
        topic_name=topics.platform_ping.id,
        data=base64.b64encode(b"{}").decode("utf-8"),
    ),
)
