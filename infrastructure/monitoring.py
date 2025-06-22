import project
from pulumi import ResourceOptions
from pulumi.config import Config
from pulumi_gcp import monitoring, secretmanager

configuration = Config()

grafana_administrator_password = configuration.require_secret(
    "GRAFANA_ADMINISTRATOR_PASSWORD"
)

grafana_administrator_password_secret = secretmanager.Secret(
    "grafana-administrator-password",
    replication={
        "user_managed": {
            "replicas": [
                {
                    "location": "us-central1",
                },
                {
                    "location": "us-east1",
                },
            ]
        }
    },
    opts=ResourceOptions(depends_on=[project.secretmanager]),
)

grafana_administrator_password_version = secretmanager.SecretVersion(
    "grafana-administrator-password-version",
    secret=grafana_administrator_password_secret.id,
    secret_data=grafana_administrator_password,
)

prometheus_alert_emails = configuration.require_secret("PROMETHEUS_ALERT_EMAILS")


def create_notification_channels(
    email_string: str,
) -> list[monitoring.NotificationChannel]:
    emails = email_string.split(",")
    channels = []
    for i, email in enumerate(emails):
        channel = monitoring.NotificationChannel(
            f"email-notifications-{i}",
            display_name=f"Email Notifications - {email.strip()}",
            type="email",
            labels={"email_address": email.strip()},
            enabled=True,
            opts=ResourceOptions(depends_on=[project.monitoring_api]),
        )
        channels.append(channel)

    return channels


email_notification_channels = prometheus_alert_emails.apply(
    create_notification_channels
)
