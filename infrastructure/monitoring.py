import project
from pulumi import FileAsset
from pulumi_gcp import cloudrun, secretmanager

prometheus_config_secret = secretmanager.Secret("prometheus-config")
prometheus_config_version = secretmanager.SecretVersion(
    "prometheus-config-version",
    secret=prometheus_config_secret.id,
    secret_data=FileAsset("infrastructure/prometheus.yaml"),
)

prometheus_service = cloudrun.Service(
    "prometheus",
    location=project.REGION,
    template=cloudrun.ServiceTemplateArgs(
        spec=cloudrun.ServiceTemplateSpecArgs(
            service_account_name=project.platform_service_account.email,
            containers=[
                cloudrun.ServiceTemplateSpecContainerArgs(
                    image="prom/prometheus:latest",
                    args=["--config.file=/etc/prometheus/prometheus.yaml"],
                    volume_mounts=[
                        cloudrun.ServiceTemplateSpecContainerVolumeMountArgs(
                            name="prometheus-config",
                            mount_path="/etc/prometheus",
                        ),
                    ],
                    ports=[
                        cloudrun.ServiceTemplateSpecContainerPortArgs(
                            container_port=9090,
                        ),
                    ],
                ),
            ],
            volumes=[
                cloudrun.ServiceTemplateSpecVolumeArgs(
                    name="prometheus-config",
                    secret=cloudrun.ServiceTemplateSpecVolumeSecretArgs(
                        secret=prometheus_config_secret.id,
                        items=[
                            cloudrun.ServiceTemplateSpecVolumeSecretItemArgs(
                                path="prometheus.yaml",
                                version=prometheus_config_version.version,
                            ),
                        ],
                    ),
                ),
            ],
        ),
    ),
)

grafana_service = cloudrun.Service(
    "grafana",
    location=project.REGION,
    template=cloudrun.ServiceTemplateArgs(
        spec=cloudrun.ServiceTemplateSpecArgs(
            service_account_name=project.platform_service_account.email,
            containers=[
                cloudrun.ServiceTemplateSpecContainerArgs(
                    image="grafana/grafana:latest",
                    ports=[
                        cloudrun.ServiceTemplateSpecContainerPortArgs(
                            container_port=3000,
                        ),
                    ],
                ),
            ],
        ),
    ),
)
