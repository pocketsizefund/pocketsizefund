import project
from pulumi import FileAsset
from pulumi_gcp import cloudrun, secretmanager

config = Config()

grafana_administrator_password = config.require_secret("GRAFANA_ADMIN_PASSWORD")

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
)
grafana_administrator_password_version = secretmanager.SecretVersion(
    "grafana-administrator-password-version",
    secret=grafana_administrator_password_secret.id,
    secret_data=grafana_administrator_password,
)

prometheus_config = """
global:
  scrape_interval: 30s

scrape_configs:
  - job_name: 'cloud-run-services'
    metrics_path: /metrics
    static_configs:
      - targets:
          - datamanager
          - positionmanager
"""

prometheus_config_object = storage.BucketObject(
    "prometheus-config",
    bucket=buckets.grafana_dashboards_bucket.name,
    content=prometheus_config,
    content_type="text/yaml",
    name="prometheus.yaml",
)

prometheus_service = cloudrun.Service(
    "prometheus",
    location=project.REGION,
    template=cloudrun.ServiceTemplateArgs(
        spec=cloudrun.ServiceTemplateSpecArgs(
            service_account_name=project.platform_service_account.email,
            containers=[
                cloudrun.ServiceTemplateSpecContainerArgs(
                    image="prom/prometheus:v2.51.2",
                    args=[
                        "--config.file=/etc/prometheus/prometheus.yaml",
                        f"--storage.tsdb.path=/prometheus",
                    ],
                    resources=cloudrun.ServiceTemplateSpecContainerResourcesArgs(
                        limits={"cpu": "500m", "memory": "512Mi"}
                    ),
                    volume_mounts=[
                        cloudrun.ServiceTemplateSpecContainerVolumeMountArgs(
                            name="prometheus-config-volume",
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
                    name="prometheus-config-volume",
                    csi=cloudrun.ServiceTemplateSpecVolumeCsiArgs(
                        driver="gcsfuse.run.app",
                        read_only=True,
                        volume_attributes={
                            "bucketName": buckets.grafana_dashboards_bucket.name,
                            "mountOptions": "implicit-dirs",
                        },
                    ),
                ),
                cloudrun.ServiceTemplateSpecVolumeArgs(
                    name="prometheus-data", empty_dir={}
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
                    image="grafana/grafana:10.4.1",
                    envs=[
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="GF_SECURITY_ADMIN_PASSWORD",
                            value_from=cloudrun.ServiceTemplateSpecContainerEnvValueFromArgs(
                                secret_key_ref=cloudrun.ServiceTemplateSpecContainerEnvValueFromSecretKeyRefArgs(
                                    name=grafana_administrator_password_secret.name,
                                    key=grafana_administrator_password_version.version,
                                )
                            ),
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="GF_INSTALL_PLUGINS",
                            value="grafana-piechart-panel",
                        ),
                        cloudrun.ServiceTemplateSpecContainerEnvArgs(
                            name="GRAFANA_DASHBOARD_BUCKET",
                            value=buckets.grafana_dashboards_bucket.name,
                        ),
                    ],
                    ports=[
                        cloudrun.ServiceTemplateSpecContainerPortArgs(
                            container_port=3000,
                        ),
                    ],
                    resources=cloudrun.ServiceTemplateSpecContainerResourcesArgs(
                        limits={"cpu": "1", "memory": "1Gi"}
                    ),
                )
            ],
        ),
    ),
)
