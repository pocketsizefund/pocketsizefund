import tomllib
from pathlib import Path

import project
import pulumi_docker_build as docker_build
from environment_variables import ENVIRONMENT_VARIABLE
from pulumi import ResourceOptions
from pulumi.config import Config
from pulumi_gcp.cloudrun import (
    Service,
    ServiceTemplateArgs,
    ServiceTemplateMetadataArgs,
    ServiceTemplateSpecArgs,
    ServiceTemplateSpecContainerArgs,
    ServiceTemplateSpecContainerStartupProbeArgs,
    ServiceTemplateSpecContainerStartupProbeHttpGetArgs,
)

config = Config()


def create_service(
    name: str,
    environment_variables: list[ENVIRONMENT_VARIABLE] | None = None,
    enable_prometheus: bool = True,  # noqa: FBT001, FBT002
) -> Service:
    if environment_variables is None:
        environment_variables = []

    try:
        with Path("pyproject.toml").open("rb") as f:
            project_data = tomllib.load(f)
            version = project_data.get("project", {}).get("version")

    except (FileNotFoundError, tomllib.TOMLDecodeError, ValueError) as e:
        message = f"Failed to read version from pyproject.toml: {e}"
        raise RuntimeError(message) from e

    service_dir = Path("../application") / name
    if not service_dir.exists():
        message = f"Service directory not found: {service_dir}"
        raise FileNotFoundError(message)

    image = docker_build.Image(
        resource_name=f"{name}-image",
        tags=[f"pocketsizefund/{name}:{version}"],
        context=docker_build.BuildContextArgs(location=str(service_dir)),
        platforms=[
            docker_build.Platform.LINUX_AMD64,
            docker_build.Platform.LINUX_ARM64,
        ],
        push=True,
        registries=[
            docker_build.RegistryArgs(
                address="docker.io",
                username=config.require_secret("DOCKERHUB_USERNAME"),
                password=config.require_secret("DOCKERHUB_PASSWORD"),
            )
        ],
    )

    # annotations for Managed Service for Prometheus
    annotations = {}
    if enable_prometheus:
        annotations.update(
            {
                "run.googleapis.com/cpu-throttling": "false",
                "prometheus.googleapis.com/scrape": "true",
                "prometheus.googleapis.com/port": "8080",
                "prometheus.googleapis.com/path": "/metrics",
                "prometheus.googleapis.com/scrape_interval": "60s",
            }
        )

    return Service(
        resource_name=name,
        opts=ResourceOptions(depends_on=[image, project.cloudrun]),
        location=project.REGION,
        template=ServiceTemplateArgs(
            metadata=ServiceTemplateMetadataArgs(annotations=annotations),
            spec=ServiceTemplateSpecArgs(
                service_account_name=project.platform_service_account.email,
                containers=[
                    ServiceTemplateSpecContainerArgs(
                        image=f"pocketsizefund/{name}:{version}",
                        envs=environment_variables,
                        startup_probe=ServiceTemplateSpecContainerStartupProbeArgs(
                            initial_delay_seconds=60,
                            period_seconds=60,
                            failure_threshold=50,
                            http_get=ServiceTemplateSpecContainerStartupProbeHttpGetArgs(
                                path="/health",
                                port=8080,
                            ),
                        ),
                    )
                ],
            ),
        ),
    )
