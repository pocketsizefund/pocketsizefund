from pathlib import Path
import tomllib

import project
import pulumi_docker_build as docker_build
from environment_variables import ENVIRONMENT_VARIABLE
from pulumi.config import Config
from pulumi_gcp.cloudrun import (
    Service,
    ServiceTemplateArgs,
    ServiceTemplateSpecArgs,
    ServiceTemplateSpecContainerArgs,
    ServiceTemplateSpecContainerStartupProbeArgs,
    ServiceTemplateSpecContainerStartupProbeHttpGetArgs,
)

config = Config()


def create_service(
    name: str, envs: list[ENVIRONMENT_VARIABLE] | None = None
) -> Service:
    if envs is None:
        envs = []

    try:
        with Path("pyproject.toml").open("rb") as f:
            project_data = tomllib.load(f)
            version = project_data.get("project", {}).get("version")
            if not version:
                raise ValueError("Version not found in pyproject.toml")
    except (FileNotFoundError, tomllib.TOMLDecodeError, ValueError) as e:
        raise RuntimeError(f"Failed to read version from pyproject.toml: {e}") from e
    service_dir = Path("../application") / name
    if not service_dir.exists():
        raise FileNotFoundError(f"Service directory not found: {service_dir}")

    image = docker_build.Image(
        f"{name}-image",
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
                username=config.require_secret("dockerhub_username"),
                password=config.require_secret("dockerhub_password"),
            )
        ],
    )

    return Service(
        name,
        location=project.REGION,
        template=ServiceTemplateArgs(
            spec=ServiceTemplateSpecArgs(
                service_account_name=project.platform_service_account.email,
                containers=[
                    ServiceTemplateSpecContainerArgs(
                        image=f"pocketsizefund/{name}:{version}",
                        envs=envs,
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
