from pathlib import Path

import pulumi
import pulumi_docker_build as docker_build
from pulumi.config import Config

configuration = Config()


def build_image(
    service_name: str,
    service_version: str,
) -> docker_build.Image:
    service_directory = Path("../application") / service_name
    if not service_directory.exists():
        message = f"Service directory not found: {service_directory}"
        raise FileNotFoundError(message)

    image = docker_build.Image(
        resource_name=f"pocketsizefund-{service_name}-image",
        tags=[f"pocketsizefund/{service_name}:{service_version}"],
        context=docker_build.BuildContextArgs(location=str(service_directory)),
        platforms=[
            docker_build.Platform.LINUX_AMD64,
            docker_build.Platform.LINUX_ARM64,
        ],
        push=True,
        registries=[
            docker_build.RegistryArgs(
                address="docker.io",
                username=configuration.require_secret("DOCKERHUB_USERNAME"),
                password=configuration.require_secret("DOCKERHUB_PASSWORD"),
            )
        ],
    )

    pulumi.export(f"{service_name.upper()}_IMAGE", image.ref)

    return image
