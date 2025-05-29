import os
from datetime import datetime, timezone
from glob import glob
from pathlib import Path

import pulumi
import pulumi_docker_build as docker_build
from loguru import logger
from pulumi import Config

config = Config()
dockerhub_username = config.require_secret("dockerhub_username")
dockerhub_password = config.require_secret("dockerhub_password")

application_path = Path("../application/").resolve()
dockerfile_paths = [
    app.relative_to(application_path) for app in application_path.glob("*/Dockerfile")
]

tags = [
    "latest",
    datetime.now(tz=timezone.utc).strftime("%Y%m%d"),
]

images = {}
for dockerfile in dockerfile_paths:
    service_dir = dockerfile.parent
    service_name = dockerfile.name
    logger.info(f"Creating image for service: {service_name}")

    images[service_name] = docker_build.Image(
        f"{service_name}-image",
        tags=[f"pocketsizefund/{service_name}:{tag}" for tag in tags],
        context=docker_build.BuildContextArgs(
            location=service_dir,
        ),
        platforms=[
            docker_build.Platform.LINUX_AMD64,
            docker_build.Platform.LINUX_ARM64,
        ],
        push=True,
        registries=[
            docker_build.RegistryArgs(
                address="docker.io",
                username=dockerhub_username,
                password=dockerhub_password,
            ),
        ],
    )

    pulumi.export(f"{service_name}-ref", images[service_name].ref)

logger.info(f"Available image services: {list(images.keys())}")
