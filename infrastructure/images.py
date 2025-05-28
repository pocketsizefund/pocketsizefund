import os
from datetime import datetime, timezone
from glob import glob
import pulumi
import pulumi_docker_build as docker_build
from pulumi import Config

config = Config()
dockerhub_username = config.require_secret("dockerhub_username")
dockerhub_password = config.require_secret("dockerhub_password")

dockerfile_paths = glob(os.path.join("..", "application", "*", "Dockerfile"))
dockerfile_paths = [os.path.relpath(dockerfile) for dockerfile in dockerfile_paths]

tags = [
    "latest",
    datetime.now(tz=timezone.utc).strftime("%Y%m%d"),
]

images = {}
for dockerfile in dockerfile_paths:
    service_dir = os.path.dirname(dockerfile)
    service_name = os.path.basename(service_dir)
    print(f"Creating image for service: {service_name}")

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

datamanager_image = images.get("datamanager")
positionmanager_image = images.get("positionmanager")
predictionengine_image = images.get("predictionengine")

print(f"Available image services: {list(images.keys())}")
