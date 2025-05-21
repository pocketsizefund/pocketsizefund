import os
from glob import glob
import pulumi
import pulumi_docker as docker
from pulumi import Config

config = Config()
docker_username = config.require_secret("dockerhub_username")
docker_password = config.require_secret("dockerhub_password")
image_tag = config.get("image_tag") or pulumi.get_stack()

images = {}
for dockerfile in glob(os.path.join("..", "application", "*", "Dockerfile")):
    service_name = os.path.basename(os.path.dirname(dockerfile))
    images[service_name] = docker.Image(
        f"{service_name}-image",
        build=docker.DockerBuild(
            context="..", dockerfile=f"application/{service_name}/Dockerfile"
        ),
        image_name=f"docker.io/pocketsizefund/{service_name}:{image_tag}",
        registry=docker.RegistryArgs(
            server="docker.io",
            username=docker_username,
            password=docker_password,
        ),
    )
