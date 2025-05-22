import os
from glob import glob
import pulumi
import pulumi_docker as docker
from pulumi import Config, export

config = Config()
docker_username = config.require_secret("dockerhub_username")
docker_password = config.require_secret("dockerhub_password")
image_tag = config.get("image_tag") or pulumi.get_stack()
docker_org = config.get("docker_org") or "pocketsizefund"

images = {}
image_uris = {}
dockerfiles = glob(os.path.join("..", "application", "*", "Dockerfile"))

if not dockerfiles:
    raise Exception("No Dockerfiles found in ../application/*/Dockerfile")

for dockerfile in dockerfiles:
    service_name = os.path.basename(os.path.dirname(dockerfile))
    image_uri = f"docker.io/{docker_org}/{service_name}:{image_tag}"
    image_uris[service_name] = image_uri
    images[service_name] = docker.Image(
        f"{service_name}-image",
        build=docker.DockerBuild(
            context="..", dockerfile=f"application/{service_name}/Dockerfile"
        ),
        image_name=image_uri,
        registry=docker.RegistryArgs(
            server="docker.io",
            username=docker_username,
            password=docker_password,
        ),
    )

required_services = ["datamanager"]
for service in required_services:
    if service not in images:
        raise Exception(f"Required Docker image for service '{service}' not found")

export("image_uris", image_uris)
