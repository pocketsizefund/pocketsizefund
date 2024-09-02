from invoke import task
from kubernetes import client, config
from kubernetes.client.rest import ApiException
import subprocess
from rich.progress import Progress, SpinnerColumn, TextColumn
import asyncio

config.load_kube_config()

v1 = client.CoreV1Api()
apps_v1 = client.AppsV1Api()


@task
def create_namespace(c, name):
    namespace = client.V1Namespace(metadata=client.V1ObjectMeta(name=name))
    try:
        v1.create_namespace(namespace)
        print(f"Namespace {name} created.")
    except ApiException as e:
        if e.status == 409:
            print(f"Namespace {name} already exists.")
        else:
            raise

@task
def apply_yaml(c, url):
    subprocess.run(["kubectl", "apply", "-f", url], check=True)

@task
def patch_config_map(c, name, namespace, patch):
    try:
        v1.patch_namespaced_config_map(name, namespace, patch)
        print(f"ConfigMap {name} in namespace {namespace} patched.")
    except ApiException as e:
        print(f"Error patching ConfigMap: {e}")

async def verify_pods_async(namespace, min_count=1):
    while True:
        pods = v1.list_namespaced_pod(namespace)
        if len(pods.items) >= min_count:
            return True
        await asyncio.sleep(1)

async def verify_service_async(name, namespace):
    while True:
        try:
            v1.read_namespaced_service(name, namespace)
            return True
        except ApiException:
            await asyncio.sleep(1)

@task
def create(c):
    create_namespace(c,"knative-serving")
    create_namespace(c,"kourier-system")
    create_namespace(c,"knative-eventing")

    apply_yaml(c,"https://github.com/knative/serving/releases/download/knative-v1.10.1/serving-crds.yaml")
    apply_yaml(c,"https://github.com/knative/serving/releases/download/knative-v1.10.1/serving-core.yaml")
    apply_yaml(c,"https://github.com/knative/net-kourier/releases/download/knative-v1.10.0/kourier.yaml")

    patch_config_map(c,"config-network", "knative-serving", {"data": {"ingress-class": "kourier.ingress.networking.knative.dev"}})

    apply_yaml(c,"https://github.com/knative/serving/releases/download/knative-v1.10.1/serving-default-domain.yaml")

    apply_yaml(c,"https://github.com/knative/eventing/releases/download/knative-v1.10.0/eventing-crds.yaml")
    apply_yaml(c,"https://github.com/knative/eventing/releases/download/knative-v1.10.0/eventing-core.yaml")

    async def run_verifications():
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            transient=True,
        ) as progress:
            tasks = [
                progress.add_task("Verifying knative-serving pods...", total=None),
                progress.add_task("Verifying kourier-system pods...", total=None),
                progress.add_task("Verifying kourier service...", total=None),
                progress.add_task("Verifying knative-eventing pods...", total=None),
            ]

            results = await asyncio.gather(
                verify_pods_async("knative-serving"),
                verify_pods_async("kourier-system"),
                verify_service_async("kourier", "kourier-system"),
                verify_pods_async("knative-eventing"),
            )

            for task, result in zip(tasks, results):
                progress.update(task, completed=True)

            if all(results):
                print("Knative installation completed and verified.")
            else:
                print("Error: Some verifications failed.")

    asyncio.run(run_verifications())
