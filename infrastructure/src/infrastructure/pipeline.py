import os
import base64
import yaml
from prefect import task, flow
import subprocess
from kubernetes import client, config
from kubernetes.client.rest import ApiException
from infrastructure.models import Namespace, Namespaces
import shutil
import subprocess
import os


config.load_kube_config()
k8s = client.CoreV1Api()

async def check_cluster_exists(cluster_name) -> bool:
    result = subprocess.run(["kind", "get", "clusters"], capture_output=True, text=True)
    return cluster_name in result.stdout

async def create_cluster(cluster_name):
    subprocess.run(["kind", "create", "cluster", "--name", cluster_name, "--config=local/k8s/cluster.yaml"])

async def configure_nodes(cluster_name):
    result = subprocess.run(["kind", "get", "nodes", "--name", cluster_name], capture_output=True, text=True)
    nodes = result.stdout.strip().split('\n')

    for node in nodes:
        required_modules = ["ip_tables", "iptable_nat", "iptable_mangle", "iptable_filter"]

        for module in required_modules:
            result = subprocess.run(["docker", "exec", node, "lsmod"], capture_output=True, text=True)
            if module not in result.stdout:
                print(f"Warning: Kernel module {module} not loaded on node {node}")


async def get_namespaces():
    namespaces = [ns.metadata for ns in k8s.list_namespace().items]

    return [{"name": ns.name, "namespace": ns.namespace, "labels": ns.labels} for ns in namespaces]

async def create_namespaces(namespace):
    try:
        k8s.create_namespace(client.V1Namespace(metadata=client.V1ObjectMeta(name=namespace)))
    except ApiException as e:
        if e.status == 409:
            print(f"Namespace {namespace} already exists")
        else:
            raise e

async def install_knative():
    if shutil.which("kn-quickstart") is None:
        subprocess.run(["git", "clone", "https://github.com/knative-extensions/kn-plugin-quickstart.git"])
        os.chdir("kn-plugin-quickstart")
        subprocess.run(["./hack/build.sh"])
        shutil.move("kn-quickstart", "/usr/local/bin")
        os.chdir("..")
        shutil.rmtree("kn-plugin-quickstart")
    subprocess.run(["kn", "quickstart", "kind", "--install-serving", "--install-eventing"])


async def create_secrets():
    secret = client.V1Secret(
        api_version="v1",
        kind="Secret",
        metadata=client.V1ObjectMeta(name="platform"),
        type="Opaque",
        string_data={
            "APCA_API_KEY_ID": os.environ["APCA_API_KEY_ID"],
            "APCA_API_SECRET_KEY": os.environ["APCA_API_SECRET_KEY"],
        }
    )

    try:
        k8s.create_namespaced_secret(namespace="live", body=secret)
    except ApiException as e:
        if e.status in [404, 409]:
            print(f"Secret already exists")
        else:
            raise e


async def install_prometheus():
    try:
        subprocess.run(["helm", "repo", "add", "prometheus-community", "https://prometheus-community.github.io/helm-charts"])
        subprocess.run(["helm", "repo", "update"])
        subprocess.run(["helm", "install", "prometheus", "prometheus-community/kube-prometheus-stack", "--namespace", "monitoring", "--create-namespace"])
    except subprocess.CalledProcessError as e:
        print(e.output)
        raise e


async def install_grafana():
    try:
        subprocess.run(["helm", "repo", "add", "grafana", "https://grafana.github.io/helm-charts"])
        subprocess.run(["helm", "repo", "update"])
    except subprocess.CalledProcessError as e:
        print(e.output)
        raise e

async def install_loki():
    try:
        subprocess.run(["helm", "install", "loki", "grafana/loki-stack", "--namespace", "monitoring"])
    except subprocess.CalledProcessError as e:
        print(e.output)
        raise e



async def pipeline(cluster_name: str = "pocketsizefund-local"):
    cluster_exists = await check_cluster_exists(cluster_name)
    if not cluster_exists:
        await create_cluster(cluster_name)
        await configure_nodes(cluster_name)
    await create_secrets()
    await create_namespaces("live")
    await install_knative()
    await install_prometheus()
    await install_grafana()
    await install_loki()
    await launch_grafana()
