from invoke import task
from kubernetes import client, config
from kubernetes.client.rest import ApiException
import subprocess
from rich.progress import Progress, SpinnerColumn, TextColumn
import asyncio
from pyhelm.chartbuilder import ChartBuilder
from pyhelm.tiller import Tiller
from rich.console import Console


console = Console()

@task
def create(c):
    console.print("[blue]Adding Observability Stack...[/blue]")

    config.load_kube_config()
    current_context = config.list_kube_config_contexts()[1]
    api_server = current_context['context']['cluster']
    cluster_info = config.list_kube_config_contexts()[1]['context']
    host = cluster_info['cluster']

    subprocess.run(["helm", "repo", "add", "prometheus-community", "https://prometheus-community.github.io/helm-charts"], check=True)
    subprocess.run(["helm", "repo", "add", "grafana", "https://grafana.github.io/helm-charts"], check=True)
    subprocess.run(["helm", "repo", "update"], check=True)

    tiller = Tiller(host=host)
    prometheus_chart = ChartBuilder({"name": "prometheus", "source": {"type": "repo", "location": "https://prometheus-community.github.io/helm-charts"}})
    tiller.install_release(prometheus_chart.get_helm_chart(), dry_run=False, namespace="monitoring", name="prometheus")

    loki_chart = ChartBuilder({"name": "loki-stack", "source": {"type": "repo", "location": "https://grafana.github.io/helm-charts"}})
    tiller.install_release(loki_chart.get_helm_chart(), dry_run=False, namespace="monitoring", name="loki")

    print("Exposing Grafana...")
    print("Grafana is now accessible at http://localhost:3000")
    print("Default credentials: admin / prom-operator")

    # subprocess.Popen(["kubectl", "port-forward", "-n", "monitoring", "service/prometheus-grafana", "3000:80"])
