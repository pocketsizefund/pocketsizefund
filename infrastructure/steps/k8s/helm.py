from invoke import task
from pyhelm.repo import from_repo
from pyhelm.chartbuilder import ChartBuilder
from dataclasses import dataclass

@dataclass
class Chart:
    """Helm chart."""
    chart: str
    version: str
    release: str
    namespace: str
    values: dict

    @classmethod
    def from_repo(cls, repo: str, name: str):
        from_repo(repo, name)


@task
def install(c, repo, name):
    """Install a helm chart."""
    chart = Chart.from_repo(repo, name)


@task
def upgrade(c, chart: Chart):
    """Upgrade a helm chart."""
    c.run(f"helm upgrade {chart.release} {chart.chart} --namespace {chart.namespace} --values {chart.values}")
