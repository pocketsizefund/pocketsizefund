
from invoke import task
from rich.console import Console
from steps.k8s import knative, observability

console = Console()

@task
def create(c):
    console.print("[blue]Creating k8s platform...[/blue]")
    knative.create(c)
    observability.create(c)
    console.print("[blue]Setup complete![/blue]")
