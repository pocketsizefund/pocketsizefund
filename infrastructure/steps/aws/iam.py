from invoke import task
from rich.console import Console

from steps import CLUSTER_NAME, iam_client

console = Console()


@task
def create(c):
    console.print("[blue]Creating IAM roles...")
    roles_to_create = [
        (f"{CLUSTER_NAME}-cluster-role", "eks.amazonaws.com"),
        (f"{CLUSTER_NAME}-node-role", "ec2.amazonaws.com")
    ]
    for role_name, service in roles_to_create:
        try:
            iam_client.create_role(
                RoleName=role_name,
                AssumeRolePolicyDocument=f'{{"Version": "2012-10-17", "Statement": [{{"Effect": "Allow", "Principal": {{"Service": "{service}"}}, "Action": "sts:AssumeRole"}}]}}'
            )
            console.log(f"[green]Created role: [/green][bold]{role_name}[/bold]")
            attached_policies = iam_client.list_attached_role_policies(RoleName=role_name)['AttachedPolicies']
            console.print(f"Policies attached to {role_name}:")
            for policy in attached_policies:
                console.print(f"  - {policy['PolicyName']}")
        except iam_client.exceptions.EntityAlreadyExistsException:
            console.log(f"[yellow]Role already exists[/yellow]: [bold]{role_name}[/bold]")
        except Exception as e:
            console.log(f"[red]Error creating role [/red][bold]{role_name}[/bold]: [yellow]{str(e)}[/yellow]")

@task
def delete(c):
    console.print("[blue]Destroying IAM roles...[/blue]")
    roles_to_delete = [f"{CLUSTER_NAME}-cluster-role", f"{CLUSTER_NAME}-node-role"]
    for role_name in roles_to_delete:
        try:
            attached_policies = iam_client.list_attached_role_policies(RoleName=role_name)['AttachedPolicies']
            for policy in attached_policies:
                iam_client.detach_role_policy(RoleName=role_name, PolicyArn=policy['PolicyArn'])

            iam_client.delete_role(RoleName=role_name)
            console.print(f"[green]Destroyed role: [/green][bold]{role_name}[/bold]")
        except iam_client.exceptions.NoSuchEntityException:
            console.print(f"[yellow]Role not found: [/yellow][bold]{role_name}[/bold]")

