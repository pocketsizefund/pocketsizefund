import click

from cli.helpers import stack as stack_helpers


@click.group()
def cli():
    pass


@cli.command()
@click.option(
    '--action',
    required=True,
    type=click.Choice(['create', 'update', 'delete']),
)
@click.option(
    '--environment',
    default='development',
    type=click.Choice(['development', 'production']),
)
def stack(action: str, environment: str):
    stack_name = 'pocketsizefund-' + environment

    if action == 'create' or action == 'update':
        stack_helpers.create_or_update_stack(
            environment=environment,
            stack_name=stack_name,
        )

    elif action == 'delete':
        stack_helpers.delete_stack(
            stack_name=stack_name,
        )


if __name__ == '__main__':
    cli()
