import subprocess

import boto3


cloudformation_client = boto3.client('cloudformation')


def create_or_update_stack(
    environment: str,
    stack_name: str,
) -> None:
    subprocess.run(
        args=[
            'cfn-lint',
            'template.yaml',
        ],
    )

    subprocess.run(
        args=[
            'sam', 'build',
            '--use-container',
            '--cached',
            '--parallel',
        ],
    )

    subprocess.run(
        args=[
            'sam', 'deploy',
            '--no-confirm-changeset',
            '--resolve-image-repos',
            '--on-failure', 'DELETE',
            '--config-env', '{}'.format(environment),
        ],
    )

    print('stack {} created successfully'.format(stack_name))


def delete_stack(
    stack_name: str,
) -> None:
    cloudformation_client.delete_stack(
        StackName=stack_name,
    )

    print('stack {} deleted successfully'.format(stack_name))
