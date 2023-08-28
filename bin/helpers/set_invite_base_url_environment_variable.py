import sys

import boto3


environment = sys.argv[1]

stack_name = 'pocketsizefund-{}'.format(environment)
function_name = 'pocketsizefund-{}-complete-invite'.format(environment)

cloudformation_client = boto3.client('cloudformation')

describe_stacks_response = cloudformation_client.describe_stacks(
    StackName=stack_name,
)

stack_outputs = describe_stacks_response['Stacks'][0]['Outputs']

complete_invite_function_url = ''
for output in stack_outputs:
    if output['OutputKey'] == 'CompleteInviteFunctionUrl':
        complete_invite_function_url = output['OutputValue']

lambda_client = boto3.client('lambda')

get_function_configuration_response = lambda_client.get_function_configuration(
    FunctionName=function_name,
)

environment_variables = get_function_configuration_response['Environment']['Variables']

environment_variables['INVITE_BASE_URL'] = complete_invite_function_url

lambda_client.update_function_configuration(
    FunctionName=function_name,
    Environment={
        'Variables': environment_variables,
    },
)
