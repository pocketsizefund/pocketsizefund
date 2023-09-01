import json
import sys

import boto3


environment = sys.argv[1]
table_name = sys.argv[2]

dynamodb_client = boto3.client('dynamodb')

response = dynamodb_client.scan(
    TableName=table_name,
)

json_file = open('{}_users_table.json'.format(environment), 'w')

json.dump(response['Items'], json_file)
