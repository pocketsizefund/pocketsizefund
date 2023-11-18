import json
import os
import sys

import boto3


environment = sys.argv[1]
table_name = sys.argv[2]

file_name = '{}_users_table.json'.format(environment)

if not os.path.exists(file_name):
    exit(code=0)

json_file = open(file_name, 'r')

items = json.load(json_file)

request = [{'PutRequest': {'Item': item}} for item in items]

dynamodb_client = boto3.client('dynamodb')

response = dynamodb_client.batch_write_item(
    RequestItems={
        table_name: request,
    }
)
