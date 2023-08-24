import boto3


class User:
    def __init__(
        self,
        id: str,
        invite_code: str,
        accepted_invite: bool,
        authorization_token: str,
    ) -> None:
        self.id = id
        self.invite_code = invite_code
        self.accepted_invite = accepted_invite
        self.authorization_token = authorization_token


class Client:
    def __init__(
        self,
        dynamodb_table_name: str,
    ) -> None:
        self.dynamodb_table_name = dynamodb_table_name
        self.dynamodb_client = boto3.client('dynamodb')

    def add_user(
        self,
        user: User,
    ) -> None:
        self.dynamodb_client.put_item(
            TableName=self.dynamodb_table_name,
            Item={
                'id': {
                    'S': user.id,
                },
                'invite_code': {
                    'S': user.invite_code,
                },
                'accepted_invite': {
                    'BOOL': False,
                },
                'authorization_token': {
                    'S': 'NONE',
                },
            },
        )

    def list_users(self) -> list[User]:
        users: list[User] = []

        response = self.dynamodb_client.scan(
            TableName=self.dynamodb_table_name,
        )

        items = response['Items']
        while 'LastEvaluatedKey' in response:
            response = self.dynamodb_client.scan(
                TableName=self.dynamodb_table_name,
                ExclusiveStartKey=response['LastEvaluatedKey'],
            )
            items.extend(response['Items'])

        for item in items:
            users.append(
                User(
                    id=item['id']['S'],
                    invite_code=item['invite_code']['S'],
                    authorization_token=item['authorization_token']['S'],
                    accepted_invite=item['accepted_invite']['BOOL'],
                )
            )

        return users

    def set_user_accepted_invite_and_authorization_token(
        self,
        id: str,
        authorization_token: str,
    ) -> None:
        update_expression = 'SET accepted_invite = :invite_value, authorization_token = :token_value'
        expression_attribute_values = {
            ':invite_value': {
                'BOOL': True,
            },
            ':token_value': {
                'S': authorization_token,
            },
        }

        self.dynamodb_client.update_item(
            TableName=self.dynamodb_table_name,
            Key={
                'id': {
                    'S': id,
                },
            },
            UpdateExpression=update_expression,
            ExpressionAttributeValues=expression_attribute_values
        )
