import unittest

from pkg.users import users


class MockDynamoDBClient:
    def __init__(
        self,
        users: dict[str, any],
    ) -> None:
        self.users = users

    def put_item(
        self,
        **kwargs: any,
    ) -> None:
        pass

    def scan(
        self,
        **kwargs: any,
    ) -> any:
        return self.users

    def update_item(
        self,
        **kwargs: any,
    ) -> None:
        pass


class TestAddUser(unittest.TestCase):
    def test_add_user_success(self):
        client = users.Client(
            dynamodb_table_name='dynamodb_table_name',
        )

        client.dynamodb_client = MockDynamoDBClient(
            users={},
        )

        client.add_user(
            user=users.User(
                id='id',
                invite_url='invite_url',
                accepted_invite=False,
                authorization_token='authorization_token',
            ),
        )


class TestListUsers(unittest.TestCase):
    def test_list_users_success(self):
        client = users.Client(
            dynamodb_table_name='dynamodb_table_name',
        )

        client.dynamodb_client = MockDynamoDBClient(
            users={
                'Items': [
                    {
                        'id': {
                            'S': 'id',
                        },
                        'invite_url': {
                            'S': 'invite_url',
                        },
                        'accepted_invite': {
                            'BOOL': False,
                        },
                        'authorization_token': {
                            'S': 'authorization_token',
                        },
                    },
                ]
            }
        )

        users_list = client.list_users()

        self.assertEqual(1, len(users_list))
        self.assertEqual('id', users_list[0].id)
        self.assertEqual('invite_url', users_list[0].invite_url)
        self.assertEqual(False, users_list[0].accepted_invite)
        self.assertEqual(
            'authorization_token',
            users_list[0].authorization_token,
        )


class TestSetUserAcceptedInviteAndAuthorizationToken(unittest.TestCase):
    def test_set_user_accepted_invite_and_authorization_token_success(self):
        client = users.Client(
            dynamodb_table_name='dynamodb_table_name',
        )

        client.dynamodb_client = MockDynamoDBClient(
            users={},
        )

        client.set_user_accepted_invite_and_authorization_token(
            id='id',
            authorization_token='authorization_token',
        )
