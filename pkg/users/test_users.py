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
                state='state',
                accepted_invite=False,
                access_token='access_token',
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
                        'state': {
                            'S': 'state',
                        },
                        'accepted_invite': {
                            'BOOL': False,
                        },
                        'access_token': {
                            'S': 'access_token',
                        },
                    },
                ]
            }
        )

        users_list = client.list_users()

        self.assertEqual(1, len(users_list))
        self.assertEqual('id', users_list[0].id)
        self.assertEqual('state', users_list[0].state)
        self.assertEqual(False, users_list[0].accepted_invite)
        self.assertEqual(
            'access_token',
            users_list[0].access_token,
        )


class TestSetUserAcceptedInviteAndAccessToken(unittest.TestCase):
    def test_set_user_accepted_invite_and_access_token_success(self):
        client = users.Client(
            dynamodb_table_name='dynamodb_table_name',
        )

        client.dynamodb_client = MockDynamoDBClient(
            users={},
        )

        client.set_user_accepted_invite_and_access_token(
            id='id',
            access_token='access_token',
        )
