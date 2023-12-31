import os
from urllib import parse

from pkg.invite import invite
from pkg.users import users


METHOD_GET = 'GET'


invite_client = invite.Client(
    secret_key=os.getenv('INVITE_SECRET_KEY'),
    base_url=os.getenv('INVITE_BASE_URL'),
    client_id=os.getenv('ALPACA_OAUTH_CLIENT_ID'),
    client_secret=os.getenv('ALPACA_OAUTH_CLIENT_SECRET'),
)

users_client = users.Client(
    dynamodb_table_name=os.getenv('USERS_TABLE_NAME'),
)


def handler(event: any, context: any) -> dict[str, any]:
    method = event['requestContext']['http']['method']
    if method != METHOD_GET:
        return {
            'statusCode': 400,
            'body': {
                'message': 'method "{}" not allowed - expected "{}"'.format(method, METHOD_GET),
            },
        }

    path = event['rawPath']
    if path == '/complete_invite':
        query_parameters = parse.parse_qs(event['rawQueryString'])

        # both added to redirect URL by Alpaca OAuth flow
        code = query_parameters['code'][0]
        state = query_parameters['state'][0]

        users = users_client.list_users()
        for user in users:
            if user.state == state:
                redirect_url = invite_client.get_redirect_url()

                access_token = invite_client.get_access_token(
                    code=code,
                    redirect_url=redirect_url,
                )

                users_client.set_user_accepted_invite_and_access_token(
                    user_id=user.user_id,
                    accepted_invite=True,
                    access_token=access_token,
                )

                return {
                    'statusCode': 200,
                    'body': {
                        'message': 'invite accepted',
                    },
                }
