import sys

from pkg.config import config
from pkg.users import users
from pkg.id import id
from pkg.invite import invite


manage_users_function_url = sys.argv[1]
users_table = sys.argv[2]

samconfig_file = config.SAMConfig(
    'samconfig.toml',
    config.ENVIRONMENT_DEVELOPMENT,
)

invite_client = invite.Client(
    secret_key=samconfig_file.get_parameter('InviteSecretKey'),
    base_url=manage_users_function_url,
    client_id=samconfig_file.get_parameter('AlpacaOAuthClientID'),
    client_secret=samconfig_file.get_parameter('AlpacaOAuthClientSecret'),
)

users_client = users.Client(
    dynamodb_table_name=users_table,
)

user_id = id.new_id()

state = invite_client.hash_value(
    input=user_id,
)

redirect_url = invite_client.get_redirect_url()

authorize_url = invite_client.get_authorize_url(
    redirect_url=redirect_url,
    state=state,
)

user = users.User(
    id=user_id,
    state=state,
)

users_client.add_user(
    user=user,
)

print(authorize_url)
