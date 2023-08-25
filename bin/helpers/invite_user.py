import sys

from pkg.config import config
from pkg.users import users
from pkg.id import id
from pkg.invite import invite


users_table = sys.argv[1]

samconfig_file = config.SAMConfig(
    'samconfig.toml',
    config.ENVIRONMENT_DEVELOPMENT,
)

invite_client = invite.Client(
    secret_key=samconfig_file.get_parameter('InviteSecretKey'),
    base_url=samconfig_file.get_parameter('InviteBaseURL'),
    client_id=samconfig_file.get_parameter('AlpacaOAuthClientID'),
    client_secret=samconfig_file.get_parameter('AlpacaOAuthClientSecret'),
)

users_client = users.Client(
    dynamodb_table_name=users_table,
)

user_id = id.generate_id()

invite_code = invite_client.generate_invite_code(
    user_id=user_id,
)

invite_url = invite_client.generate_invite_url(
    invite_code=invite_code,
)

user = users.User(
    id=user_id,
    invite_code=invite_code,
)

users_client.add_user(
    user=user,
)

print(invite_url)
