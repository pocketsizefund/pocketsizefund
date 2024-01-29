import configparser
from os import path
import json

from pkg.config import config


samconfig_file = config.SAMConfig(
    'samconfig.toml',
    config.ENVIRONMENT_DEVELOPMENT,
)

output = {
    'alpaca_api_key': samconfig_file.get_parameter('AlpacaAPIKey'),
    'alpaca_secret_key': samconfig_file.get_parameter('AlpacaAPISecret'),
}

credentials_file_path = path.expanduser('~/.aws/credentials')

config = configparser.ConfigParser()
config.read(credentials_file_path)

output['aws_access_key_id'] = config.get('default', 'aws_access_key_id')
output['aws_secret_access_key'] = config.get(
    'default',
    'aws_secret_access_key',
)

json_output = json.dumps(output)

print(json_output)
