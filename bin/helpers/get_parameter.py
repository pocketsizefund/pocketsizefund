import sys

from pkg.config import config


samconfig_file = config.SAMConfig(
    'samconfig.toml',
    config.ENVIRONMENT_DEVELOPMENT,
)

parameter_name = sys.argv[1]

parameter = samconfig_file.get_parameter(parameter_name)

print(parameter)
