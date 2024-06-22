"""Hold all project-level configuration."""

import argparse
import datetime

ENVIRONMENT_DEVELOPMENT = "development"
TIMEZONE = datetime.timezone.utc  # noqa: UP017


class Config:
    """Config class."""

    def __init__(self, environment: str = ENVIRONMENT_DEVELOPMENT) -> None:
        """Config class.

        Args:
            environment (str, optional): Environment to use. Defaults to ENVIRONMENT_DEVELOPMENT.
        """
        self.parameters = {}
        with open(f"etc/.env.{environment.lower()}") as config_file:  # noqa: PTH123
            for line in config_file:
                key, value = line.strip().split("=", 1)
                self.parameters[key.strip()] = value.strip()

    def get_parameter(self, parameter_name: str) -> str:
        """Get a parameter from the config file.

        Args:
            parameter_name (str): Parameter name.
        """
        return self.parameters[parameter_name]


if __name__ == "__main__":
    config_file = Config(
        ENVIRONMENT_DEVELOPMENT,
    )

    parser = argparse.ArgumentParser(description="Process a config file.")
    parser.add_argument("--parameter", type=str, help="The parameter to retrieve.")

    arguments = parser.parse_args()

    print(config_file.get_parameter(arguments.parameter))  # noqa: T201
