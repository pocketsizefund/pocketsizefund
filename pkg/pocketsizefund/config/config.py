"""Hold all project-level configuration."""

import argparse
import datetime

import toml

ENVIRONMENT_DEVELOPMENT = "development"
TIMEZONE = datetime.timezone.utc  # noqa: UP017


class SAMConfig:
    """SAMConfig class."""

    def __init__(self, file_path: str, environment: str = ENVIRONMENT_DEVELOPMENT) -> None:
        """SAM Config class.

        Args:
            file_path (str): Path to SAM config file.
            environment (str, optional): Environment to use. Defaults to ENVIRONMENT_DEVELOPMENT.
        """
        self.samconfig_file = toml.load(file_path)

        self.parameters: dict[str, str] = {}
        parameters = self.samconfig_file[environment]["deploy"]["parameters"]
        for parameter in parameters["parameter_overrides"]:
            parameter_split = parameter.split("=")
            self.parameters[parameter_split[0]] = parameter_split[1]

    def get_parameter(self, parameter_name: str) -> str:
        """Get a parameter from the SAM config file.

        Args:
            parameter_name (str): Parameter name.
        """
        return self.parameters[parameter_name]


if __name__ == "__main__":
    samconfig_file = SAMConfig(
        "samconfig.toml",
        ENVIRONMENT_DEVELOPMENT,
    )

    parser = argparse.ArgumentParser(description="Process a config file.")
    parser.add_argument("--parameter", type=str, help="The parameter to retrieve.")

    arguments = parser.parse_args()

    print(samconfig_file.get_parameter(arguments.parameter))  # noqa: T201
