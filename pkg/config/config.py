"""Config module for SAM (Serverless Application Model)"""

import datetime

import toml

ENVIRONMENT_DEVELOPMENT = "development"
TIMEZONE = datetime.timezone.utc


class SAMConfig:
    """SAMConfig class."""

    def __init__(self, file_path: str, environment: str = ENVIRONMENT_DEVELOPMENT) -> None:
        """
        SAM Config class.

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
        """
        Get parameter value.

        Args:
            parameter_name (str): Parameter name.
        """
        return self.parameters[parameter_name]
