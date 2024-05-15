"""Read and parse a SAM config file."""
import argparse

import toml

<<<<<<< HEAD
=======

>>>>>>> 9e24e11 (ruff - double quotes preferred)
ENVIRONMENT_DEVELOPMENT = "development"


class SAMConfig:
    """Read and parse a SAM config file."""

    def __init__(
        self,
        file_path: str,
        environment: str = ENVIRONMENT_DEVELOPMENT,
    ) -> None:
        """Initialize the SAMConfig object."""
        self.samconfig_file = toml.load(file_path)

        self.parameters: dict[str, str] = {}
        parameters = self.samconfig_file[environment]["deploy"]["parameters"]
        for parameter in parameters["parameter_overrides"]:
            parameter_split = parameter.split("=")
            self.parameters[parameter_split[0]] = parameter_split[1]

    def get_parameter(self, parameter_name: str) -> str:
        """Get a parameter from the SAM config file."""
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
