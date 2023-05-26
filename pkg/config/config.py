import toml


class SAMConfig:
    def __init__(self, file_path: str) -> None:
        self.samconfig_file = toml.load(file_path)

        self.parameters: dict[str, str] = {}
        parameters = self.samconfig_file['default']['global']['parameters']
        for parameter in parameters['parameter_overrides']:
            parameter_split = parameter.split('=')
            self.parameters[parameter_split[0]] = parameter_split[1]

    def get_parameter(self, parameter_name: str) -> str:
        return self.parameters[parameter_name]
