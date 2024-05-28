"""Parse name into infrastructure format."""

import sys

original_name = sys.argv[1]
environment = sys.argv[2]

updated_name = ""
if original_name == "inferenceendpoint":
    updated_name = "model-inference-endpoint"

elif original_name == "positionmanager":
    updated_name = "function-position-manager"

else:
    sys.exit(f'"{original_name}" name not found')

updated_name = f"pocketsizefund-{environment}-{updated_name}"

print(updated_name)  # noqa: T201
