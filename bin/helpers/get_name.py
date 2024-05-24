"""Parse name into infrastructure format."""

import sys

original_name = sys.argv[1]
environment = sys.argv[2]

updated_name = ""
if original_name == "setpositions":
    updated_name = "function-set-positions"

elif original_name == "predictprice":
    updated_name = "model-predict-price"

else:
    sys.exit(f'"{original_name}" name not found')

updated_name = f"pocketsizefund-{environment}-{updated_name}"

print(updated_name)  # noqa: T201
