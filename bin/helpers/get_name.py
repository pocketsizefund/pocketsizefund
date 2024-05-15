"""Parse name into infrastructure format."""  # noqa: INP001
import sys

original_name = sys.argv[1]
environment = sys.argv[2]

updated_name = ""
if original_name == "createpositions":
    updated_name = "function-create-positions"

elif original_name == "clearpositions":
    updated_name = "function-clear-positions"

elif original_name == "predictprice":
    updated_name = "model-predict-price"

else:
    sys.exit(f'"{original_name}" name not found')

updated_name = f"pocketsizefund-{environment}-{updated_name}"

print(updated_name)  # noqa: T201
