import sys


original_name = sys.argv[1]
environment = sys.argv[2]

updated_name = ""
if original_name == "fetchdata":
    updated_name = "function-fetch-data"

elif original_name == "createpositions":
    updated_name = "function-create-positions"

elif original_name == "clearpositions":
    updated_name = "function-clear-positions"

elif original_name == "schedulepositions":
    updated_name = "function-schedule-positions"

elif original_name == "completeinvite":
    updated_name = "function-complete-invite"

elif original_name == "postperformance":
    updated_name = "function-post-performance"

elif original_name == "trainlstm":
    updated_name = "model-train-lstm"

elif original_name == "predictlstm":
    updated_name = "model-predict-lstm"

elif original_name == "evaluatelstm":
    updated_name = "model-evaluate-lstm"

else:
    exit(f'"{original_name}" name not found')

updated_name = f"pocketsizefund-{environment}-{updated_name}"

print(updated_name)
