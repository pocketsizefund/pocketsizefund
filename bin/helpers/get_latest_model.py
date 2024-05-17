import boto3

s3_client = boto3.client("s3")

file_names: list[str] = []

continuation_token: str = None
while True:
    list_arguments = {
        "Bucket": "pocketsizefund-artifacts",
        "Prefix": "models",
    }

    if continuation_token:
        list_arguments["ContinuationToken"] = continuation_token

    response = s3_client.list_objects_v2(**list_arguments)

    if response["KeyCount"] == 0:
        break

    for content in response["Contents"]:
        key = content["Key"]
        if "model.tar.gz" in key:
            file_names.append(key)

    if not response["IsTruncated"]:
        break

file_names.sort(reverse=True)

print(file_names[0])
