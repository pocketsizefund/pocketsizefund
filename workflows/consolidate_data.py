from union import workflow, task, Secret, ImageSpec, current_context
import s3fs

s3_image = ImageSpec(
    name="pocketsizefund/s3",
    platform="linux/amd64",
    packages=[
        "polars",
        "pandas",
        "pandera[polars]",
        "flytekitplugins-pandera",
        "s3fs",
    ],
)


@task(
    container_image=s3_image,
    secret_requests=[
        Secret(key="AWS_ACCESS_KEY_ID"),
        Secret(key="AWS_SECRET_ACCESS_KEY"),
        Secret(key="DATA_BUCKET"),
    ],
)
def list_available_data(*, prefix: str = "", file_extension: str = "parquet") -> list[str]:
    AWS_SECRET_ACCESS_KEY = current_context().secrets.get(key="AWS_SECRET_ACCESS_KEY")
    AWS_ACCESS_KEY_ID = current_context().secrets.get(key="AWS_ACCESS_KEY_ID")
    BUCKET = current_context().secrets.get(key="DATA_BUCKET")

    fs = s3fs.S3FileSystem(secret=AWS_SECRET_ACCESS_KEY, key=AWS_ACCESS_KEY_ID)
    return [
        f"s3://{path}"
        for path in fs.find(f"{BUCKET}/{prefix}", maxdepth=None)
        if path.endswith(file_extension)
    ]


@workflow
def shape_bars_data() -> list[str]:
    return list_available_data()
