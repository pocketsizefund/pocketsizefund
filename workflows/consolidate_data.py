from union import workflow, task
import s3fs


@task
def list_available_data(
    *, bucket: str, prefix: str = "", file_extension: str = "parquet"
) -> list[str]:
    fs = s3fs.S3FileSystem(anon=False)
    return [
        f"s3://{path}"
        for path in fs.find(f"{bucket}/{prefix}", maxdepth=None)
        if path.endswth(file_extension)
    ]


@workflow
def shape_bars_data(bucket: str) -> list[str]:
    return list_available_data(bucket=bucket)
