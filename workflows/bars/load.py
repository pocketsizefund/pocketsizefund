from union import workflow, task


@task
def load_from_polygon(date: datetime.date) -> None:
    return


@workflow
def daily() -> None:
    return load_from_polygon()
