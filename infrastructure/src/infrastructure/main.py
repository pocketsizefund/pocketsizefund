import asyncio
from typer import Typer
from infrastructure.pipeline import pipeline

app = Typer()

@app.command("up")
def up() -> None:
    asyncio.run(pipeline())

@app.command("down")
def down() -> None:
    pass
