import glob
from anthropic import Anthropic
from instructor import from_anthropic, Partial
from pydantic import BaseModel
from typing import List, Dict
from pathlib import Path
import pathspec
import ell
from rich.console import Console
from pydantic import BaseModel
import datetime


client = from_anthropic(Anthropic())


class FileResponse(BaseModel):
    files: list[str]


class FilePicker:
    name = "file_picker"
    description = "pick relevant files from a directory"

    def __init__(self, directory, blacklist: list[str]):
        self.directory = directory
        self.blacklist = blacklist

    @property
    def files(self):
        directory = str(Path(self.directory).resolve() / "**")

        spec = pathspec.PathSpec.from_lines("gitwildmatch", self.blacklist)
        files = []
        for file in glob.glob(directory, recursive=True):
            if Path(file).is_file() and not spec.match_file(file):
                files.append(file)
        return files

    def chat(self, query: str):
        return client.chat.completions.create(
            model="claude-3-5-sonnet-20240620",
            max_tokens=1024 * 8,
            messages=[
                {
                    "role": "system",
                    "content": f"You are an expert in terraform, and you are given a directory of files and a user query. Decide which files are best suited to answering the question.<Files>{self.files}</Files>",
                },
                {"role": "user", "content": query},
            ],
            response_model=FileResponse,
        ).files

    def load_files(self, query):
        files = self.chat(query)
        contents = []
        for file in files:
            with open(file, "r") as f:
                content = f.read()
            contents += [{"file_name": file, "contents": content}]

        return contents

    def load_all_files(self):
        contents = []
        for file in self.files:
            with open(file, "r") as f:
                content = f.read()
            contents += [{"file_name": file, "contents": content}]
        return contents


def chat_over_files(files, query):
    return client.chat.completions.create(
        model="claude-3-5-sonnet-20240620",
        max_tokens=1024 * 8,
        messages=[
            {
                "role": "system",
                "content": f"You are an expert in terraform, and you are given files and a user query. Answer the user's question with full code examples, showing diffs between the current code and the solution.",
            },
            {"role": "user", "content": query},
        ],
        response_model=str,
    )


if __name__ == "__main__":
    import sys

    console = Console()
    console.clear()

    file_picker = FilePicker(directory=".", blacklist=[".terraform/", "*.tfstate*", "chat.py"])

    file_context = []
    memory_buffer = []

    while True:
        user_input = input("> ")
        if user_input.lower() == "exit":
            break

        if user_input.lower() == "reset":
            file_context = []
            memory_buffer = []
            continue

        if user_input.lower() == "save":
            file_suffix = datetime.datetime.now(datetime.UTC).timestamp() * 100
            with open(f"chat-{file_suffix}.log", "w+") as f:
                for memory in memory_buffer:
                    f.write(f"{memory[0]}\n{memory[1]}\n\n")

        if sys.argv[1] == "all":
            file_context += file_picker.load_all_files()
        else:
            file_context += file_picker.load_files(user_input)

        response = chat_over_files(file_context, user_input)

        memory_buffer.append((user_input, response))

        console.print(response)
