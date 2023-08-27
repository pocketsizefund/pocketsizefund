import uuid


def new_id() -> str:
    return str(uuid.uuid4())
