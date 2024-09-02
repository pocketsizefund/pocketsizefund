from pydantic import BaseModel


class Namespace(BaseModel):
    name: str
    status: str

class Namespaces(BaseModel):
    name: str

    @classmethod
    def get(cls, client):
        return dir([ns.metadata for ns in client.list_namespace().items][0])
