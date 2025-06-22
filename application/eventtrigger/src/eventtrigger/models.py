from pydantic import BaseModel


class TriggerEventRequest(BaseModel):
    event: str
