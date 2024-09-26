from pydantic import BaseModel
from mlflow import MlflowClient


# TODO: create a version tag from sha of model
class Experiment(BaseModel):
    name: str
    team: str = "psf-core"
    domain: str
    model_architecture: str
    description: str | None = None


    @property
    def client(self):
        return MlflowClient(tracking_uri="http://localhost:8092")

    def load(self):
        pass

    def create(self):
        self.client.create_experiment(
            self.name,
            tags={"team": self.team, "domain": self.domain, "model_architecture": self.model_architecture}
        )

