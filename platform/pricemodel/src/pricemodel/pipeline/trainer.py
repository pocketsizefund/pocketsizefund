from types import SimpleNamespace
from tinygrad import nn, Tensor, TinyJit, GlobalCounters
from rich.progress import (
    Progress,
    TextColumn,
    BarColumn,
    TimeRemainingColumn,
    TimeElapsedColumn
)
from rich.console import Group
from rich.live import Live
import mlflow
from mlflow import MlflowClient
from tempfile import TemporaryDirectory
from pathlib import Path



class TrainLossColumn(TextColumn):
    def render(self, task):
        loss = task.fields.get("loss", None)
        if loss is not None:
            return rf"loss\[train]={loss:.4f}"
        return "loss[train]=NA"


class ValLossColumn(TextColumn):
    def render(self, task):
        loss = task.fields.get("val_loss", None)
        if loss is not None:
            return rf"loss\[ val ]={loss:.4f}"
        return "loss[val]=NA"

class StatusColumn(TextColumn):
    def render(self, task):
        run_status = task.fields.get("run_status", None)
        if run_status is not None:
            return f"{run_status}"
        return "NA"

class Trainer:
    # TODO: hp tuning
    def __init__(self,
                 *,
                 experiment,
                 model,
                 hyperparameters,
                 optimizer,
                 train,
                 validation,
                 max_epochs: int,
                 batch_size: int,
                 ):
        self.experiment = experiment
        self.hyperparameters = hyperparameters
        self.model = model(**self.hyperparameters)
        self.optimizer = optimizer(nn.state.get_parameters(self.model))
        self.train = train
        self.validation = validation
        self.max_epochs = max_epochs
        self.batch_size = batch_size
        self.train_steps = self.train[0].shape[0]
        self.steps_per_epoch = self.train_steps // self.batch_size

    def save_checkpoint(self, flow, epoch):
        state_dict = nn.state.get_state_dict(self.model)
        with TemporaryDirectory() as tmp:
            path = Path(tmp, f"tft={epoch}.safetensors")
            nn.state.safe_save(state_dict, path)
            flow.log_artifact(path)


    def batch(self, dataset, samples=None):
        return dataset[0][samples], dataset[1][samples]

    @TinyJit
    @Tensor.train()
    def step(self):
        samples = Tensor.randint(self.batch_size, high=self.train_steps)
        x, y = self.batch(self.train, samples)
        self.optimizer.zero_grad()
        prediction = self.model(x)
        loss = prediction.sub(y).square().mean().sqrt()
        loss.backward()
        self.optimizer.step()
        return loss

    @TinyJit
    @Tensor.test()
    def val_loss(self) -> Tensor:
        return self.model(self.validation[0]).sub(self.validation[1]).square().mean().sqrt()


    def __call__(self):
        mlflow.log_param("viewers", 1271)
        mlflow.log_param("followers", 487)

        mlflow.log_params(self.hyperparameters)

        # TODO: turn our Dataset class into an mlflow.Dataset class
        # mlflow.log_input(self.train, "training")
        # mlflow.log_input(self.validation, "validation")


        current_run = mlflow.active_run().info
# <RunInfo: artifact_uri='mlflow-artifacts:/0/2a39335b9a8b4b7a92f2b6aeec07ea8d/artifacts',
# end_time=None,
# experiment_id='0',
# lifecycle_stage='active',
# run_id='2a39335b9a8b4b7a92f2b6aeec07ea8d',
# start_time=1727284585291,
# status='RUNNING',

        header = Progress(
            TextColumn("status"),
            TextColumn("user"),
            TextColumn("run id"),
            TextColumn("run name"),
            TextColumn("experiment"),
        )

        run_info_progress = Progress(
            StatusColumn("status="),
            TextColumn(f"{current_run.user_id}"),
            TextColumn(current_run.run_name),
            TextColumn(f"[bold white]{current_run.run_id}"),
            TextColumn(current_run.experiment_id),
            # BarColumn(),
            # TimeElapsedColumn(),
            # "<",
            # TimeRemainingColumn(),

        )

        step_progress = Progress(
            TextColumn("[bold green]step {task.completed}/{task.total}"),
            BarColumn(),
            TimeElapsedColumn(),
            "<",
            TimeRemainingColumn(),
            TrainLossColumn("loss[train]="),
            refresh_per_second=20,
        )


        epoch_progress = Progress(
            TextColumn("[bold blue]epoch {task.completed}/{task.total}"),
            BarColumn(),
            TimeElapsedColumn(),
            "<",
            TimeRemainingColumn(),
            ValLossColumn("loss[ val ]="),
            refresh_per_second=1,
        )

        progress_group = Group(header,
                               run_info_progress,
                               epoch_progress,
                               step_progress)

        with Live(progress_group, refresh_per_second=20):

            header.add_task("Header")
            run_info_task = run_info_progress.add_task("run status",
                    run_status="[bold yellow]training")


            epoch_task = epoch_progress.add_task("Epochs",
                                            total=self.max_epochs)
            for epoch in range(1, self.max_epochs + 1):
                GlobalCounters.reset()
                step_task = step_progress.add_task(
                    f"Epoch {epoch} Steps",
                    total=self.steps_per_epoch
                )

                for _ in range(self.steps_per_epoch):
                    GlobalCounters.reset()
                    loss = self.step()
                    loss_value = loss.numpy().item()
                    mlflow.log_metric("rmse.loss.train", loss_value)
                    step_progress.update(
                        step_task,
                        advance=1,
                        loss=loss_value)
                if epoch != self.max_epochs:
                    step_progress.remove_task(step_task)

                self.save_checkpoint(mlflow, epoch)
                validation_loss = self.val_loss().numpy().item()
                mlflow.log_metric("rmse.loss.validation", validation_loss)
                epoch_progress.update(epoch_task,
                                      advance=1,
                                      val_loss=validation_loss
                                      )
            run_info_progress.update(
                    run_info_task,
                    run_status="[bold green]finished"
            )
