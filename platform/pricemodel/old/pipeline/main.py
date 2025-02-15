from trainer import Trainer
from model import SimpleModel
from data import load_data, prepare_data, split_data
from tinygrad import nn


if __name__ == "__main__":
    data = load_data("../consolidated_data.csv")
    data, static_features, encoder = prepare_data(data)

    train, val = split_data(data)

    trainer = Trainer(
        model=SimpleModel,
        hyperparameters={"input_size": 6, "hidden_size": 6},
        optimizer=nn.optim.Adam,
        max_epochs=5,
        batch_size=32,
        train=train,
        validation=val,
    )

    trainer()
