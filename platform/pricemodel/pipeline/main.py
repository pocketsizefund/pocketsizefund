from trainer import Trainer
from model import SimpleModel
from data import load_data, process_data, train_val_split
from tinygrad import nn


if __name__ == "__main__":
    data = load_data()
    print(data)
    data = process_data(data)

    print(data)

    train, val = train_val_split(data)

    trainer = Trainer(
        model = SimpleModel,
        hyperparameters = {"input_size": 2, "hidden_size": 1},
        optimizer = nn.optim.Adam,
        max_epochs = 5,
        batch_size = 32,
        train = train,
        validation = val,
        )

    trainer()
