import polars as pl
from tinygrad import nn, Tensor, dtypes



def load_data():
    return pl.read_csv("consolidated_data.csv").select(["open_price", "close_price"])


def process_data(data):
    return Tensor(data.to_numpy(), dtype=dtypes.float)


def train_val_split(data):
    return data, data
