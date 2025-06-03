from tinygrad.tensor import Tensor
from application.predictionengine.src.predictionengine.ticker_embedding import (
    TickerEmbedding,
)


def test_ticker_embedding_initialization() -> None:
    embedding = TickerEmbedding(ticker_count=100, embedding_size=32)

    assert hasattr(embedding, "embedding")


def test_ticker_embedding_forward() -> None:
    embedding = TickerEmbedding(ticker_count=10, embedding_size=16)

    ticker_ids: Tensor = Tensor([1])
    result: Tensor = embedding.forward(ticker_ids)

    assert result.shape == (1, 16)

    ticker_ids = Tensor([1, 2, 3])
    result = embedding.forward(ticker_ids)

    assert result.shape == (3, 16)


def test_ticker_embedding_different_sizes() -> None:
    for embedding_size in [8, 16, 32, 64]:
        embedding = TickerEmbedding(ticker_count=50, embedding_size=embedding_size)
        ticker_ids = Tensor([0, 1, 2])
        result = embedding.forward(ticker_ids)

        assert result.shape == (3, embedding_size)


def test_ticker_embedding_range() -> None:
    embedding = TickerEmbedding(ticker_count=5, embedding_size=8)

    for ticker_id in range(5):
        ticker_ids = Tensor([ticker_id])
        result = embedding.forward(ticker_ids)
        assert result.shape == (1, 8)


def test_ticker_embedding_consistency() -> None:
    embedding = TickerEmbedding(ticker_count=10, embedding_size=3)
    tickers = Tensor([1, 2, 3])

    result = embedding.forward(tickers)

    assert result.shape == (3, 3)

    for _, ticker_id in enumerate([1, 2, 3]):
        individual_result = embedding.forward(Tensor([ticker_id]))
        assert individual_result.shape == (1, 3)
