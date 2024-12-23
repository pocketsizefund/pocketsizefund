from typing import Dict, List, Optional, Tuple
from tinygrad import Tensor


class MultiEmbedding:
    def __init__(
        self,
        embedding_sizes: Optional[Dict[str, Tuple[int, int]]],
        x_categoricals: List[str] = None,
        categorical_groups: Optional[Dict[str, List[str]]] = {},
    ) -> None:
        self.embedding_sizes = {
            name: (size, get_embedding_size(size)) if isinstance(size, int) else size
            for name, size in embedding_sizes.items()
        }

        self.categorical_groups = categorical_groups
        self.x_categoricals = x_categoricals

        self.embeddings = {}
        for name in self.embedding_sizes.keys():
            embedding_size = self.embedding_sizes[name][1]
            if self.maximum_embedding_size is not None:
                embedding_size = min(embedding_size, self.maximum_embedding_size)

            self.embedding_sizes[name] = list(self.embedding_sizes[name])
            self.embedding_sizes[name][1] = embedding_size

            if name in self.categorical_groups:
                self.embeddings[name] = TimeDistributedEmbeddingBag(
                    self.embedding_sizes[name][0], embedding_size, mode="sum", batch_first=True
                )
            else:
                self.embeddings[name] = MultiEmbedding(
                    self.embedding_sizes[name][0],
                    embedding_size,
                )

    def forward(
        self,
        x: Tensor,
    ) -> Dict[str, Tensor]:
        input_vectors = {}

        for name, embedding in self.embeddings.items():
            if name in self.categorical_groups:
                input_vectors[name] = embedding(
                    x[
                        ...,
                        [
                            self.x_categoricals.index(categorical_name)
                            for categorical_name in self.categorical_groups[name]
                        ],
                    ]
                )

            else:
                input_vectors[name] = embedding(x[..., self.x_categoricals.index(name)])

        return input_vectors


def get_embedding_size(class_count: int, max_size: int = 100) -> int:
    if class_count > 2:
        return min(round(1.6 * class_count**0.56), max_size)
    else:
        return 1


class TimeDistributedEmbeddingBag:
    def __init__(
        self,
        feature_count: int,
        embedding_dimension: int,
    ) -> None:
        self.embeddings = Tensor.uniform(feature_count, embedding_dimension)

    def forward(
        self,
        x: Tensor,
    ) -> Tensor:
        _, sequence_length, _ = x.shape
        outputs = []

        for t in range(sequence_length):
            features = x[:, t, :]
            embeddings = features @ self.embeddings
            outputs.append(embeddings)

        return outputs[0].stack(outputs, dim=1) if len(outputs) > 1 else outputs[0]
