from typing import Dict, List, Optional, Tuple, Union
from tinygrad import Tensor


class MultiEmbedding:
    def __init__(
        self,
        embedding_sizes: Optional[Dict[str, Tuple[int, int]]],
        x_categoricals: List[str] = None,
        categorical_groups: Optional[Dict[str, List[str]]] = {},
    ) -> None:
        """
        Embedding layer for categorical variables including groups of categorical variables.

        embedding_sizes values are tuples of variable count (e.g. 10 unique variables) and
        embedding dimensions (e.g. 3).
        x_categoricals is a list of input variable names.
        categorical_groups is a dictionary of groups of categorical variables.
        """
        self.embedding_sizes = {
            name: (size, get_embedding_size(size)) if isinstance(size, int) else size
            for name, size in embedding_sizes.items()
        }

        self.x_categoricals = x_categoricals
        self.categorical_groups = categorical_groups

        self.embeddings = {}
        for name in self.embedding_sizes.keys():
            unique_variable_count = self.embedding_sizes[name][0]
            embedding_size = self.embedding_sizes[name][1]

            if name in self.categorical_groups:
                self.embeddings[name] = TimeDistributedEmbeddingBag(
                    feature_count=unique_variable_count,
                    embedding_dimension=embedding_size,
                )
            else:
                self.embeddings[name] = Embedding(
                    feature_count=unique_variable_count,
                    embedding_dimension=embedding_size,
                )

    @property
    def output_size(self) -> Union[Dict[str, int], int]:
        return {name: s[1] for name, s in self.embedding_sizes.items()}

    def forward(
        self,
        x: Tensor,
    ) -> Dict[str, Tensor]:
        input_vectors: Dict[str, Tensor] = {}

        for name, embedding in self.embeddings.items():
            if name in self.categorical_groups:
                input_vectors[name] = embedding.forward(
                    x[
                        ...,
                        [
                            self.x_categoricals.index(categorical_name)
                            for categorical_name in self.categorical_groups[name]
                        ],
                    ]
                )

            else:
                input_vectors[name] = embedding.forward(x[..., self.x_categoricals.index(name)])

        return input_vectors


# NOTE: this can likely be removed
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
        self.embedding_table = Tensor.uniform(
            (feature_count, embedding_dimension),
            low=-0.1,
            high=0.1,
        )

    def forward(
        self,
        x: Tensor,
    ) -> Tensor:
        if len(x.shape) == 2:
            x = x.unsqueeze(0)  # add batch dimension if 2D

        _, sequence_length, _ = x.shape

        output = Tensor.empty(x.shape[0], self.embedding_table.shape[1])

        for t in range(sequence_length):
            features = x[:, t, :]
            embeddings = features @ self.embedding_table

            output.stack(embeddings)

        return output


class Embedding:
    def __init__(
        self,
        feature_count: int,
        embedding_dimension: int,
    ) -> None:
        self.embedding_table = Tensor.uniform(
            (feature_count, embedding_dimension),
            low=-0.1,
            high=0.1,
        )

    def forward(
        self,
        x: Tensor,
    ) -> Tensor:
        return self.embedding_table[x]
