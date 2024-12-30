from tinygrad import Tensor
from tinygrad.nn import Linear
from tinygrad.dtype import DType, dtypes
from typing import Tuple, List


class InterpretableMultiHeadAttention:
    def __init__(
        self,
        heads_count: int,
        models_count: int,
        dropout_rate: float = 0.0,
    ) -> None:
        self.heads_count = heads_count
        self.models_count = models_count
        self.dropout_rate = dropout_rate

        self.keys_dimensions = self.queries_dimensions = self.values_dimensions = (
            self.models_count // self.heads_count
        )

        self.values_layer = Linear(self.models_count, self.values_dimensions)
        self.keys_layers = [
            Linear(self.models_count, self.keys_dimensions) for _ in range(self.heads_count)
        ]
        self.queries_layers = [
            Linear(self.models_count, self.queries_dimensions) for _ in range(self.heads_count)
        ]

        self.attention = ScaledDotProductAttention()

        self.head_weight = Linear(
            self.values_dimensions,
            self.models_count,
            bias=False,
        )

    def forward(
        self,
        q: Tensor,
        k: Tensor,
        v: Tensor,
        mask: Tensor = None,
    ) -> Tuple[Tensor, Tensor]:
        heads: List[Tensor] = []
        attentions: List[Tensor] = []

        vs = self.values_layer(v)

        for i in range(self.heads_count):
            qs = self.queries_layers[i](q)
            ks = self.keys_layers[i](k)

            head, attention = self.attention.forward(qs, ks, vs, mask)

            head_droput = head.dropout(self.dropout_rate)
            heads.append(head_droput)

            attentions.append(attention)

        head = heads[0].stack(*heads[1:], dim=2) if len(heads) > 1 else heads[0]
        attention = (
            attentions[0].stack(*attentions[1:], dim=2) if len(attentions) > 1 else attentions[0]
        )

        outputs = head.mean(axis=2) if len(heads) > 1 else head

        outputs = self.head_weight(outputs)
        outputs = outputs.dropout(self.dropout_rate)

        return outputs, attention


class ScaledDotProductAttention:
    def forward(
        self,
        q: Tensor,
        k: Tensor,
        v: Tensor,
        mask: Tensor = None,
    ) -> Tuple[Tensor, Tensor]:
        attention = bmm(q, k.permute(0, 2, 1))

        dimension = Tensor(k.size(-1), dtype=attention.dtype, device=attention.device).sqrt()
        attention = attention / dimension

        if mask is not None:
            attention = masked_fill(attention, mask, -1e9)

        attention = attention.softmax(axis=2)

        output = bmm(attention, v)

        return output, attention


def bmm(
    A: Tensor,
    B: Tensor,
) -> Tensor:
    return Tensor.einsum("bij,bjk->bik", A, B)


def masked_fill(
    tensor: Tensor,
    mask: Tensor,
    value: float,
) -> Tensor:
    mask_tensor = tensor_astype(mask, tensor.dtype)

    return tensor * (1 - mask_tensor) + Tensor(value) * mask_tensor


def tensor_astype(tensor: Tensor, dtype: DType) -> Tensor:
    if dtypes.is_float(dtype):
        return tensor * 1.0
    elif dtypes.is_int(dtype):
        return tensor * 1
    else:
        raise ValueError(f"Unsupported dtype: {dtype}")
