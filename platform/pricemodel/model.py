from typing import Dict, List, Optional, Tuple, Union
from embedding import MultiEmbedding
from variable_selection_network import VariableSelectionNetwork
from gated_residual_network import GatedResidualNetwork, GatedLinearUnit, AddNorm, GateAddNorm
from lstm import LSTM
from attention import InterpretableMultiHeadAttention
from tinygrad.nn import Linear
from tinygrad.nn.state import safe_save, safe_load, get_state_dict, load_state_dict


class TemporalFusionTransformer:
    def __init__(
        self,
        hidden_size: int = 16,
        lstm_layers: int = 1,  # NOTE: rename
        dropout_rate: float = 0.1,
        output_size: Union[int, List[int]] = 7,
        #     loss: MultiHorizonMetric = QuantileLoss(),
        attention_head_size: int = 4,
        #     max_encoder_length: int = 10,
        static_categoricals: Optional[List[str]] = [],
        static_reals: Optional[List[str]] = [],
        time_varying_categoricals_encoder: Optional[List[str]] = [],
        time_varying_categoricals_decoder: Optional[List[str]] = [],
        categorical_groups: Optional[Union[Dict, List[str]]] = {},
        time_varying_reals_encoder: Optional[List[str]] = [],
        time_varying_reals_decoder: Optional[List[str]] = [],
        #     x_reals: Optional[List[str]] = [],
        x_categoricals: Optional[List[str]] = [],
        hidden_continuous_size: int = 8,
        hidden_continuous_sizes: Optional[Dict[str, int]] = {},
        embedding_sizes: Optional[Dict[str, Tuple[int, int]]] = {},
        #     embedding_paddings: Optional[List[str]] = [],
        #     embedding_labels: Optional[Dict[str, np.ndarray]] = {},
        #     learning_rate: float = 1e-3,
        #     log_interval: Union[int, float] = -1,
        #     log_val_interval: Union[int, float] = None,
        #     log_gradient_flow: bool = False,
        #     reduce_on_plateau_patience: int = 1000,
        #     monotone_constaints: Optional[Dict[str, int]] = {},
        share_single_variable_networks: bool = False,
        #     causal_attention: bool = True,
        #     logging_metrics: nn.ModuleList = [],
        #     **kwargs,
        reals: List[str] = [],  # NOTE: potentially taken from input data
        target_count: int = 1,  # NOTE: potentially taken from input data
    ) -> None:
        self.reals = reals
        self.target_count = target_count

        self.input_embeddings = MultiEmbedding(
            embedding_sizes=embedding_sizes,
            x_categoricals=x_categoricals,
            categorical_groups=categorical_groups,
        )

        self.prescalers = {
            name: Linear(1, hidden_continuous_sizes.get(name, hidden_continuous_size))
            for name in self.reals
        }

        static_input_sizes = {
            name: self.input_embeddings.output_size[name] for name in static_categoricals
        }

        static_input_sizes.update(
            {
                name: hidden_continuous_sizes.get(name, hidden_continuous_size)
                for name in static_reals
            }
        )

        self.static_variable_selection = VariableSelectionNetwork(
            input_sizes=static_input_sizes,
            hidden_size=hidden_size,
            input_embedding_flags={name: True for name in static_categoricals},
            dropout_rate=dropout_rate,
            prescalers=self.prescalers,
        )

        encoder_input_sizes = {
            name: self.input_embeddings.output_size[name]
            for name in time_varying_categoricals_encoder
        }

        encoder_input_sizes.update(
            {
                name: hidden_continuous_sizes.get(name, hidden_continuous_size)
                for name in time_varying_reals_encoder
            }
        )

        decoder_input_sizes = {
            name: self.input_embeddings.output_size[name]
            for name in time_varying_categoricals_decoder
        }

        decoder_input_sizes.update(
            {
                name: hidden_continuous_sizes.get(name, hidden_continuous_size)
                for name in time_varying_reals_decoder
            }
        )

        if share_single_variable_networks:
            self.shared_single_variable_grns = {}
            for name, input_size in encoder_input_sizes.items():
                self.shared_single_variable_grns[name] = GatedResidualNetwork(
                    input_size=input_size,
                    hidden_size=min(input_size, hidden_size),
                    output_size=hidden_size,
                    dropout_rate=dropout_rate,
                )
            for name, input_size in decoder_input_sizes.items():
                if name not in self.shared_single_variable_grns:
                    self.shared_single_variable_grns[name] = GatedResidualNetwork(
                        input_size=input_size,
                        hidden_size=min(input_size, hidden_size),
                        output_size=hidden_size,
                        dropout_rate=dropout_rate,
                    )

        self.encoder_variable_selection = VariableSelectionNetwork(
            input_sizes=encoder_input_sizes,
            hidden_size=self.hparams.hidden_size,
            input_embedding_flags={name: True for name in time_varying_categoricals_encoder},
            dropout_rate=dropout_rate,
            context_size=hidden_size,
            prescalers=self.prescalers,
            single_variable_grns=(
                {} if not share_single_variable_networks else self.shared_single_variable_grns
            ),
        )

        self.decoder_variable_selection = VariableSelectionNetwork(
            input_sizes=decoder_input_sizes,
            hidden_size=hidden_size,
            input_embedding_flags={name: True for name in time_varying_categoricals_decoder},
            dropout_rate=dropout_rate,
            context_size=hidden_size,
            prescalers=self.prescalers,
            single_variable_grns=(
                {} if not share_single_variable_networks else self.shared_single_variable_grns
            ),
        )

        self.static_context_variable_selection = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        self.static_context_initial_hidden_lstm = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        self.static_context_initial_cell_lstm = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        self.static_context_enrichment = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        self.lstm_encoder = LSTM(
            input_size=hidden_size,
            hidden_size=hidden_size,
            num_layers=lstm_layers,
            dropout_rate=dropout_rate if lstm_layers > 1 else 0,
            batch_first=True,
        )

        self.lstm_decoder = LSTM(
            input_size=hidden_size,
            hidden_size=hidden_size,
            num_layers=lstm_layers,
            dropout_rate=dropout_rate if lstm_layers > 1 else 0,
            batch_first=True,
        )

        self.post_lstm_gate_encoder = GatedLinearUnit(
            input_size=self.hparams.hidden_size, dropout=self.hparams.dropout
        )

        self.post_lstm_gate_decoder = self.post_lstm_gate_encoder

        self.post_lstm_add_norm_encoder = AddNorm(input_size=hidden_size)
        self.post_lstm_add_norm_decoder = self.post_lstm_add_norm_encoder

        self.static_enrichment = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
            context_size=hidden_size,
        )

        self.multihead_attn = InterpretableMultiHeadAttention(
            heads_count=attention_head_size,
            models_count=hidden_size,
            dropout_rate=dropout_rate,
        )

        self.post_attn_gate_norm = GateAddNorm(
            input_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        self.pos_wise_ff = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        self.pre_output_gate_norm = GateAddNorm(
            input_size=hidden_size,
            dropout_rate=None,
        )

        if self.target_count > 1:
            self.output_layer = [Linear(hidden_size, output_size) for output_size in output_size]
        else:
            self.output_layer = Linear(hidden_size, output_size)

    def train(
        self,
    ) -> None:  # NOTE: NEEDS TYPE
        pass  # TEMP

    def save(self) -> None:
        self.file_name = "temporal_fusion_transformer_model.safetensors"

        model_state = get_state_dict(self)

        safe_save(model_state, self.file_name)

    @staticmethod
    def load(self) -> None:
        model_state = safe_load(self.file_name)

        load_state_dict(self, model_state)

    def predict(
        self,
    ) -> any:  # NOTE: NEEDS TYPE
        pass  # TEMP
