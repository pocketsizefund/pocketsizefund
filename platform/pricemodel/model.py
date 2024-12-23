from typing import Dict, List, Optional, Tuple, Union
from embedding import MultiEmbedding
from variable_selection_network import VariableSelectionNetwork
from gated_residual_network import GatedResidualNetwork, GatedLinearUnit, AddNorm, GateAddNorm
from lstm import LSTM
from attention import InterpretableMultiHeadAttention
from tinygrad import Tensor
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
        x_reals: Optional[List[str]] = [],
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
        causal_attention: bool = True,
        #     logging_metrics: nn.ModuleList = [],
        #     **kwargs,
        reals: List[str] = [],  # NOTE: potentially taken from input data
        target_count: int = 1,  # NOTE: potentially taken from input data
        static_variables: List[str] = [],  # NOTE: potentially taken from input data
    ) -> None:
        self.hidden_size = hidden_size
        self.reals = reals
        self.x_reals = x_reals
        self.static_variables = static_variables
        self.target_count = target_count
        self.lstm_layers = lstm_layers
        self.causal_attention = causal_attention

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
            layer_count=lstm_layers,
            dropout_rate=dropout_rate if lstm_layers > 1 else 0,
            batch_first=True,
        )

        self.lstm_decoder = LSTM(
            input_size=hidden_size,
            hidden_size=hidden_size,
            layer_count=lstm_layers,
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

    def forward(
        self,
        x: Dict[str, Tensor],
    ) -> Dict[str, Tensor]:
        encoder_lengths = x["encoder_lengths"]
        decoder_lengths = x["decoder_lengths"]
        x_cat = x["encoder_cat"].cat(x["decoder_cat"], dim=1)
        x_cont = x["encoder_cont"].cat(x["decoder_cont"], dim=1)
        timesteps = x_cont.size(1)
        max_encoder_length = int(encoder_lengths.max())
        input_vectors = self.input_embeddings(x_cat)
        input_vectors.update(
            {
                name: x_cont[..., idx].unsqueeze(-1)
                for idx, name in enumerate(self.x_reals)
                if name in self.reals
            }
        )

        if len(self.static_variables) > 0:
            static_embedding = {name: input_vectors[name][:, 0] for name in self.static_variables}
            static_embedding, static_variable_selection = self.static_variable_selection(
                static_embedding
            )
        else:
            static_embedding = Tensor.zeros(
                (x_cont.size(0), self.hidden_size), dtype=self.dtype, device=self.device
            )
            static_variable_selection = Tensor.zeros(
                (x_cont.size(0), 0), dtype=self.dtype, device=self.device
            )

        static_context_variable_selection = self.expand_static_context(
            self.static_context_variable_selection(static_embedding), timesteps
        )

        # NOTE: fix encoder_variables reference
        embeddings_varying_encoder = {
            name: input_vectors[name][:, :max_encoder_length] for name in self.encoder_variables
        }

        embeddings_varying_encoder, encoder_sparse_weights = self.encoder_variable_selection(
            embeddings_varying_encoder,
            static_context_variable_selection[:, :max_encoder_length],
        )

        # NOTE: fix decoder_variables reference
        embeddings_varying_decoder = {
            name: input_vectors[name][:, max_encoder_length:] for name in self.decoder_variables
        }

        embeddings_varying_decoder, decoder_sparse_weights = self.decoder_variable_selection(
            embeddings_varying_decoder,
            static_context_variable_selection[:, max_encoder_length:],
        )

        input_hidden = self.static_context_initial_hidden_lstm(static_embedding).expand(
            self.lstm_layers, -1, -1
        )

        input_cell = self.static_context_initial_cell_lstm(static_embedding).expand(
            self.lstm_layers, -1, -1
        )

        encoder_output, (hidden, cell) = self.lstm_encoder(
            x=embeddings_varying_encoder,
            hidden_state=input_hidden,
            # (input_hidden, input_cell),
            # lengths=encoder_lengths,
            # enforce_sorted=False,
        )

        decoder_output, _ = self.lstm_decoder(
            x=embeddings_varying_decoder,
            hidden_state=hidden,
            # (hidden, cell),
            # lengths=decoder_lengths,
            # enforce_sorted=False,
        )

        lstm_output_encoder = self.post_lstm_gate_encoder(encoder_output)
        lstm_output_encoder = self.post_lstm_add_norm_encoder(
            lstm_output_encoder, embeddings_varying_encoder
        )

        lstm_output_decoder = self.post_lstm_gate_decoder(decoder_output)
        lstm_output_decoder = self.post_lstm_add_norm_decoder(
            lstm_output_decoder, embeddings_varying_decoder
        )

        lstm_output = Tensor.empty(lstm_output_decoder.shape)
        lstm_output = lstm_output.cat([lstm_output_encoder, lstm_output_decoder], dim=1)

        static_context_enrichment = self.static_context_enrichment(static_embedding)
        attn_input = self.static_enrichment(
            lstm_output, self.expand_static_context(static_context_enrichment, timesteps)
        )

        attn_output, attn_output_weights = self.multihead_attn(
            q=attn_input[:, max_encoder_length:],
            k=attn_input,
            v=attn_input,
            mask=self.get_attention_mask(
                encoder_lengths=encoder_lengths,
                decoder_lengths=decoder_lengths,
            ),
        )

        attn_output = self.post_attn_gate_norm(attn_output, attn_input[:, max_encoder_length:])

        output = self.pos_wise_ff(attn_output)

        output = self.pre_output_gate_norm(output, lstm_output[:, max_encoder_length:])
        if self.target_count > 1:
            output = [output_layer(output) for output_layer in self.output_layer]
        else:
            output = self.output_layer(output)

        # return self.to_network_output(
        #     prediction=self.transform_output(output, target_scale=x["target_scale"]),
        #     encoder_attention=attn_output_weights[..., :max_encoder_length],
        #     decoder_attention=attn_output_weights[..., max_encoder_length:],
        #     static_variables=static_variable_selection,
        #     encoder_variables=encoder_sparse_weights,
        #     decoder_variables=decoder_sparse_weights,
        #     decoder_lengths=decoder_lengths,
        #     encoder_lengths=encoder_lengths,
        # )

        pass  # TEMP

    def expand_static_context(
        self,
        context: Tensor,
        timesteps: int,
    ) -> Tensor:
        return context[:, None].expand(-1, timesteps, -1)

    def get_attention_mask(
        self,
        encoder_lengths: Tensor,
        decoder_lengths: Tensor,
    ):
        decoder_length = decoder_lengths.max()
        if self.causal_attention:
            attend_step = Tensor.arange(decoder_length, device=self.device)
            predict_step = Tensor.arange(0, decoder_length, device=self.device)[:, None]
            decoder_mask = (
                (attend_step >= predict_step).unsqueeze(0).expand(encoder_lengths.size(0), -1, -1)
            )
        else:
            decoder_mask = (
                self.create_mask(decoder_length, decoder_lengths)
                .unsqueeze(1)
                .expand(-1, decoder_length, -1)
            )

        encoder_mask = (
            self.create_mask(encoder_lengths.max(), encoder_lengths)
            .unsqueeze(1)
            .expand(-1, decoder_length, -1)
        )

        mask = Tensor.empty(encoder_mask.shape)
        mask = mask.cat(
            (
                encoder_mask,
                decoder_mask,
            ),
            dim=2,
        )
        return mask

    def create_mask(
        self,
        size: int,
        lengths: Tensor,
        inverse: bool = False,
    ) -> Tensor:
        if inverse:
            return Tensor.arange(size, device=lengths.device).unsqueeze(0) < lengths.unsqueeze(-1)
        else:
            return Tensor.arange(size, device=lengths.device).unsqueeze(0) >= lengths.unsqueeze(-1)

    def save(self) -> None:
        self.file_name = "temporal_fusion_transformer_model.safetensors"

        model_state = get_state_dict(self)

        safe_save(model_state, self.file_name)

    @staticmethod
    def load(self) -> None:
        model_state = safe_load(self.file_name)

        load_state_dict(self, model_state)
