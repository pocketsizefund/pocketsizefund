from typing import Dict, List, Optional, Tuple, Union, Any
from pricemodel.embedding import MultiEmbedding
from pricemodel.variable_selection_network import VariableSelectionNetwork
from pricemodel.gated_residual_network import (
    GatedResidualNetwork,
    GatedLinearUnit,
    GateAddNorm,
    AddNorm,
)
from pricemodel.metrics import smape, mape, mae, rmse, quantile_loss
from pricemodel.lstm import LSTM
from pricemodel.attention import InterpretableMultiHeadAttention
from pricemodel.dataset import DataSet
from tinygrad.nn import Linear
from tinygrad import Tensor


# TODO: remove in-line comments
# TODO: consolidate duplicate parameters
# TODO: add back required parameters if new features are added (e.g. "day of month")
class TemporalFusionTransformer:
    def __init__(
        self,
        hidden_size: int = 16,
        lstm_layers: int = 1,
        dropout_rate: float = 0.1,
        output_size: Union[
            int, List[int]
        ] = 7,  # number of values to predict (OHLCV + ticker + timestamp) (1 or 5 or 7?)
        attention_head_size: int = 4,
        static_categoricals: Optional[List[str]] = ["ticker"],
        # static_reals: Optional[List[str]] = [],  # TODO: remove if unused
        # time_varying_categoricals_encoder: Optional[List[str]] = [],  # TODO: remove if unused
        # time_varying_categoricals_decoder: Optional[List[str]] = [],  # TODO: remove if unused
        # categorical_groups: Optional[Union[Dict, List[str]]] = {},  # TODO: remove if unused
        time_varying_reals_encoder: Optional[List[str]] = [
            "open",
            "high",
            "low",
            "close",
            "volume",
        ],
        time_varying_reals_decoder: Optional[List[str]] = [
            "open",
            "high",
            "low",
            "close",
            "volume",
        ],
        x_reals: Optional[List[str]] = [
            "open",
            "high",
            "low",
            "close",
            "volume",
        ],
        x_categoricals: Optional[List[str]] = ["ticker"],
        hidden_continuous_size: int = 8,
        hidden_continuous_sizes: Optional[Dict[str, int]] = {},  # TODO: remove if unused
        embedding_sizes: Optional[
            Dict[str, Tuple[int, int]]
        ] = {},  # unique tickers (dynamic from data) w/ embedding size (16)
        # share_single_variable_networks: bool = False, # TODO: remove if unused
        causal_attention: bool = True,
    ):
        self.hidden_size = hidden_size
        self.lstm_layers = lstm_layers
        self.x_reals = x_reals
        self.smape = smape
        self.mape = mape
        self.mae = mae
        self.rmse = rmse
        self.loss = quantile_loss
        # self.reals = list(
        #     dict.fromkeys(static_reals + time_varying_reals_encoder + time_varying_reals_decoder)
        # )
        self.reals = list(dict.fromkeys(time_varying_reals_encoder + time_varying_reals_decoder))
        # self.static_variables = static_categoricals + static_reals
        self.static_variables = static_categoricals
        # self.encoder_variables = time_varying_categoricals_encoder + time_varying_reals_encoder
        self.encoder_variables = time_varying_reals_encoder
        # self.decoder_variables = time_varying_categoricals_decoder + time_varying_reals_decoder
        self.decoder_variables = time_varying_reals_decoder
        self.causal_attention = causal_attention

        # processing inputs embeddings
        self.input_embeddings = MultiEmbedding(
            embedding_sizes=embedding_sizes,
            x_categoricals=x_categoricals,
            # categorical_groups=categorical_groups,
        )

        # continuous variable processing
        self.prescalers = {
            name: Linear(
                in_features=1,
                out_features=hidden_continuous_sizes.get(name, hidden_continuous_size),
            )
            for name in self.reals
        }

        # variable selection for static variables
        static_input_sizes = {
            name: self.input_embeddings.output_size[name] for name in static_categoricals
        }
        # static_input_sizes.update(
        #     {
        #         name: hidden_continuous_sizes.get(name, hidden_continuous_size)
        #         for name in static_reals
        #     }
        # )
        self.static_variable_selection = VariableSelectionNetwork(
            input_sizes=static_input_sizes,
            hidden_size=hidden_size,
            input_embedding_flags={name: True for name in static_categoricals},
            dropout_rate=dropout_rate,
            prescalers=self.prescalers,
        )

        # variable selection for encoder and decoder
        # encoder_input_sizes = {
        #     name: self.input_embeddings.output_size[name]
        #     for name in time_varying_categoricals_encoder
        # }
        # encoder_input_sizes.update(
        #     {
        #         name: hidden_continuous_sizes.get(name, hidden_continuous_size)
        #         for name in time_varying_reals_encoder
        #     }
        # )

        # decoder_input_sizes = {
        #     name: self.input_embeddings.output_size[name]
        #     for name in time_varying_categoricals_decoder
        # }
        # decoder_input_sizes.update(
        #     {
        #         name: hidden_continuous_sizes.get(name, hidden_continuous_size)
        #         for name in time_varying_reals_decoder
        #     }
        # )

        # create single variable grns that are shared across decoder and encoder
        # if share_single_variable_networks:
        # self.shared_single_variable_grns = {}
        # for name, input_size in encoder_input_sizes.items():
        #     self.shared_single_variable_grns[name] = GatedResidualNetwork(
        #         input_size=input_size,
        #         hidden_size=min(input_size, hidden_size),
        #         output_size=hidden_size,
        #         dropout_rate=dropout_rate,
        #     )
        # for name, input_size in decoder_input_sizes.items():
        #     if name not in self.shared_single_variable_grns:
        #         self.shared_single_variable_grns[name] = GatedResidualNetwork(
        #             input_size=input_size,
        #             hidden_size=min(input_size, hidden_size),
        #             output_size=hidden_size,
        #             dropout_rate=dropout_rate,
        #         )

        # self.encoder_variable_selection = VariableSelectionNetwork(
        #     input_sizes=encoder_input_sizes,
        #     hidden_size=hidden_size,
        #     input_embedding_flags={name: True for name in time_varying_categoricals_encoder},
        #     dropout_rate=dropout_rate,
        #     context_size=hidden_size,
        #     prescalers=self.prescalers,
        #     single_variable_grns=(
        #         {} if not share_single_variable_networks else self.shared_single_variable_grns
        #     ),
        # )

        # self.decoder_variable_selection = VariableSelectionNetwork(
        #     input_sizes=decoder_input_sizes,
        #     hidden_size=hidden_size,
        #     input_embedding_flags={name: True for name in time_varying_categoricals_decoder},
        #     dropout_rate=dropout_rate,
        #     context_size=hidden_size,
        #     prescalers=self.prescalers,
        #     single_variable_grns=(
        #         {} if not share_single_variable_networks else self.shared_single_variable_grns
        #     ),
        # )

        # static encoders for variable selection
        self.static_context_variable_selection = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        # for hidden state of the lstm
        self.static_context_initial_hidden_lstm = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        # for cell state of the lstm
        self.static_context_initial_cell_lstm = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        # for post lstm static enrichment
        self.static_context_enrichment = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        # lstm encoder (history) and decoder (future) for local processing
        self.lstm_encoder = LSTM(
            input_size=hidden_size,
            hidden_size=hidden_size,
            layer_count=lstm_layers,
            dropout_rate=dropout_rate if lstm_layers > 1 else 0,
        )

        self.lstm_decoder = LSTM(
            input_size=hidden_size,
            hidden_size=hidden_size,
            layer_count=lstm_layers,
            dropout_rate=dropout_rate if lstm_layers > 1 else 0,
        )

        # skip connection for lstm
        self.post_lstm_gate_encoder = GatedLinearUnit(
            input_size=hidden_size,
            dropout_rate=dropout_rate,
        )
        self.post_lstm_gate_decoder = self.post_lstm_gate_encoder
        # self.post_lstm_gate_decoder = GatedLinearUnit(hidden_size, dropout=dropout)
        self.post_lstm_add_norm_encoder = AddNorm(input_size=hidden_size)
        # self.post_lstm_add_norm_decoder = AddNorm(hidden_size, trainable_add=True)
        self.post_lstm_add_norm_decoder = self.post_lstm_add_norm_encoder

        # static enrichment and processing past LSTM
        self.static_enrichment = GatedResidualNetwork(
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
            context_size=hidden_size,
        )

        # attention for long-range processing
        self.multihead_attn = InterpretableMultiHeadAttention(  # TODO: rename attribute
            heads_count=attention_head_size,
            models_count=hidden_size,
            dropout_rate=dropout_rate,
        )
        self.post_attn_gate_norm = GateAddNorm(  # TODO: rename attribute
            input_size=hidden_size,
            dropout_rate=dropout_rate,
        )
        self.pos_wise_ff = GatedResidualNetwork(  # TODO: rename attribute
            input_size=hidden_size,
            hidden_size=hidden_size,
            output_size=hidden_size,
            dropout_rate=dropout_rate,
        )

        # output processing -> no dropout at this late stage
        self.pre_output_gate_norm = GateAddNorm(
            input_size=hidden_size,
            dropout_rate=None,
        )

        if self.n_targets > 1:  # if to run with multiple targets # TODO: fix value (output_size?)
            self.output_layer = [
                Linear(
                    in_features=hidden_size,
                    out_features=output_size,
                )
                for output_size in output_size
            ]
        else:
            self.output_layer = Linear(
                in_features=hidden_size,
                out_features=output_size,
            )

    def forward(self, x: Dict[str, Tensor]) -> Dict[str, Tensor]:
        encoder_lengths = x["encoder_lengths"]
        decoder_lengths = x["decoder_lengths"]
        x_cat = Tensor.empty(x["encoder_cat"].shape)  # TODO: rename variable
        x_cat = x_cat.cat(
            x["encoder_categories"], x["decoder_categories"], dim=1
        )  # concatenate in time dimension

        x_cont = Tensor.empty(x["encoder_cont"].shape)  # TODO: rename variable
        x_cont = x_cont.cat(
            x["encoder_continous_variables"], x["decoder_continous_variables"], dim=1
        )  # concatenate in time dimension

        timesteps = x_cont.size(1)  # encode + decode length
        max_encoder_length = int(encoder_lengths.max())
        input_vectors = self.input_embeddings.forward(x_cat)
        input_vectors.update(
            {
                name: x_cont[..., idx].unsqueeze(-1)
                for idx, name in enumerate(self.x_reals)
                if name in self.reals
            }
        )

        # Embedding and variable selection
        if len(self.static_variables) > 0:
            # static embeddings will be constant over entire batch
            static_embedding = {name: input_vectors[name][:, 0] for name in self.static_variables}
            static_embedding, static_variable_selection = self.static_variable_selection.forward(
                x=static_embedding,
            )
        else:
            static_embedding = Tensor.zeros(
                (x_cont.size(0), self.hidden_size),
                dtype=self.dtype,
                device=self.device,
            )
            static_variable_selection = Tensor.zeros(
                (x_cont.size(0), 0), dtype=self.dtype, device=self.device
            )

        # static_context_variable_selection = self.expand_static_context(
        #     self.static_context_variable_selection.forward(static_embedding),
        #     timesteps,
        # )

        embeddings_varying_encoder = {
            name: input_vectors[name][:, :max_encoder_length] for name in self.encoder_variables
        }

        # embeddings_varying_encoder, encoder_sparse_weights = (
        #     self.encoder_variable_selection.forward(
        #         embeddings_varying_encoder,
        #         static_context_variable_selection[:, :max_encoder_length],
        #     )
        # )

        embeddings_varying_decoder = {
            name: input_vectors[name][:, max_encoder_length:]
            for name in self.decoder_variables  # select decoder
        }

        # embeddings_varying_decoder, decoder_sparse_weights = (
        #     self.decoder_variable_selection.forward(
        #         embeddings_varying_decoder,
        #         static_context_variable_selection[:, max_encoder_length:],
        #     )
        # )

        # LSTM calculate initial state
        input_hidden = self.static_context_initial_hidden_lstm.forward(static_embedding).expand(
            self.lstm_layers,
            -1,
            -1,
        )

        input_cell = self.static_context_initial_cell_lstm.forward(static_embedding).expand(
            self.lstm_layers,
            -1,
            -1,
        )

        # run local encoder
        encoder_output, (hidden, cell) = self.lstm_encoder.forward(
            embeddings_varying_encoder,
            (input_hidden, input_cell),
            # lengths=encoder_lengths,
            # enforce_sorted=False,
        )

        # run local decoder
        decoder_output, _ = self.lstm_decoder.forward(
            embeddings_varying_decoder,
            (hidden, cell),
            # lengths=decoder_lengths,
            # enforce_sorted=False,
        )

        # skip connection over lstm
        lstm_output_encoder = self.post_lstm_gate_encoder.forward(encoder_output)
        lstm_output_encoder = self.post_lstm_add_norm_encoder.forward(
            x=lstm_output_encoder,
            skip=embeddings_varying_encoder,
        )

        lstm_output_decoder = self.post_lstm_gate_decoder.forward(decoder_output)
        lstm_output_decoder = self.post_lstm_add_norm_decoder.forward(
            x=lstm_output_decoder,
            skip=embeddings_varying_decoder,
        )

        lstm_output = Tensor.empty(lstm_output_encoder.shape)
        lstm_output = lstm_output.cat(lstm_output_encoder, lstm_output_decoder, dim=1)

        # static enrichment
        static_context_enrichment = self.static_context_enrichment.forward(static_embedding)
        attn_input = self.static_enrichment.forward(  # TODO: rename variables
            x=lstm_output,
            context=self.expand_static_context(static_context_enrichment, timesteps),
        )

        # Attention
        attn_output, attn_output_weights = self.multihead_attn.forward(  # TODO: rename variables
            q=attn_input[:, max_encoder_length:],  # query only for predictions
            k=attn_input,
            v=attn_input,
            mask=self.get_attention_mask(
                encoder_lengths=encoder_lengths,
                decoder_lengths=decoder_lengths,
            ),
        )

        # skip connection over attention
        attn_output = self.post_attn_gate_norm.forward(  # TODO: rename variables
            x=attn_output,
            skip=attn_input[:, max_encoder_length:],
        )

        output = self.pos_wise_ff.forward(x=attn_output)

        # skip connection over temporal fusion decoder (not LSTM decoder despite the LSTM output
        # contains a skip from the variable selection network)
        output = self.pre_output_gate_norm.forward(output, lstm_output[:, max_encoder_length:])
        if self.n_targets > 1:  # if to use multi-target architecture
            output: List[Tensor] = [output_layer(output) for output_layer in self.output_layer]
        else:
            output = self.output_layer(output)

        # TODO: complete output transformation logic (see helper functions below)
        return {
            "prediction": None,
            #     prediction=self.transform_output(output, target_scale=x["target_scale"]),
            "encoder_attention": attn_output_weights[..., :max_encoder_length],
            "decoder_attention": attn_output_weights[..., max_encoder_length:],
            "static_variables": static_variable_selection,
            # "encoder_variables": encoder_sparse_weights,
            # "decoder_variables": decoder_sparse_weights,
            "decoder_lengths": decoder_lengths,
            "encoder_lengths": encoder_lengths,
        }

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
    ) -> Tensor:
        decoder_length = decoder_lengths.max()
        if self.causal_attention:
            # indices to which is attended
            attend_step = Tensor.arange(start=decoder_length, device=self.device)
            # indices for which is predicted
            predict_step = Tensor.arange(start=0, stop=decoder_length, device=self.device)[:, None]
            # do not attend to steps to self or after prediction
            decoder_mask = (
                (attend_step >= predict_step).unsqueeze(0).expand(encoder_lengths.size(0), -1, -1)
            )

        else:
            # there is value in attending to future forecasts if they are made with knowledge
            # currently available
            #   one possibility is here to use a second attention layer for future attention
            #   (assuming different effects matter in the future than the past) or alternatively
            #   using the same layer but allowing forward attention - i.e. only masking out
            #   non-available data and self
            decoder_mask = (
                self.create_mask(decoder_length, decoder_lengths)
                .unsqueeze(1)
                .expand(-1, decoder_length, -1)
            )

        # do not attend to steps where data is padded
        encoder_mask = (
            self.create_mask(encoder_lengths.max(), encoder_lengths)
            .unsqueeze(1)
            .expand(-1, decoder_length, -1)
        )

        # combine masks along attended time - first encoder and then decoder
        mask = Tensor.empty(encoder_mask.shape)
        mask = mask.cat(
            encoder_mask,
            decoder_mask,
            dim=2,
        )

        return mask

    def create_mask(
        size: int,
        lengths: Tensor,
    ) -> Tensor:
        return Tensor.arange(
            start=size,
            device=lengths.device,
        ).unsqueeze(0) >= lengths.unsqueeze(-1)

    def transform_output(
        self,
        prediction: Tensor,
        # prediction: Union[Tensor, List[Tensor]], # TODO: likely just a single tensor
        target_scale: List[Tuple[Tensor, Tensor]],
        # target_scale: Union[Tensor, List[Tensor]], # TODO: likely just a single tensor
        # loss: Optional[Metric] = None,
    ) -> Tensor:
        pass

    #     if isinstance(loss, MultiLoss):
    #         out = loss.rescale_parameters(
    #             prediction,
    #             target_scale=target_scale,
    #             encoder=self.output_transformer.normalizers,  # need to use normalizer per encoder
    #         )
    #     else:
    #         out = loss.rescale_parameters(
    #             prediction, target_scale=target_scale, encoder=self.output_transformer
    #         )
    #     return out

    # def rescale_parameters(
    #     self,
    #     parameters: torch.Tensor,
    #     target_scale: torch.Tensor,
    #     encoder: BaseEstimator,
    # ) -> torch.Tensor:
    #     """
    #     Rescale normalized parameters into the scale required for the output.

    #     Args:
    #         parameters (torch.Tensor): normalized parameters (indexed by last dimension)
    #         target_scale (torch.Tensor): scale of parameters (n_batch_samples x (center, scale))
    #         encoder (BaseEstimator): original encoder that normalized the target in the first place

    #     Returns:
    #         torch.Tensor: parameters in real/not normalized space
    #     """
    #     return encoder(dict(prediction=parameters, target_scale=target_scale))

    # TODO: complete train method
    def train(
        self,
        dataset: DataSet,  # TODO: change type if needed
    ) -> None:  # TODO: change type if needed
        # outline:
        # [ ] set scalers + metadata
        # [ ] iterate over dataset
        # [ ] apply fit + loss

        pass

    # TODO: build save model method
    # TODO: build load model method
    # TODO: build predict method
