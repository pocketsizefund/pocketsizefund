import pandas
import lightning.pytorch as pl
from lightning.pytorch.loggers import TensorBoardLogger
from lightning.pytorch.callbacks import EarlyStopping, LearningRateMonitor
from pytorch_forecasting import TemporalFusionTransformer, TimeSeriesDataSet, metrics
from pytorch_forecasting.metrics import MAE, SMAPE, PoissonLoss, QuantileLoss, MultiLoss, RMSE
from pytorch_forecasting.data import GroupNormalizer, MultiNormalizer
import torch
from lightning.pytorch.loggers import WandbLogger


from icecream import ic


class Model:
    def __init__(self):
        self.model = None

    def train_model(
        self,
        data: pandas.DataFrame,
    ):
        
        # outline:
        # [ ] ensure equal count per ticker (?)
        # [ ] ensure no missing data
        # [ ] ensure no duplicates
        # [ ] ensure no NaNs
        # [ ] ensure all timestamps represented per ticker (?)



        start_date = data["timestamp"].min()
        end_date = data["timestamp"].max()

        dates = pandas.date_range(start_date, end_date, freq="d")
        time_indexes = range(len(dates))
        dates = pandas.DataFrame({"time_index": time_indexes, "date": dates})

        data = data.merge(dates, left_on="timestamp", right_on="date")

        data['weekday'] = data['timestamp'].dt.weekday
        data['yearday'] = data['timestamp'].dt.dayofyear

        data['ticker'] = pandas.Categorical(data['ticker'])

        print('data:', data)

        maximum_prediction_length = 5
        maximum_encoder_length = 24 # should be 20 (?)
        # training_cutoff = data["time_idx"].max() - max_prediction_length

        # print('data:', data)

        training = TimeSeriesDataSet(
            data=data,
            time_idx='time_index',
            target=[
                'open_price',
                'high_price',
                'low_price',
                'close_price', 
            ],
            group_ids=['ticker'],
            min_encoder_length=maximum_encoder_length // 2,
            max_encoder_length=maximum_encoder_length,
            min_prediction_length=1,
            max_prediction_length=maximum_prediction_length,
            static_categoricals=['ticker'],
            # static_reals=['weekday', 'yearday'],
            # time_varying_known_categoricals=['weekday', 'yearday'],
            # variable_groups={},
            time_varying_known_reals=['time_index'],
            # time_varying_unknown_categoricals=[],
            time_varying_unknown_reals=[
                'open_price',
                'high_price',
                'low_price',
                'close_price',
                'volume',
            ],
            target_normalizer=MultiNormalizer(
                normalizers=[
                    GroupNormalizer(
                        groups=['ticker'],
                        transformation='softplus',
                    ),
                    GroupNormalizer(
                        groups=['ticker'],
                        transformation='softplus',
                    ),
                    GroupNormalizer(
                        groups=['ticker'],
                        transformation='softplus',
                    ),
                    GroupNormalizer(
                        groups=['ticker'],
                        transformation='softplus',
                    ),
                    # GroupNormalizer(
                    #     groups=['ticker'],
                    #     transformation='softplus',
                    # ),
                ]
            ),
            # target_normalizer=GroupNormalizer(
            #     groups=['ticker'],
            #     transformation='softplus',
            # ),
            add_relative_time_idx=True,
            add_target_scales=True,
            add_encoder_length=True,
            allow_missing_timesteps=True, # TEMP
        )


        print('reaching here -----------------------')

        validation = TimeSeriesDataSet.from_dataset(
            training, 
            data, 
            predict=True, 
            stop_randomization=True,
        )


        print('reaching here 2 -----------------------')

        batch_size = 128  # set this between 32 to 128
        train_dataloader = training.to_dataloader(
            train=True,
            batch_size=batch_size,
            num_workers=0,
        )




        validation_dataloader = validation.to_dataloader(
            train=False, 
            batch_size=batch_size,  # FIXED? batch_size * 10 may have been causing the emptiness
            num_workers=0,
        )



        pl.seed_everything(42)
        # trainer = pl.Trainer(
        #     accelerator="cpu",
        #     gradient_clip_val=0.1,
        # )

        # tft = TemporalFusionTransformer.from_dataset(
        #     training,
        #     # not meaningful for finding the learning rate but otherwise very important
        #     learning_rate=0.03,
        #     hidden_size=8,  # most important hyperparameter apart from learning rate
        #     # number of attention heads. Set to up to 4 for large datasets
        #     attention_head_size=1,
        #     dropout=0.1,  # between 0.1 and 0.3 are good values
        #     hidden_continuous_size=8,  # set to <= hidden_size
        #     loss=QuantileLoss(),
        #     optimizer="Ranger"
        #     # reduce learning rate if no improvement in validation loss after x epochs
        #     # reduce_on_plateau_patience=1000,
        # )

        # # find optimal learning rate
        # from lightning.pytorch.tuner import Tuner

        # res = Tuner(trainer).lr_find(
        #     tft,
        #     train_dataloaders=train_dataloader,
        #     val_dataloaders=val_dataloader,
        #     max_lr=10.0,
        #     min_lr=1e-6,
        # )

        # print(f"suggested learning rate: {res.suggestion()}")
        # fig = res.plot(show=True, suggest=True)
        # fig.show()

        # # configure network and trainer
        early_stop_callback = EarlyStopping(
            monitor="val_loss", 
            min_delta=1e-4, 
            patience=10, 
            verbose=False, 
            mode="min",
        )
        learning_rate_logger = LearningRateMonitor()  # log the learning rate
        logger = TensorBoardLogger("lightning_logs")  # logging results to a tensorboard

        trainer = pl.Trainer(
            max_epochs=50,
            accelerator="gpu",
            enable_model_summary=True,
            gradient_clip_val=0.1,
            limit_train_batches=50,  # comment in for training, running valiation every 30 batches
            # fast_dev_run=True,  # comment in to check that networkor dataset has no serious bugs
            # logger = WandbLogger(project="FILL IN"),
            callbacks=[
                learning_rate_logger,
                early_stop_callback,
            ],
            logger=logger,
        )

        loss = MultiLoss(
            metrics=[RMSE()],
            # metrics=metrics,
            # metrics=[QuantileLoss()],
            # losses=[
            #     torch.nn.MSELoss(), 
            #     torch.nn.L1Loss(),
            # ],
        )

        model = TemporalFusionTransformer.from_dataset(
            dataset=training,
            learning_rate=0.03,
            hidden_size=16,
            attention_head_size=2,
            dropout=0.1,
            hidden_continuous_size=8,
            loss=RMSE(),
            log_interval=10,  # uncomment for learning rate finder and otherwise, e.g. to 10 for logging every 10 batches
            optimizer="Ranger",
            reduce_on_plateau_patience=4,
        )

        print('train_dataloader:', train_dataloader)
        print('validation_dataloader:', validation_dataloader)


        trainer.fit(
            model,
            train_dataloaders=train_dataloader,
            val_dataloaders=validation_dataloader,
        )

        self.model = model

    def save_model(self):
        pass # TEMP

    def load_model(self):
        pass # TEMP

    def get_predictions(self):

        # outline:
        # [ ] accept predicting input data
        # - [ ] received from the call site not from Client.get_predictions
        # - - [ ] e.g. data_client gets data in sagemaker endpoint
        #              in order to not handle parsing data in the caller
        # [ ] invoke trained model
        # [ ] 

        pass # TEMP


data = pandas.read_csv("pkg/model/test_data.csv")

data['timestamp'] = pandas.to_datetime(data['timestamp'])

print('data:', data)

model = Model()

model.train_model(
    data=data,
)

class Client:
    def __init__(self):
        pass # TEMP

    def get_predictions(
        self,
        data: pandas.DataFrame,
    ):
        
        # outline:
        # [ ] configure sagemaker endpoint
        # [ ] invoke endpoint
        # [ ] parse response predictions
        # [ ] return parsed predictions

        pass # TEMP
