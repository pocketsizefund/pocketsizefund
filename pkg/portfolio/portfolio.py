import copy

from finquant import portfolio
import pandas


class Client():
    def __init__(self):
        pass

    def calculate_weights(
        self,
        data: pandas.DataFrame,
    ) -> dict[str, float]:
        copied_data = copy.deepcopy(data)

        copied_data.pivot(
            index='timestamp',
            columns='ticker',
            values='close_price',
        ).reset_index().fillna(0.0).rename_axis(mapper=None, axis=1)

        copied_data.rename(
            columns={'timestamp': 'Date'},
            inplace=True,
        )

        numreric_data = copied_data.select_dtypes(include=[float, int])

        ticker_portfolio = portfolio.build_portfolio(data=numreric_data)

        weights = ticker_portfolio.ef_efficient_return(target=0.25)

        return weights.to_dict()['Allocation']
