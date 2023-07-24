import copy

from finquant import portfolio
import pandas


class Client():
    def __init__(self):
        # os.environ['MPLCONFIGDIR'] = '/tmp/matplotlib' # suppress warning log

        pass

    def calculate_weights(
        self,
        data: pandas.DataFrame,
    ) -> dict[str, float]:
        copied_data = copy.deepcopy(data)

        copied_data.drop_duplicates(
            subset=['timestamp', 'ticker'],
            inplace=True,
        )

        pivoted_data = copied_data.pivot(
            index='timestamp',
            columns='ticker',
            values='close_price',
        )
        pivoted_data.reset_index(inplace=True)
        pivoted_data.rename_axis(mapper=None, axis=1, inplace=True)

        pivoted_data.rename(
            columns={'timestamp': 'Date'},
            inplace=True,
        )

        numeric_data = pivoted_data.select_dtypes(include=[float, int])

        ticker_portfolio = portfolio.build_portfolio(data=numeric_data)

        weights = ticker_portfolio.ef_efficient_return(target=0.25)

        return weights.to_dict()['Allocation']
