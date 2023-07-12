from finquant import portfolio
import pandas


class Client():
    def __init__(
        self,
        data: pandas.DataFrame
    ):
        self.data = data.pivot(
            index='timestamp',
            columns='ticker',
            values='close_price',
        ).reset_index().fillna(0.0).rename_axis(mapper=None, axis=1)

        self.data.rename(
            columns={'timestamp': 'Date'},
            inplace=True,
        )

    def calculate_weights(
        self,
        tickers: list[str],
    ) -> dict[str, float]:
        columns = ['Date'] + tickers

        ticker_data = self.data[columns]
        ticker_data_numeric = ticker_data.select_dtypes(
            include=[float, int],
        )

        ticker_portfolio = portfolio.build_portfolio(
            data=ticker_data_numeric,
        )

        weights = ticker_portfolio.ef_efficient_return(target=0.25)

        return weights.to_dict()['Allocation']
