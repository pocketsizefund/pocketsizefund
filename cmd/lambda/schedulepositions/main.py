import os
import datetime

from pkg.trade import trade


trade_client = trade.Client(
    darqube_api_key=os.getenv('DARQUBE_API_KEY'),
    alpaca_api_key=os.getenv('ALPACA_API_KEY'),
    alpaca_api_secret=os.getenv('ALPACA_API_SECRET'),
    alpha_vantage_api_key=os.getenv('ALPHA_VANTAGE_API_KEY'),
    is_paper=True if os.getenv('IS_PAPER') == 'true' else False,
)


def handler(
    event: any,
    context: any,
) -> dict[str, any]:
    _ = event, context

    trade_client.set_position_schedules(
        start_at=datetime.datetime.now(),
        create_positions_lambda_arn=os.getenv('CREATE_POSITIONS_LAMBDA_ARN'),
        clear_positions_lambda_arn=os.getenv('CLEAR_POSITIONS_LAMBDA_ARN'),
        invoke_lambda_role_arn=os.getenv('INVOKE_LAMBDA_ROLE_ARN'),
    )
