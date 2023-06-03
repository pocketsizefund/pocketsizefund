import json
import io
import random
import time

import pandas
import openai

from pkg.config import config
from pkg.storage import storage


# arbitrary minimum number of days to account for weekends and holidays
DAYS_COUNT = 30

samconfig_file = config.SAMConfig('samconfig.toml')

storage_client = storage.Client(
    s3_data_bucket_name=samconfig_file.get_parameter('S3DataBucketName'),
)

equities_bars_file_names = storage_client.list_file_names(
    prefix=storage.PREFIX_EQUITY_BARS_CLEAN_PATH
)

dataframes = storage_client.load_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_CLEAN_PATH,
    file_names=equities_bars_file_names,
)

dataframe = pandas.concat(dataframes)

grouped_dataframe = dataframe.groupby('ticker')

jsonl_data: list[dict[str, str]] = []
for group_name in grouped_dataframe.groups.keys():
    group = grouped_dataframe.get_group(group_name)

    sorted_group = group.sort_values(
        by='timestamp',
        ascending=False,
    )

    oldest_timestamp = sorted_group['timestamp'].min()
    newest_timestamp = sorted_group['timestamp'].max()

    difference = (newest_timestamp - oldest_timestamp).days

    if difference < DAYS_COUNT:
        continue

    oldest_unix = oldest_timestamp.timestamp()
    newest_unix = newest_timestamp.timestamp()

    for i in range(0, 3):
        random_unix = random.uniform(oldest_unix, newest_unix)

        start_timestamp = pandas.Timestamp.fromtimestamp(random_unix)
        end_timestamp = start_timestamp + pandas.Timedelta(days=DAYS_COUNT)

        if end_timestamp > newest_timestamp:
            end_timestamp = newest_timestamp
            start_timestamp = end_timestamp - pandas.Timedelta(days=DAYS_COUNT)

        selected_rows = sorted_group[
            (sorted_group['timestamp'] >= start_timestamp) &
            (sorted_group['timestamp'] <= end_timestamp)
        ]

        selected_rows.reset_index(
            inplace=True,
            drop=True,
        )

        close_prices = selected_rows['close_price']
        price_changes = close_prices.diff().tolist()
        # remove the first value which is NaN
        price_changes = price_changes[1:]
        price_changes.append(0.0)
        price_changes = [round(float(change), 2) for change in price_changes]

        inputs: list[str] = []
        for index, row in selected_rows.iterrows():
            input = 'timestamp: {}, price: {:.2f}'.format(
                row['timestamp'].date(),
                row['close_price'],
            )
            inputs.append(input)

        jsonl_data.append({
            'prompt': '{} - '.format(group_name) + ', '.join(inputs[5:]) + ' -> ',
            'completion': ' ' + ', '.join(inputs[:5]) + '\n',
        })

jsonl_file = io.StringIO()
for jsonl_line in jsonl_data:
    json.dump(jsonl_line, jsonl_file)
    jsonl_file.write('\n')

openai.api_key = samconfig_file.get_parameter('OpenAIAPIKey')

with open('fine_tune_data.jsonl', 'w') as file:
    file.write(jsonl_file.getvalue().rstrip('\n'))

openai_file_response = openai.File.create(
    file=bytes(jsonl_file.getvalue().rstrip('\n'), encoding='utf-8'),
    purpose='fine-tune',
)

file_id = openai_file_response['id']

openai_fine_tune_create_response = openai.FineTune.create(
    training_file=file_id,
    model='ada',
)

fine_tune_id = openai_fine_tune_create_response['id']

model_id = None
while model_id is None:
    openai_fine_tune_list_response = openai.FineTune.list_events(fine_tune_id)

    for event in openai_fine_tune_list_response['data']:
        message = event['message']
        if 'Uploaded model' in message:
            with open('fine_tune_model_creation_events.json', 'w') as file:
                file.write(json.dumps(openai_fine_tune_list_response['data']))

            model_id = message.split(': ')[1]
            break

    print('training model...')
    time.sleep(10)

with open('fine_tune_model_id.json', 'w') as file:
    file.write(json.dumps({
        'model_id': model_id
    }))

print('model id:', model_id)
