import json
import io

import pandas
import openai

from pkg.config import config
from pkg.storage import storage


samconfig_file = config.SAMConfig('samconfig.toml')

storage_client = storage.Client(
    s3_data_bucket_name=samconfig_file.get_parameter('S3DataBucketName'),
)

equities_bars_file_names = storage_client.list_file_names(
    prefix=storage.PREFIX_EQUITY_BARS_PROCESSED_PATH
)

dataframes = storage_client.load_dataframes(
    prefix=storage.PREFIX_EQUITY_BARS_PROCESSED_PATH,
    file_names=equities_bars_file_names,
)

dataframe = pandas.concat(dataframes)

grouped_dataframe = dataframe.groupby(dataframe.ticker)

jsonl_data = []
for group_name in grouped_dataframe.groups.keys():
    group = grouped_dataframe.get_group(group_name)

    sorted_group = group.sort_values(by='timestamp', ascending=False)

    close_prices = sorted_group['close_price']
    price_changes = close_prices.diff().tolist()
    price_changes = price_changes[1:]  # Remove the first value which is NaN
    price_changes.append(0.0)
    price_changes = [float(change) for change in price_changes]

    for index, row in sorted_group.iterrows():
        completion = 'Expected Next Move: {}'
        change = price_changes[index[1]]
        if change > 0.0:
            completion = completion.format('UP')
        elif change < 0.0:
            completion = completion.format('DOWN')
        else:
            completion = completion.format('FLAT')

        prompt = row.to_json()

        jsonl_data.append({
            'prompt': prompt,
            'completion': completion,
        })

openai.api_key = samconfig_file.get_parameter('OpenAIAPIKey')

jsonl_file = io.StringIO()
for jsonl_line in jsonl_data:
    json.dump(jsonl_line, jsonl_file)
    jsonl_file.write('\n')


openai_file_response = openai.File.create(
    file=bytes(jsonl_file.getvalue() + '\n', encoding='utf-8'),
    purpose='fine-tune',
)

file_id = openai_file_response['id']

openai_fine_tune_response = openai.FineTune.create(
    training_file=file_id,
)

model_id = openai_fine_tune_response['id']

print(model_id)
