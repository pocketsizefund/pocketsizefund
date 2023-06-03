import json
import random
import re

import openai

from pkg.config import config


samconfig_file = config.SAMConfig('samconfig.toml')

openai.api_key = samconfig_file.get_parameter('OpenAIAPIKey')

prompts = []
for i in range(1, 6):
    low_price = random.uniform(220, 222)
    high_price = random.uniform(low_price, low_price + 2)

    open_price = random.uniform(low_price, high_price)
    close_price = random.uniform(low_price, high_price)

    volume = int(random.uniform(200000, 300000))

    prompts.append('timestamp: 2018-02-0{}, close_price: {:.2f}'.format(
        i,
        close_price,
    ))

prompt = 'ESS - ' + ', '.join(prompts) + ' ->'

file_content = open('fine_tune_model_id.json', 'r').read()
file_json = json.loads(file_content)

openai_completion_response = openai.Completion.create(
    model=file_json['model_id'],
    prompt=prompt,
    max_tokens=250,
    temperature=0.2,
    stop=['\n'],
    frequency_penalty=1,
)

print('openai_completion_response:', openai_completion_response)

if len(openai_completion_response['choices']) == 0:
    print('no choices found')
    exit(1)

text = openai_completion_response['choices'][0]['text']

prices_strings = re.findall(r"\d+\.\d+", text)

prices_float = [float(value) for value in prices_strings]
