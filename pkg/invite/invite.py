import hmac
import hashlib
from urllib import parse

import requests


class Client:
    def __init__(
        self,
        secret_key: str,
        base_url: str,
        client_id: str,
        client_secret: str,
    ) -> None:
        self.secret_key = secret_key.encode(
            'utf-8',
        )
        self.base_url = base_url
        self.client_id = client_id
        self.client_secret = client_secret
        self.http_client = requests

    def hash_value(
        self,
        input: str,
    ) -> str:
        output = hmac.new(
            self.secret_key,
            input.encode('utf-8'),
            hashlib.sha256,
        ).hexdigest()

        return output

    def get_redirect_url(
        self,
    ) -> str:
        redirect_url = '{}/complete_invite'.format(
            self.base_url,
        )

        encoded_redirect_url = parse.quote(
            redirect_url,
        )

        return encoded_redirect_url

    def get_authorize_url(
        self,
        redirect_url: str,
        state: str,
    ) -> str:
        authorize_url = 'https://app.alpaca.markets/oauth/authorize?response_type=code&client_id={}&redirect_uri={}&state={}&scope=account:write%20trading'.format(
            self.client_id,
            redirect_url,
            state,
        )

        return authorize_url

    def get_access_token(
        self,
        code: str,
        redirect_url: str,
    ) -> str:
        oauth_token_url = 'https://api.alpaca.markets/oauth/token?grant_type=authorization_code&code={}&client_id={}&client_secret={}&redirect_uri={}'.format(
            code,
            self.client_id,
            self.client_secret,
            redirect_url,
        )

        response = self.http_client.post(
            url=oauth_token_url,
        )

        access_token = response.json()['access_token']

        return access_token
