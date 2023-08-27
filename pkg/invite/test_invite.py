import unittest

from pkg.invite import invite


class MockHTTPAuthorizationTokenResponse:
    def __init__(
        self,
        data: dict[str, any],
    ) -> None:
        self.data = data

    def json(self) -> dict[str, any]:
        return self.data


class MockHTTPClient:
    def __init__(
        self,
        response: MockHTTPAuthorizationTokenResponse,
    ) -> None:
        self.response = response

    def get(
        self,
        **kwargs,
    ) -> any:
        pass

    def post(
        self,
        **kwargs,
    ) -> any:
        return self.response


class TestHashValue(unittest.TestCase):
    def test_hash_value_success(self):
        client = invite.Client(
            secret_key='secret_key',
            base_url='https://base_url.com',
            client_id='client_id',
            client_secret='client_secret',
        )

        code = client.hash_value(
            input='user_id',
        )

        self.assertEqual(len(code), 64)


class TestGetRedirectURL(unittest.TestCase):
    def test_get_redirect_url_success(self):
        client = invite.Client(
            secret_key='secret_key',
            base_url='https://base_url.com',
            client_id='client_id',
            client_secret='client_secret',
        )

        redirect_url = client.get_redirect_url()

        self.assertEqual(
            redirect_url,
            'https%3A//base_url.com/complete_invite',
        )


class TestGetAuthorizeURL(unittest.TestCase):
    def test_get_authorize_url_success(self):
        client = invite.Client(
            secret_key='secret_key',
            base_url='https://base_url.com',
            client_id='client_id',
            client_secret='client_secret',
        )

        client.http_client = MockHTTPClient(
            response=MockHTTPAuthorizationTokenResponse(
                data={},
            )
        )

        authorize_url = client.get_authorize_url(
            redirect_url='redirect_url',
            state='state',
        )

        self.assertEqual(
            authorize_url,
            'https://app.alpaca.markets/oauth/authorize?response_type=code&client_id=client_id&redirect_uri=redirect_url&state=state&scope=account:write%20trading',
        )


class TestGetAccessToken(unittest.TestCase):
    def test_get_access_token_success(self):
        client = invite.Client(
            secret_key='secret_key',
            base_url='https://base_url.com',
            client_id='client_id',
            client_secret='client_secret',
        )

        client.http_client = MockHTTPClient(
            response=MockHTTPAuthorizationTokenResponse(
                data={
                    'access_token': 'access_token',
                }
            ),
        )

        access_token = client.get_access_token(
            code='code',
            redirect_url='redirect_url',
        )

        self.assertEqual(
            access_token,
            'access_token',
        )
