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


class TestGenerateInviteCode(unittest.TestCase):
    def test_generate_invite_code_success(self):
        client = invite.Client(
            secret_key='secret_key',
            base_url='https://base_url.com',
            client_id='client_id',
            client_secret='client_secret',
        )

        code = client.generate_invite_code(
            user_id='user_id',
        )

        self.assertEqual(len(code), 64)


class TestGenerateInviteURL(unittest.TestCase):
    def test_generate_invite_url_success(self):
        client = invite.Client(
            secret_key='secret_key',
            base_url='https://base_url.com',
            client_id='client_id',
            client_secret='client_secret',
        )

        invite_url = client.generate_invite_url(
            invite_code='invite_code',
        )

        self.assertEqual(
            invite_url,
            'https://base_url.com/accept_invite?invite_code=invite_code',
        )


class TestGenerateRedirectURL(unittest.TestCase):
    def test_generate_redirect_url_success(self):
        client = invite.Client(
            secret_key='secret_key',
            base_url='https://base_url.com',
            client_id='client_id',
            client_secret='client_secret',
        )

        redirect_url = client.generate_redirect_url()

        self.assertEqual(
            redirect_url,
            'https://base_url.com/complete',
        )


class TestRedirectToAuthorizeURL(unittest.TestCase):
    def test_redirect_to_authorize_url_success(self):
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

        client.redirect_to_authorize_url(
            redirect_url='redirect_url',
            state='state',
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
