import unittest

from pkg.message import message


class MockSNSClient:
    def __init__(
        self,
    ) -> None:
        pass

    def publish(
        self,
        TopicArn: str,
        Subject: str,
        Message: str,
    ) -> any:
        return None


class TestSendEmail(unittest.TestCase):
    def test_send_email_success(self):
        client = message.Client(
            topic_arn='topic_arn',
        )

        client.sns_client = MockSNSClient()

        client.send_message(
            subject='subject',
            message='message',
        )
