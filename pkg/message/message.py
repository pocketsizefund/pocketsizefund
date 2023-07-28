import boto3


class Client:
    def __init__(
        self,
        topic_arn: str,
    ):
        self.sns_client = boto3.client('sns')
        self.topic_arn = topic_arn

    def send_message(
        self,
        message: str,
        subject: str,
    ):
        self.sns_client.publish(
            TopicArn=self.topic_arn,
            Subject=subject,
            Message=message,
        )
