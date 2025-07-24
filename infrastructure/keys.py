import json

import pulumi
import pulumi_aws as aws
from tags import pulumi_tags


def create_duckdb_user_access_key(
    data_bucket_name: pulumi.Output[str],
) -> aws.iam.AccessKey:
    duckdb_user = aws.iam.User(
        resource_name="pocketsizefund-duckdb-user",
        name="pocketsizefund-duckdb-user",
        tags=pulumi_tags,
    )

    duckdb_policy = aws.iam.Policy(
        resource_name="pocketsizefund-duckdb-policy",
        name="pocketsizefund-duckdb-policy",
        description="Policy for application service DuckDB access",
        policy=json.dumps(
            {
                "Version": "2012-10-17",
                "Statement": [
                    {
                        "Effect": "Allow",
                        "Action": [
                            "s3:GetObject",
                            "s3:ListBucket",
                            "s3:PutObject",
                            "s3:DeleteObject",
                        ],
                        "Resource": [
                            f"arn:aws:s3:::{data_bucket_name}/*",
                            f"arn:aws:s3:::{data_bucket_name}",
                        ],
                    }
                ],
            }
        ),
        tags=pulumi_tags,
    )

    aws.iam.UserPolicyAttachment(
        resource_name="pocketsizefund-duckdb-user-policy",
        user=duckdb_user.name,
        policy_arn=duckdb_policy.arn,
    )

    return aws.iam.AccessKey(
        resource_name="pocketsizefund-duckdb-user-access-key",
        user=duckdb_user.name,
        status="Active",
    )
