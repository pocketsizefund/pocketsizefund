provider "aws" {
  region = "us-east-1"
}

resource "aws_s3_bucket" "pocketsizefund-unique-bucket-name" {
  bucket = "pocketsizefund-unique-bucket-name"
  acl = "private"
  force_destroy = true
  tags = {
    "environment" = "live"
  }
}
