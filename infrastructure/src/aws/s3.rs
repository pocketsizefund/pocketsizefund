use anyhow::Result;
use rusoto_core::Region;
use rusoto_s3::{CreateBucketRequest, S3Client, S3};

pub async fn create_bucket(bucket_name: &str, region: Region) -> Result<()> {
    let client = S3Client::new(region);
    let create_bucket_req = CreateBucketRequest {
        bucket: bucket_name.to_string(),
        ..Default::default()
    };

    client.create_bucket(create_bucket_req).await?;
    println!("Created bucket: {}", bucket_name);
    Ok(())
}

pub async fn check_bucket_exists(bucket_name: &str, region: Region) -> Result<bool> {
    let client = S3Client::new(region);
    let head_bucket_req = rusoto_s3::HeadBucketRequest {
        bucket: bucket_name.to_string(),
        ..Default::default()
    };
    client.head_bucket(head_bucket_req).await?;
    tracing::info!("Bucket {} exists", bucket_name);
    Ok(true)
}
