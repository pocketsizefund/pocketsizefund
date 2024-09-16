use infrastructure::aws::s3::{check_bucket_exists, create_bucket};
use infrastructure::config;

#[tokio::main]
async fn main() {
    let aws_config = config::Aws::default();
    let bucket_name = "my-fund-data-bucket";
    let bucket_region = aws_config.region.clone();

    tracing::info!("hello world");

    tracing::info!("creating bucket {}", bucket_name);

    // if check_bucket_exists(bucket_name, bucket_region.clone())
    //     .await
    //     .unwrap()
    // {
    //     println!("Bucket {} already exists", bucket_name);
    // } else {
    //     println!("Creating bucket {}", bucket_name);
    //     create_bucket(bucket_name, bucket_region).await.unwrap();
    // }
}

// // Usage in main.rs or another function
// use eks_cluster_manager::aws::s3::create_bucket;
//
// async fn setup_buckets() -> Result<()> {
//     let region = Region::UsEast1; // Replace with your desired region
//     create_bucket("my-fund-data-bucket", region.clone()).await?;
//     create_bucket("my-fund-artifacts-bucket", region).await?;
//     Ok(())
//
//
// }
//
