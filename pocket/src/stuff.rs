use clap::Parser;
use std::path::Path;
use walkdir::WalkDir;
use ignore::Walk;
use qdrant_client::prelude::*;
use tokio;

use qdrant_client::qdrant::{
    Condition, CreateCollectionBuilder, Distance, Filter, PointStruct, ScalarQuantizationBuilder,
    SearchParamsBuilder, SearchPointsBuilder, UpsertPointsBuilder, VectorParamsBuilder,
};
use qdrant_client::{Payload, Qdrant, QdrantError};


#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The path to the repository to scan
    #[arg(short, long)]
    repo_path: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    println!("args: {:#?}", args);
    // let qdrant_url = std::env::var("QDRANT_URL").expect("QDRANT_URL must be set");
    // let qdrant_api_key = std::env::var("QDRANT_API_KEY").expect("QDRANT_API_KEY must be set");
    //
    // let client = Qdrant::from_url(&qdrant_url).api_key(qdrant_api_key).build()?;
    //
    // let collection = match create_collection(&client, "psf").await {
    //     Ok(collection) => collection,
    //     Err(e) => {
    //         tracing::warn!("Failed to create collection: {:?}", e);
    //     }
    // };
    //
    // let collections_list = client.list_collections().await?;
    //
    // println!("Collections: {:#?}", collections_list);
    //
    // let repo_path = Path::new(".");
    //
    // let paths = scan_repository(repo_path)?;
    //
    // println!("paths: {:#?}", paths);

    Ok(())
}


async fn scan_repository(client: &Qdrant, collection_name: &str, path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    for entry in Walk::new(path) {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() {
            upload_file_to_qdrant(client, path).await?;

        }
    }
    Ok(())
}

async fn upload_file_to_qdrant(client: &Qdrant, path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string(path).context("Failed to read file")?;
    let file_name = path.file_name().unwrap().to_str();
    let collection = client.get_collection(collection_name).await?;
    let points = collection.points(file_name).await?;
    if points.is_empty() {
        let payload = Payload::from_file(path)?;
        client.upsert_points(collection_name, vec![payload]).await?;
    }
    Ok(())
}



async fn create_collection(client: &Qdrant, collection_name: &str) -> Result<(), Box<dyn std::error::Error>> {
    client.create_collection(
        CreateCollectionBuilder::new(collection_name)
            .vectors_config(VectorParamsBuilder::new(10, Distance::Cosine)
            .quantization_config(ScalarQuantizationBuilder::default()))
    ).await?;
    Ok(())
}
