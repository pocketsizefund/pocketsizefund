use std::path::Path;
use clap::Parser;
use pocket::repo::scan_repository;


/// A simple Pocket Size Fund CLI
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The path to the repository to scan
    #[arg(short, long, default_value = ".")]
    repo_path: String,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    println!("args: {:#?}", args);

    let repo_path = Path::new(&args.repo_path);

    scan_repository(repo_path).await;
}


