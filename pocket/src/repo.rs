
use std::path::Path;
use walkdir::WalkDir;
use ignore::Walk;
use std::path::PathBuf;

async fn scan_repository(path: &Path) {
    // return a vec of paths
    let paths = get_paths(path);

    for path in paths {
        println!("{:?}", path);

    }

}

fn get_paths(path: &Path) -> Vec<PathBuf> {
    let paths = WalkDir::new(path).into_iter().filter_map(|e| e.ok()).filter(|e| e.file_type().is_file());

    paths
}
