use camino::Utf8PathBuf;
use driver::{DependencyTree, DriverDataBase, init_ingot};
use url::Url;

pub fn print_tree(path: &Utf8PathBuf) {
    let ingot_url = match ingot_url(path) {
        Ok(url) => url,
        Err(message) => {
            eprintln!("{message}");
            return;
        }
    };

    let mut db = DriverDataBase::default();
    let _ = init_ingot(&mut db, &ingot_url);

    let tree = DependencyTree::build(&db, &ingot_url);
    print!("{}", tree.display());
}

fn ingot_url(path: &Utf8PathBuf) -> Result<Url, String> {
    let canonical_path = path
        .canonicalize_utf8()
        .map_err(|_| format!("Error: invalid or non-existent directory path: {path}"))?;

    if !canonical_path.is_dir() {
        return Err(format!("Error: {path} is not a directory"));
    }

    Url::from_directory_path(canonical_path.as_str())
        .map_err(|_| format!("Error: invalid directory path: {path}"))
}
