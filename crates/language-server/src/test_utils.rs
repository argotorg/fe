#[cfg(test)]
use driver::{DriverDataBase, init_ingot};
use std::path::Path;
use url::Url;

/// Load all files from an ingot directory into the database
/// This is similar to what happens during initialization or when a new fe.toml is created
#[allow(clippy::print_stderr)]
#[cfg(test)]
pub fn load_ingot_from_directory(db: &mut DriverDataBase, ingot_dir: &Path) {
    let ingot_url =
        Url::from_directory_path(ingot_dir).expect("Failed to create URL from directory path");

    let diagnostics = init_ingot(db, &ingot_url);

    // In tests, we might want to panic on serious errors
    for diagnostic in &diagnostics {
        match diagnostic {
            driver::IngotInitDiagnostics::MissingFeToml { .. }
            | driver::IngotInitDiagnostics::InvalidToml { .. } => {
                panic!("Failed to resolve test ingot at {ingot_dir:?}: {diagnostic}");
            }
            _ => {
                // Log other diagnostics but don't panic
                eprintln!("Test ingot diagnostic for {ingot_dir:?}: {diagnostic}");
            }
        }
    }
}
