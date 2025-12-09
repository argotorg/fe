use std::path::{Path, PathBuf};
use url::Url;

/// Extension trait for URL to provide cross-platform compatibility
#[allow(clippy::result_unit_err)]
pub trait UrlExt {
    /// Create a URL from a file path.
    ///
    /// This is the native `Url::from_file_path` function on native platforms,
    /// but implemented manually for WASM targets where the original is unavailable.
    fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Url, ()>;

    /// Creates a URL from a file path with full error handling
    fn from_file_path_lossy<P: AsRef<Path>>(path: P) -> Url;

    /// Converts a URL to a file path
    fn to_file_path(&self) -> Result<PathBuf, ()>;
}

impl UrlExt for Url {
    fn from_file_path<P: AsRef<Path>>(path: P) -> Result<Url, ()> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            Url::from_file_path(path)
        }

        #[cfg(target_arch = "wasm32")]
        {
            let path = path.as_ref();
            // For WASM, we need to manually construct the URL
            let path_str = path.to_string_lossy();
            let url_str = if path_str.starts_with('/') {
                format!("file://{}", path_str)
            } else {
                format!("file:///{}", path_str)
            };
            Url::parse(&url_str).map_err(|_| ())
        }
    }

    fn from_file_path_lossy<P: AsRef<Path>>(path: P) -> Url {
        Self::from_file_path(path).expect("Failed to create URL from file path")
    }

    fn to_file_path(&self) -> Result<PathBuf, ()> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            url::Url::to_file_path(self)
        }

        #[cfg(target_arch = "wasm32")]
        {
            // Basic implementation for WASM - extract path from URL
            if self.scheme() != "file" {
                return Err(());
            }

            let path = self.path();
            Ok(PathBuf::from(path))
        }
    }
}
