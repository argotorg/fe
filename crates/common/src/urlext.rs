use camino::Utf8PathBuf;
use url::Url;

#[derive(Debug)]
pub enum UrlExtError {
    DirectoryRangeError,
    AsDirectoryError,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UrlError {
    InvalidPath,
    JoinError,
    CanonicalizationError,
}

pub trait UrlExt {
    fn parent(&self) -> Option<Url>;
    fn directory(&self) -> Option<Url>;
    fn join_directory(&self, path: &Utf8PathBuf) -> Result<Url, UrlError>;
}

impl UrlExt for Url {
    fn directory(&self) -> Option<Url> {
        if self.cannot_be_a_base() {
            return None;
        };
        let mut url = self.clone();

        if url.path().ends_with('/') {
            return Some(url);
        }

        if let Ok(mut segments) = url.path_segments_mut() {
            segments.pop();
            segments.push("");
        }

        Some(url)
    }

    fn parent(&self) -> Option<Url> {
        let directory = self.directory()?;

        if *self != directory {
            // If we're not already at a directory, return the directory
            Some(directory)
        } else if self.path() == "/" {
            None
        } else {
            // We're already at a directory, go up one level
            let mut parent = self.clone();
            if let Ok(mut segments) = parent.path_segments_mut() {
                segments.pop();
                segments.pop();
                segments.push(""); // Ensure trailing slash
            }
            Some(parent)
        }
    }

    fn join_directory(&self, path: &Utf8PathBuf) -> Result<Url, UrlError> {
        Ok(if path.as_str().ends_with("/") {
            self.join(path.as_str()).map_err(|_| UrlError::JoinError)?
        } else {
            let mut path = path.clone();
            path.push("");
            self.join(path.as_str()).map_err(|_| UrlError::JoinError)?
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_directory_basic() {
        let url = Url::parse("https://example.com/foo/bar/baz").unwrap();
        let directory = url.directory();
        assert!(directory.is_some());
        assert_eq!(directory.unwrap().as_str(), "https://example.com/foo/bar/");

        let url = Url::parse("https://example.com/foo/bar/baz/").unwrap();
        let directory = url.directory();
        assert!(directory.is_some());
        assert_eq!(
            directory.unwrap().as_str(),
            "https://example.com/foo/bar/baz/"
        );
    }

    #[test]
    fn test_parent_basic() {
        let url = Url::parse("https://example.com/foo/bar").unwrap();
        let parent = url.parent();
        assert!(parent.is_some());
        assert_eq!(parent.unwrap().as_str(), "https://example.com/foo/");

        let url = Url::parse("https://example.com/foo/").unwrap();
        let parent = url.parent();
        assert!(parent.is_some());
        assert_eq!(parent.unwrap().as_str(), "https://example.com/");

        let url = Url::parse("https://example.com/").unwrap();
        let parent = url.parent();
        assert!(
            parent.is_none(),
            "Parent should be `None` but instead we got {parent:?}"
        );
    }

    #[test]
    fn test_file_url_parent_behavior() {
        // Test file:// URL paths
        let file_url = Url::parse("file:///foo/bar/baz.txt").unwrap();

        // Test directory() behavior
        let dir = file_url.directory();
        assert!(dir.is_some());
        assert_eq!(dir.unwrap().as_str(), "file:///foo/bar/");

        // Test parent() behavior - from file to directory
        let parent = file_url.parent();
        assert!(parent.is_some());
        assert_eq!(parent.unwrap().as_str(), "file:///foo/bar/");

        // Test parent of directory
        let dir_url = Url::parse("file:///foo/bar/").unwrap();
        let parent = dir_url.parent();
        assert!(parent.is_some());
        let parent_url = parent.unwrap();
        assert_eq!(parent_url.as_str(), "file:///foo/");

        // Test parent of parent
        let parent_of_parent = parent_url.parent();
        assert!(parent_of_parent.is_some());
        assert_eq!(parent_of_parent.unwrap().as_str(), "file:///");

        // Test parent of root
        let root_url = Url::parse("file:///").unwrap();
        let root_parent = root_url.parent();
        assert!(root_parent.is_none(), "Root URL should have no parent");
    }
}
