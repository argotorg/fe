use camino::Utf8PathBuf;
use radix_immutable::{StringPrefixView, StringTrie, Trie};
use salsa::{Durability, Setter};
use url::Url;

use crate::{InputDb, file::File, indexmap::ArcIndexMap, stdlib::is_embedded_builtin};

#[derive(Debug)]
pub enum InputIndexError {
    CannotReuseInput,
}

/// The files of the compilation, by URL.
///
/// The embedded `core` and `std` files are kept apart from the user's files,
/// at high durability, so that a change to the user's files leaves every query
/// that reads only the builtins verified without walking its dependencies.
#[salsa::input]
#[derive(Debug)]
pub struct Workspace {
    builtin_files: StringTrie<Url, File>,
    builtin_paths: ArcIndexMap<File, Url>,
    files: StringTrie<Url, File>,
    paths: ArcIndexMap<File, Url>,
}

#[salsa::tracked]
impl Workspace {
    pub fn default(db: &dyn InputDb) -> Self {
        Workspace::builder(
            Trie::new(),
            ArcIndexMap::default(),
            Trie::new(),
            ArcIndexMap::default(),
        )
        .builtin_files_durability(Durability::HIGH)
        .builtin_paths_durability(Durability::HIGH)
        .new(db)
    }

    /// The files under `url`'s scheme.
    fn files_for<'db>(&self, db: &'db dyn InputDb, url: &Url) -> &'db StringTrie<Url, File> {
        if is_embedded_builtin(url) {
            self.builtin_files(db)
        } else {
            self.files(db)
        }
    }

    /// The paths under `url`'s scheme.
    fn paths_for<'db>(&self, db: &'db dyn InputDb, url: &Url) -> &'db ArcIndexMap<File, Url> {
        if is_embedded_builtin(url) {
            self.builtin_paths(db)
        } else {
            self.paths(db)
        }
    }

    /// Replaces the files and paths under `url`'s scheme.
    fn set_for(
        &self,
        db: &mut dyn InputDb,
        url: &Url,
        files: StringTrie<Url, File>,
        paths: ArcIndexMap<File, Url>,
    ) {
        if is_embedded_builtin(url) {
            self.set_builtin_files(db).to(files);
            self.set_builtin_paths(db).to(paths);
        } else {
            self.set_files(db).to(files);
            self.set_paths(db).to(paths);
        }
    }

    pub(crate) fn set(
        &self,
        db: &mut dyn InputDb,
        url: Url,
        file: File,
    ) -> Result<File, InputIndexError> {
        if let Some(existing_url) = self.get_path(db, file)
            && existing_url != url
        {
            return Err(InputIndexError::CannotReuseInput);
        }

        let files = self.files_for(db, &url).insert(url.clone(), file);
        let mut paths = self.paths_for(db, &url).clone();
        paths.insert(file, url.clone());
        self.set_for(db, &url, files, paths);
        Ok(file)
    }

    pub fn get(&self, db: &dyn InputDb, url: &Url) -> Option<File> {
        self.files_for(db, url).get(url).copied()
    }

    pub fn remove(&self, db: &mut dyn InputDb, url: &Url) -> Option<File> {
        let (files, file) = self.files_for(db, url).remove(url);
        let file = file?;
        let mut paths = self.paths_for(db, url).clone();
        paths.remove(&file);
        self.set_for(db, url, files, paths);
        Some(file)
    }

    #[salsa::tracked]
    pub fn items_at_base(self, db: &dyn InputDb, base: Url) -> StringPrefixView<Url, File> {
        self.files_for(db, &base).view_subtrie(base)
    }

    /// The URL of `file`. A builtin file reads only the builtin paths.
    pub fn get_path(&self, db: &dyn InputDb, file: File) -> Option<Url> {
        self.builtin_paths(db)
            .get(&file)
            .or_else(|| self.paths(db).get(&file))
            .cloned()
    }

    #[salsa::tracked]
    pub fn get_relative_path(self, db: &dyn InputDb, base: Url, file: File) -> Option<Utf8PathBuf> {
        let file_url = self.get_path(db, file)?;
        base.make_relative(&file_url).map(Utf8PathBuf::from)
    }

    /// The file at `url`, created with `initial_content` if there is none.
    /// A builtin file's text is as durable as the builtin paths.
    pub fn touch(&self, db: &mut dyn InputDb, url: Url, initial_content: Option<String>) -> File {
        if let Some(file) = self.get(db, &url) {
            return file;
        }
        let durability = if is_embedded_builtin(&url) {
            Durability::HIGH
        } else {
            Durability::LOW
        };
        let input_file = File::builder(initial_content.unwrap_or_default())
            .durability(durability)
            .new(db);
        self.set(db, url, input_file)
            .expect("Failed to create file")
    }

    pub fn update(&self, db: &mut dyn InputDb, url: Url, content: String) -> File {
        let file = self.touch(db, url, None);
        file.set_text(db).to(content);
        file
    }

    /// Every file, the builtins first.
    pub fn all_files<'db>(&self, db: &'db dyn InputDb) -> impl Iterator<Item = (Url, File)> + 'db {
        self.builtin_files(db).iter().chain(self.files(db).iter())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::define_input_db;

    define_input_db!(TestDatabase);

    #[test]
    fn test_input_index_basic() {
        let mut db = TestDatabase::default();
        let index = db.workspace();

        // Create a file and add it to the index
        let file = File::__new_impl(&db, "test content".to_string());
        let url = Url::parse("file:///test.fe").unwrap();

        index
            .set(&mut db, url.clone(), file)
            .expect("Failed to set file");

        // Test we can look up the file
        let retrieved_file = index.get(&db, &url);
        assert!(retrieved_file.is_some());
        assert_eq!(retrieved_file.unwrap(), file);

        // Test removal
        let removed_file = index.remove(&mut db, &url);
        assert!(removed_file.is_some());
        assert_eq!(removed_file.unwrap(), file);

        // Verify it's gone
        let retrieved_file = index.get(&db, &url);
        assert!(retrieved_file.is_none());
    }

    #[test]
    fn test_input_index_path() {
        let mut db = TestDatabase::default();
        let index = db.workspace();

        // Create a file and add it to the index
        let file = File::__new_impl(&db, "test content".to_string());
        let url = Url::parse("file:///test.fe").unwrap();

        index
            .set(&mut db, url.clone(), file)
            .expect("Failed to set file");

        // Test we can look up the path
        let path = index.get_path(&db, file);
        assert!(path.is_some());
        assert_eq!(path.unwrap(), url);

        // Test we can look up a cloned file's path
        #[allow(clippy::clone_on_copy)]
        let cloned_file = file.clone();
        let path = index.get_path(&db, cloned_file);
        assert!(path.is_some());
        assert_eq!(path.unwrap(), url);
    }

    #[test]
    fn builtin_and_user_files_are_indexed_apart() {
        let mut db = TestDatabase::default();
        let index = db.workspace();
        let builtin_url = Url::parse("builtin-core:/extra.fe").unwrap();
        let user_url = Url::parse("file:///extra.fe").unwrap();
        let builtin = index.touch(&mut db, builtin_url.clone(), None);
        let user = index.touch(&mut db, user_url.clone(), None);

        assert_eq!(index.get_path(&db, builtin), Some(builtin_url.clone()));
        assert_eq!(index.get_path(&db, user), Some(user_url.clone()));
        assert!(matches!(
            index.set(&mut db, user_url, builtin),
            Err(InputIndexError::CannotReuseInput)
        ));
        let urls: Vec<_> = index.all_files(&db).map(|(url, _)| url).collect();
        assert!(urls.contains(&builtin_url));
        assert_eq!(index.remove(&mut db, &builtin_url), Some(builtin));
        assert_eq!(index.get(&db, &builtin_url), None);
    }
}
