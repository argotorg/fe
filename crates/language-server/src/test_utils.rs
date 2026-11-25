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

    let had_diagnostics = init_ingot(db, &ingot_url);
    if had_diagnostics {
        panic!("Failed to resolve test ingot at {ingot_dir:?}: init produced diagnostics");
    }
}

/// A package of two files where `alpha.fe` asks, with a header, for a function
/// that the provider in `beta.fe` writes in its quotation, and the view that
/// generation prepares for it. `cursor` is an offset of `alpha.fe` that lies in
/// a comment there and in the quotation of `beta.fe`.
#[cfg(test)]
pub struct PreparedPackage {
    pub _dir: tempfile::TempDir,
    /// The authored database, as the editor holds it.
    pub authored: DriverDataBase,
    /// The view that generation prepares.
    pub db: DriverDataBase,
    pub alpha_url: Url,
    pub alpha_text: String,
    pub cursor: parser::TextSize,
}

/// Prepare the package of `PreparedPackage`; `generated_fn` is the function
/// the provider quotes.
#[cfg(test)]
pub fn prepared_package(generated_fn: &str) -> PreparedPackage {
    let dir = tempfile::tempdir().expect("tempdir");
    let beta_text = format!(
        "use core::derive::DeriveInherent\n\
use core::meta::{{Code, Item, Record}}\n\
pub struct Make {{}}\n\
impl DeriveInherent for Make {{\n\
    const fn derive(_ record: Record) -> Code<Item> {{\n\
        quote {{ {generated_fn} }}\n\
    }}\n\
}}\n"
    );
    let cursor = beta_text.find("generated").expect("generated function") + 3;
    let alpha_text = format!(
        "// {}\nuse ingot::beta::Make\npub struct Pair {{ first: u256 }}\nimpl Pair with (Make)\n",
        "p".repeat(cursor + 40)
    );
    let src = dir.path().join("src");
    std::fs::create_dir_all(&src).expect("src dir");
    std::fs::write(
        dir.path().join("fe.toml"),
        "[ingot]\nname = \"app\"\nversion = \"0.1.0\"\n",
    )
    .expect("fe.toml");
    std::fs::write(src.join("lib.fe"), "").expect("lib.fe");
    std::fs::write(src.join("alpha.fe"), &alpha_text).expect("alpha.fe");
    std::fs::write(src.join("beta.fe"), &beta_text).expect("beta.fe");

    let mut db = DriverDataBase::default();
    load_ingot_from_directory(&mut db, dir.path());
    let ingot_url = Url::from_directory_path(dir.path()).expect("directory url");
    let prepared = driver::generation::prepare_ingots(&db, std::slice::from_ref(&ingot_url))
        .expect("generation succeeds")
        .expect("the package requests generation");
    let alpha_url = Url::from_file_path(src.join("alpha.fe")).expect("file url");
    PreparedPackage {
        _dir: dir,
        authored: db,
        db: prepared,
        alpha_url,
        alpha_text,
        cursor: parser::TextSize::from(cursor as u32),
    }
}
