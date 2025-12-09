use camino::Utf8PathBuf;
use rust_embed::Embed;
use url::Url;

use crate::{
    InputDb,
    ingot::{Ingot, IngotBaseUrl},
};

pub static BUILTIN_CORE_BASE_URL: &str = "builtin-core:///";
pub static BUILTIN_STD_BASE_URL: &str = "builtin-std:///";

fn initialize_builtin<E: Embed>(db: &mut dyn InputDb, base_url: &str) {
    let base = Url::parse(base_url).unwrap();

    for (path, contents) in E::iter().filter_map(|path| {
        E::get(&path).map(|content| {
            let contents = String::from_utf8(content.data.into_owned()).unwrap();
            (Utf8PathBuf::from(path.to_string()), contents)
        })
    }) {
        base.touch(db, path, contents.into());
    }
}

#[derive(Embed)]
#[folder = "../../library/core"]
pub struct Core;

pub trait HasBuiltinCore: InputDb {
    fn initialize_builtin_core(&mut self);
    fn builtin_core(&self) -> Ingot<'_>;
}

impl<T: InputDb> HasBuiltinCore for T {
    fn initialize_builtin_core(&mut self) {
        initialize_builtin::<Core>(self, BUILTIN_CORE_BASE_URL);
    }

    fn builtin_core(&self) -> Ingot<'_> {
        let core = self
            .workspace()
            .containing_ingot(self, Url::parse(BUILTIN_CORE_BASE_URL).unwrap());
        core.expect("Built-in core ingot failed to initialize")
    }
}

#[derive(Embed)]
#[folder = "../../library/std"]
pub struct Std;

pub trait HasBuiltinStd: InputDb {
    fn initialize_builtin_std(&mut self);
    fn builtin_std(&self) -> Ingot<'_>;
}

impl<T: InputDb> HasBuiltinStd for T {
    fn initialize_builtin_std(&mut self) {
        initialize_builtin::<Std>(self, BUILTIN_STD_BASE_URL);
    }

    fn builtin_std(&self) -> Ingot<'_> {
        let std = self
            .workspace()
            .containing_ingot(self, Url::parse(BUILTIN_STD_BASE_URL).unwrap());
        std.expect("Built-in std ingot failed to initialize")
    }
}
