use std::fmt;

use crate::{db::MirDb, ir::function::BodyDataStore};

mod inst;
mod types;
mod value;

pub trait PrettyPrint {
    fn pretty_print<W: fmt::Write>(
        &self,
        db: &dyn MirDb,
        store: &BodyDataStore,
        w: &mut W,
    ) -> fmt::Result;

    fn pretty_string(&self, db: &dyn MirDb, store: &BodyDataStore) -> String {
        let mut s = String::new();
        self.pretty_print(db, store, &mut s).unwrap();
        s
    }
}
