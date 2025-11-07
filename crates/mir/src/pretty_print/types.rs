use std::fmt::{self, Write};

use crate::{
    db::MirDb,
    ir::{function::BodyDataStore, TypeId},
};

use super::PrettyPrint;

impl PrettyPrint for TypeId {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        _store: &BodyDataStore,
        w: &mut W,
    ) -> fmt::Result {
        self.print(db, w)
    }
}
