use std::rc::Rc;

use fe_analyzer::namespace::items::{self as analyzer_items};

use crate::{db::MirDb, ir::FunctionId};

pub fn mir_lower_struct_all_functions(
    db: &dyn MirDb,
    struct_: analyzer_items::StructId,
) -> Rc<Vec<FunctionId>> {
    struct_
        .all_functions(db.upcast())
        .iter()
        .map(|func| db.mir_lowered_pseudo_monomorphized_func_signature(*func))
        .collect::<Vec<_>>()
        .into()
}
