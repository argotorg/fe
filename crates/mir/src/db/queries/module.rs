use std::rc::Rc;

use fe_analyzer::namespace::items::{self as analyzer_items, TypeDef};

use crate::{db::MirDb, ir::FunctionId};

pub fn mir_lower_module_all_functions(
    db: &dyn MirDb,
    module: analyzer_items::ModuleId,
) -> Rc<Vec<FunctionId>> {
    let mut functions = vec![];

    let items = module.all_items(db.upcast());
    items.iter().for_each(|item| match item {
        analyzer_items::Item::Function(func) => {
            functions.push(db.mir_lowered_func_signature(*func))
        }

        analyzer_items::Item::Type(TypeDef::Contract(contract)) => {
            functions.extend_from_slice(&db.mir_lower_contract_all_functions(*contract))
        }

        analyzer_items::Item::Type(TypeDef::Struct(struct_)) => {
            functions.extend_from_slice(&db.mir_lower_struct_all_functions(*struct_))
        }

        analyzer_items::Item::Type(TypeDef::Enum(enum_)) => {
            functions.extend_from_slice(&db.mir_lower_enum_all_functions(*enum_))
        }

        _ => {}
    });

    functions.into()
}
