use std::{collections::BTreeMap, rc::Rc};

use fe_analyzer::{
    display::Displayable,
    namespace::{items as analyzer_items, items::Item, types as analyzer_types},
};

use smol_str::SmolStr;

use crate::{
    db::MirDb,
    ir::{self, function::Linkage, FunctionSignature, TypeId},
    lower::function::{lower_func_body, lower_func_signature, lower_monomorphized_func_signature},
};

pub fn mir_lowered_func_signature(
    db: &dyn MirDb,
    analyzer_func: analyzer_items::FunctionId,
) -> ir::FunctionId {
    lower_func_signature(db, analyzer_func)
}

pub fn mir_lowered_monomorphized_func_signature(
    db: &dyn MirDb,
    analyzer_func: analyzer_items::FunctionId,
    resolved_generics: BTreeMap<SmolStr, analyzer_types::TypeId>,
) -> ir::FunctionId {
    lower_monomorphized_func_signature(db, analyzer_func, resolved_generics)
}

/// Generate MIR function and monomorphize generic parameters as if they were
/// called with unit type NOTE: THIS SHOULD ONLY BE USED IN TEST CODE
pub fn mir_lowered_pseudo_monomorphized_func_signature(
    db: &dyn MirDb,
    analyzer_func: analyzer_items::FunctionId,
) -> ir::FunctionId {
    let resolved_generics = analyzer_func
        .sig(db.upcast())
        .generic_params(db.upcast())
        .iter()
        .map(|generic| (generic.name(), analyzer_types::TypeId::unit(db.upcast())))
        .collect::<BTreeMap<_, _>>();
    lower_monomorphized_func_signature(db, analyzer_func, resolved_generics)
}

pub fn mir_lowered_func_body(db: &dyn MirDb, func: ir::FunctionId) -> Rc<ir::FunctionBody> {
    lower_func_body(db, func)
}

impl ir::FunctionId {
    pub fn signature(self, db: &dyn MirDb) -> Rc<FunctionSignature> {
        db.lookup_mir_intern_function(self)
    }

    pub fn return_type(self, db: &dyn MirDb) -> Option<TypeId> {
        self.signature(db).return_type
    }

    pub fn linkage(self, db: &dyn MirDb) -> Linkage {
        self.signature(db).linkage
    }

    pub fn analyzer_func(self, db: &dyn MirDb) -> analyzer_items::FunctionId {
        self.signature(db).analyzer_func_id
    }

    pub fn body(self, db: &dyn MirDb) -> Rc<ir::FunctionBody> {
        db.mir_lowered_func_body(self)
    }

    pub fn module(self, db: &dyn MirDb) -> analyzer_items::ModuleId {
        let analyzer_func = self.analyzer_func(db);
        analyzer_func.module(db.upcast())
    }

    pub fn is_contract_init(self, db: &dyn MirDb) -> bool {
        self.analyzer_func(db)
            .data(db.upcast())
            .sig
            .is_constructor(db.upcast())
    }

    /// Returns a type suffix if a generic function was monomorphized
    pub fn type_suffix(&self, db: &dyn MirDb) -> SmolStr {
        self.signature(db)
            .resolved_generics
            .values()
            .fold(String::new(), |acc, param| {
                format!("{}_{}", acc, param.display(db.upcast()))
            })
            .into()
    }

    pub fn name(&self, db: &dyn MirDb) -> SmolStr {
        let analyzer_func = self.analyzer_func(db);
        analyzer_func.name(db.upcast())
    }

    /// Returns `class_name::fn_name` if a function is a method else `fn_name`.
    pub fn debug_name(self, db: &dyn MirDb) -> SmolStr {
        let analyzer_func = self.analyzer_func(db);
        let func_name = format!(
            "{}{}",
            analyzer_func.name(db.upcast()),
            self.type_suffix(db)
        );

        match analyzer_func.sig(db.upcast()).self_item(db.upcast()) {
            Some(Item::Impl(id)) => {
                let class_name = format!(
                    "<{} as {}>",
                    id.receiver(db.upcast()).display(db.upcast()),
                    id.trait_id(db.upcast()).name(db.upcast())
                );
                format!("{class_name}::{func_name}").into()
            }
            Some(class) => {
                let class_name = class.name(db.upcast());
                format!("{class_name}::{func_name}").into()
            }
            _ => func_name.into(),
        }
    }

    pub fn returns_aggregate(self, db: &dyn MirDb) -> bool {
        self.return_type(db)
            .map(|ty| ty.is_aggregate(db))
            .unwrap_or_default()
    }
}
