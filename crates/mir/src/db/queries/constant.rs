use std::rc::Rc;

use fe_analyzer::namespace::items as analyzer_items;

use crate::{
    db::MirDb,
    ir::{Constant, ConstantId, SourceInfo, TypeId},
};

pub fn mir_lowered_constant(
    db: &dyn MirDb,
    analyzer_const: analyzer_items::ModuleConstantId,
) -> ConstantId {
    let name = analyzer_const.name(db.upcast());
    let value = analyzer_const.constant_value(db.upcast()).unwrap();
    let ty = analyzer_const.typ(db.upcast()).unwrap();
    let module_id = analyzer_const.module(db.upcast());
    let span = analyzer_const.span(db.upcast());
    let id = analyzer_const.node_id(db.upcast());

    let ty = db.mir_lowered_type(ty);
    let source = SourceInfo { span, id };

    let constant = Constant {
        name,
        value: value.into(),
        ty,
        module_id,
        source,
    };

    db.mir_intern_const(constant.into())
}

impl ConstantId {
    pub fn data(self, db: &dyn MirDb) -> Rc<Constant> {
        db.lookup_mir_intern_const(self)
    }

    pub fn ty(self, db: &dyn MirDb) -> TypeId {
        self.data(db).ty
    }
}
