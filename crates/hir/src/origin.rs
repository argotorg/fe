use common::origin::OriginKey;
use salsa::Update;

use crate::hir_def::{Body, ExprId, StmtId};

/// Origin key for a HIR expression. The expression ID is only meaningful inside
/// its owning HIR body.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct HirExprOrigin<'db> {
    key: OriginKey<Body<'db>, ExprId>,
}

impl<'db> HirExprOrigin<'db> {
    pub const fn new(body: Body<'db>, expr: ExprId) -> Self {
        Self {
            key: OriginKey::new(body, expr),
        }
    }

    pub fn body(self) -> Body<'db> {
        self.key.into_parts().0
    }

    pub fn expr(self) -> ExprId {
        self.key.into_parts().1
    }
}

/// Origin key for a HIR statement. Statement and expression IDs are
/// intentionally distinct origin types even though both are body-local integers.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
pub struct HirStmtOrigin<'db> {
    key: OriginKey<Body<'db>, StmtId>,
}

impl<'db> HirStmtOrigin<'db> {
    pub const fn new(body: Body<'db>, stmt: StmtId) -> Self {
        Self {
            key: OriginKey::new(body, stmt),
        }
    }

    pub fn body(self) -> Body<'db> {
        self.key.into_parts().0
    }

    pub fn stmt(self) -> StmtId {
        self.key.into_parts().1
    }
}
