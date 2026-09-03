use common::origin::{
    OriginExportKey, OriginExportLocalKey, OriginExportOwnerKey, OriginKey, OriginKeyTextError,
    validate_origin_key_text,
};
use cranelift_entity::EntityRef;
use salsa::Update;

use crate::hir_def::{Body, ExprId, StmtId};

pub const HIR_EXPR_EXPORT_KIND: &str = "hir.expr";
pub const HIR_STMT_EXPORT_KIND: &str = "hir.stmt";

/// Stable export owner key for a HIR body.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub struct HirOriginBodyOwnerKey(String);

impl HirOriginBodyOwnerKey {
    pub fn new(value: impl Into<String>) -> Self {
        Self::try_new(value).unwrap_or_else(|err| panic!("{err}"))
    }

    pub fn try_new(value: impl Into<String>) -> Result<Self, OriginKeyTextError> {
        let value = value.into();
        validate_origin_key_text("HIR origin body owner key", &value)?;
        Ok(Self(value))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl OriginExportOwnerKey for HirOriginBodyOwnerKey {
    fn as_str(&self) -> &str {
        &self.0
    }
}

struct HirExprOriginLocalKey(ExprId);

impl OriginExportLocalKey for HirExprOriginLocalKey {
    fn to_export_local_key(&self) -> String {
        self.0.index().to_string()
    }
}

struct HirStmtOriginLocalKey(StmtId);

impl OriginExportLocalKey for HirStmtOriginLocalKey {
    fn to_export_local_key(&self) -> String {
        self.0.index().to_string()
    }
}

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

    pub fn export_key(self, stable_body_key: &HirOriginBodyOwnerKey) -> OriginExportKey {
        OriginExportKey::new(
            HIR_EXPR_EXPORT_KIND,
            stable_body_key,
            &HirExprOriginLocalKey(self.expr()),
        )
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

    pub fn export_key(self, stable_body_key: &HirOriginBodyOwnerKey) -> OriginExportKey {
        OriginExportKey::new(
            HIR_STMT_EXPORT_KIND,
            stable_body_key,
            &HirStmtOriginLocalKey(self.stmt()),
        )
    }
}
