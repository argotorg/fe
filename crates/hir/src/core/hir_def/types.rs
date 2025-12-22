use super::{Body, IdentId, Partial, PathId};
use crate::HirDb;

#[salsa::interned]
#[derive(Debug)]
pub struct TypeId<'db> {
    #[return_ref]
    pub data: TypeKind<'db>,
}

impl<'db> TypeId<'db> {
    pub fn is_self_ty(self, db: &dyn HirDb) -> bool {
        if let TypeKind::Path(path) = self.data(db) {
            (|| Some(path.to_opt()?.as_ident(db)?.data(db) == "Self"))().unwrap_or(false)
        } else {
            false
        }
    }

    pub fn fallback_self_ty(db: &'db dyn HirDb) -> Self {
        Self::new(
            db,
            TypeKind::Path(Partial::Present(PathId::from_ident(
                db,
                IdentId::make_self_ty(db),
            ))),
        )
    }

    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let print_ty = |t: &Partial<TypeId>| {
            t.to_opt()
                .map_or("<missing>".into(), |t| t.pretty_print(db))
        };

        match self.data(db) {
            TypeKind::Ptr(t) => format!("*{}", print_ty(t)),
            TypeKind::Path(p) => p
                .to_opt()
                .map_or_else(|| "<missing>".into(), |p| p.pretty_print(db)),
            TypeKind::Tuple(tup) => {
                let elems = tup
                    .data(db)
                    .iter()
                    .map(print_ty)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", elems)
            }
            TypeKind::Array(t, len_body) => {
                let elem_ty = print_ty(t);
                let len_str = len_body
                    .to_opt()
                    .map(|body| {
                        use crate::hir_def::{Expr, LitKind};
                        // Try to get the body expression and print it
                        let expr_id = body.expr(db);
                        match body.exprs(db).get(expr_id).and_then(|e| e.clone().to_opt()) {
                            Some(Expr::Lit(LitKind::Int(int_id))) => int_id.data(db).to_string(),
                            _ => "<expr>".into(),
                        }
                    })
                    .unwrap_or_else(|| "<missing>".into());
                format!("[{}; {}]", elem_ty, len_str)
            }
            TypeKind::Never => "!".into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind<'db> {
    Ptr(Partial<TypeId<'db>>),
    Path(Partial<PathId<'db>>),
    Tuple(TupleTypeId<'db>),
    /// The first `TypeId` is the element type, the second `Body` is the length.
    Array(Partial<TypeId<'db>>, Partial<Body<'db>>),
    Never,
}

#[salsa::interned]
#[derive(Debug)]
pub struct TupleTypeId<'db> {
    #[return_ref]
    pub data: Vec<Partial<TypeId<'db>>>,
}

impl<'db> TupleTypeId<'db> {
    pub fn to_ty(self, db: &'db dyn HirDb) -> TypeId<'db> {
        TypeId::new(db, TypeKind::Tuple(self))
    }

    pub fn len(self, db: &dyn HirDb) -> usize {
        self.data(db).len()
    }
}
