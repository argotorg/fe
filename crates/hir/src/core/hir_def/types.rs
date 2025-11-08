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
            TypeKind::Tuple(tup) => tup
                .data(db)
                .iter()
                .map(print_ty)
                .collect::<Vec<_>>()
                .join(", "),
            TypeKind::Array(t, _) => format!("[{}; {{..}}]", print_ty(t)),
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
