use super::{Body, IdentId, Partial, PathId};
use crate::{
    HirDb,
    hir_def::{GenericParamOwner, TypeId},
};

#[salsa::interned]
#[derive(Debug)]
pub struct GenericArgListId<'db> {
    #[return_ref]
    pub data: Vec<GenericArg<'db>>,
    pub is_given: bool,
}

impl<'db> GenericArgListId<'db> {
    pub fn none(db: &'db dyn HirDb) -> Self {
        Self::new(db, vec![], false)
    }

    pub fn given(db: &'db dyn HirDb, data: Vec<GenericArg<'db>>) -> Self {
        Self::new(db, data, true)
    }

    pub fn given1_type(db: &'db dyn HirDb, ty: TypeId<'db>) -> Self {
        Self::given_types(db, [ty])
    }

    pub fn given_types(db: &'db dyn HirDb, tys: impl IntoIterator<Item = TypeId<'db>>) -> Self {
        Self::given(
            db,
            tys.into_iter()
                .map(|ty| {
                    GenericArg::Type(TypeGenericArg {
                        ty: Partial::Present(ty),
                    })
                })
                .collect(),
        )
    }

    pub fn len(self, db: &dyn HirDb) -> usize {
        self.data(db).len()
    }

    pub fn is_empty(self, db: &dyn HirDb) -> bool {
        self.data(db).is_empty()
    }

    pub fn pretty_print(self, db: &dyn HirDb) -> String {
        fn space_adjacent_angles(s: &str) -> String {
            let mut out = String::with_capacity(s.len());
            let mut prev: Option<char> = None;
            for ch in s.chars() {
                if matches!((prev, ch), (Some('<'), '<') | (Some('>'), '>')) {
                    out.push(' ');
                }
                out.push(ch);
                prev = Some(ch);
            }
            out
        }

        if !self.is_given(db) {
            "".into()
        } else {
            space_adjacent_angles(&format!(
                "<{}>",
                self.data(db)
                    .iter()
                    .map(|p| match p {
                        GenericArg::Const(c) => c.value.pretty_print(db),
                        GenericArg::Type(t) => {
                            t.ty.to_opt()
                                .map_or_else(|| "<missing>".into(), |t| t.pretty_print(db))
                        }
                        GenericArg::AssocType(a) => {
                            let name = a
                                .name
                                .to_opt()
                                .map_or_else(|| "<missing>".into(), |n| n.data(db).to_string());
                            let ty =
                                a.ty.to_opt()
                                    .map_or_else(|| "<missing>".into(), |t| t.pretty_print(db));
                            format!("{name} = {ty}")
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
        }
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct GenericParamListId<'db> {
    #[return_ref]
    pub data: Vec<GenericParam<'db>>,
}

impl GenericParamListId<'_> {
    pub fn len(&self, db: &dyn HirDb) -> usize {
        self.data(db).len()
    }

    /// Whether, as the parameters of an associated type, this list has
    /// something associated types do not support: a const parameter, or a
    /// default.
    pub fn has_unsupported_assoc_params(self, db: &dyn HirDb) -> bool {
        self.data(db).iter().any(|param| match param {
            GenericParam::Type(param) => param.default_ty.is_some(),
            GenericParam::Const(_) => true,
        })
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct FuncParamListId<'db> {
    #[return_ref]
    pub data: Vec<FuncParam<'db>>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct WhereClauseId<'db> {
    /// The clause's predicates, in source order. A predicate's position
    /// here is its index for the clause's spans.
    #[return_ref]
    pub predicates: Vec<WhereClausePredicate<'db>>,
}

impl<'db> WhereClauseId<'db> {
    /// The type bound predicates, each with its position among all the
    /// clause's predicates.
    pub fn type_predicates(
        self,
        db: &'db dyn HirDb,
    ) -> impl Iterator<Item = (usize, &'db WherePredicate<'db>)> + 'db {
        self.predicates(db)
            .iter()
            .enumerate()
            .filter_map(|(idx, predicate)| match predicate {
                WhereClausePredicate::Type(predicate) => Some((idx, predicate)),
                WhereClausePredicate::Const(_) => None,
            })
    }

    /// The const conditions, in source order.
    pub fn const_predicates(self, db: &'db dyn HirDb) -> &'db [Body<'db>] {
        where_clause_const_predicates(db, self)
    }
}

#[salsa::tracked(return_ref)]
fn where_clause_const_predicates<'db>(
    db: &'db dyn HirDb,
    clause: WhereClauseId<'db>,
) -> Vec<Body<'db>> {
    clause
        .predicates(db)
        .iter()
        .filter_map(|predicate| match predicate {
            WhereClausePredicate::Const(body) => Some(*body),
            WhereClausePredicate::Type(_) => None,
        })
        .collect()
}

/// A predicate of a `where` clause.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WhereClausePredicate<'db> {
    /// A trait or kind bound on a type, `T: Trait`.
    Type(WherePredicate<'db>),
    /// A boolean const condition, `N > 0`.
    Const(Body<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum GenericParam<'db> {
    Type(TypeGenericParam<'db>),
    Const(ConstGenericParam<'db>),
}

impl<'db> GenericParam<'db> {
    pub fn name(&self) -> Partial<IdentId<'db>> {
        match self {
            Self::Type(ty) => ty.name,
            Self::Const(c) => c.name,
        }
    }

    pub fn has_default(&self) -> bool {
        match self {
            Self::Type(ty) => ty.default_ty.is_some(),
            Self::Const(c) => c.default.is_some(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub struct GenericParamView<'db> {
    pub param: &'db GenericParam<'db>,
    pub owner: GenericParamOwner<'db>,
    pub idx: usize,
}

impl<'db> GenericParamView<'db> {
    pub fn name(&self) -> Partial<IdentId<'db>> {
        match self.param {
            GenericParam::Type(ty) => ty.name,
            GenericParam::Const(c) => c.name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenericParam<'db> {
    pub name: Partial<IdentId<'db>>,
    pub bounds: Vec<TypeBound<'db>>,
    pub default_ty: Option<TypeId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstGenericParam<'db> {
    pub name: Partial<IdentId<'db>>,
    pub ty: Partial<TypeId<'db>>,
    pub default: Option<ConstGenericArgValue<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From)]
pub enum GenericArg<'db> {
    Type(TypeGenericArg<'db>),
    Const(ConstGenericArg<'db>),
    AssocType(AssocTypeGenericArg<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeGenericArg<'db> {
    pub ty: Partial<TypeId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstGenericArg<'db> {
    pub value: ConstGenericArgValue<'db>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum ConstGenericArgValue<'db> {
    Expr(Partial<Body<'db>>),
    Hole,
}

impl<'db> ConstGenericArgValue<'db> {
    pub fn pretty_print(&self, db: &dyn HirDb) -> String {
        match self {
            Self::Expr(body) => body
                .to_opt()
                .map_or_else(|| "<missing>".into(), |b| b.pretty_print(db)),
            Self::Hole => "_".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssocTypeGenericArg<'db> {
    pub name: Partial<IdentId<'db>>,
    pub ty: Partial<TypeId<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum FuncParamMode {
    /// Default `x: T`: readable but cannot be moved-out.
    View,
    /// `x: own T`: callee takes ownership of the argument.
    Own,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncParam<'db> {
    pub mode: FuncParamMode,
    pub is_mut: bool,
    pub has_ref_prefix: bool,
    pub has_own_prefix: bool,
    pub is_label_suppressed: bool,
    pub name: Partial<FuncParamName<'db>>,
    pub ty: Partial<TypeId<'db>>,

    /// `true` if this parameter is `self` and the type is not specified.
    /// `ty` should have `Self` type without any type arguments.
    pub self_ty_fallback: bool,
}

impl<'db> FuncParam<'db> {
    pub fn label_eagerly(&self) -> Option<IdentId<'db>> {
        (!self.is_label_suppressed)
            .then(|| self.name.to_opt())
            .flatten()
            .and_then(|name| match name {
                FuncParamName::Ident(ident) => Some(ident),
                FuncParamName::Underscore => None,
            })
    }

    pub fn name(&self) -> Option<IdentId<'db>> {
        match self.name.to_opt()? {
            FuncParamName::Ident(name) => Some(name),
            _ => None,
        }
    }

    pub fn is_self_param(&self, db: &dyn HirDb) -> bool {
        self.name.to_opt().is_some_and(|name| name.is_self(db))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WherePredicate<'db> {
    pub ty: Partial<TypeId<'db>>,
    pub bounds: Vec<TypeBound<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FuncParamName<'db> {
    Ident(IdentId<'db>),
    Underscore,
}

impl<'db> FuncParamName<'db> {
    pub fn ident(db: &'db dyn HirDb, name: &str) -> Self {
        Self::Ident(IdentId::new(db, name))
    }

    pub fn is_self(&self, db: &dyn HirDb) -> bool {
        match self {
            FuncParamName::Ident(id) => id.is_self(db),
            _ => false,
        }
    }

    pub fn pretty_print(&self, db: &dyn HirDb) -> String {
        match self {
            FuncParamName::Ident(name) => name.data(db).to_string(),
            FuncParamName::Underscore => "_".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub enum TypeBound<'db> {
    Trait(TraitRefId<'db>),
    Kind(Partial<KindBound>),
}

#[salsa::interned]
#[derive(Debug)]
pub struct TraitRefId<'db> {
    /// The path to the trait.
    pub path: Partial<PathId<'db>>,
}

impl<'db> TraitRefId<'db> {
    /// Returns the generic arg list of the last segment of the trait ref path
    pub fn generic_args(self, db: &'db dyn HirDb) -> Option<GenericArgListId<'db>> {
        self.path(db).to_opt().map(|path| path.generic_args(db))
    }

    pub fn pretty_print(self, db: &dyn HirDb) -> String {
        self.path(db)
            .to_opt()
            .map_or("<missing>".to_string(), |p| p.pretty_print(db))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum KindBound {
    /// `*`
    Mono,
    /// `* -> *`
    Abs(Partial<Box<KindBound>>, Partial<Box<KindBound>>),
}
