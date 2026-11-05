use super::{Body, IdentId, Partial, PathId};
use crate::{
    HirDb,
    hir_def::{GenericParamOwner, TypeId, TypeKind, TypeMode},
};

#[salsa::interned]
#[derive(Debug)]
pub struct GenericArgListId<'db> {
    pub data: Vec<GenericArg<'db>>,
    #[returns(copy)]
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
                        GenericArg::Const(c) => c
                            .value
                            .to_opt()
                            .map_or_else(|| "<missing>".into(), |b| b.pretty_print(db)),
                        GenericArg::Type(t) => {
                            t.ty.to_opt()
                                .map_or_else(|| "<missing>".into(), |t| t.pretty_print(db))
                        }
                        GenericArg::AssocType(a) => {
                            let name = a
                                .name
                                .to_opt()
                                .map_or_else(|| "<missing>".into(), |n| n.data(db).to_string());
                            let mode = a
                                .mode
                                .map_or_else(String::new, |mode| format!("<{}>", mode.keyword()));
                            let ty =
                                a.ty.to_opt()
                                    .map_or_else(|| "<missing>".into(), |t| t.pretty_print(db));
                            format!("{name}{mode} = {ty}")
                        }
                        GenericArg::Mode(mode) => mode.keyword().to_string(),
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
    pub data: Vec<GenericParam<'db>>,
}

impl GenericParamListId<'_> {
    pub fn len(&self, db: &dyn HirDb) -> usize {
        self.data(db).len()
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct FuncParamListId<'db> {
    pub data: Vec<FuncParam<'db>>,
}

#[salsa::interned]
#[derive(Debug)]
pub struct WhereClauseId<'db> {
    /// The clause's predicates, in source order. A predicate's position
    /// here is its index for the clause's spans.
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

#[salsa::tracked]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum WhereClausePredicate<'db> {
    /// A trait or kind bound on a type, `T: Trait`.
    Type(WherePredicate<'db>),
    /// A boolean const condition, `N > 0`.
    Const(Body<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, salsa::SalsaValue)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct TypeGenericParam<'db> {
    pub name: Partial<IdentId<'db>>,
    pub bounds: Vec<TypeBound<'db>>,
    pub default_ty: Option<TypeId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct ConstGenericParam<'db> {
    pub name: Partial<IdentId<'db>>,
    pub ty: Partial<TypeId<'db>>,
    pub default: Option<Body<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, salsa::SalsaValue)]
pub enum GenericArg<'db> {
    Type(TypeGenericArg<'db>),
    Const(ConstGenericArg<'db>),
    AssocType(AssocTypeGenericArg<'db>),
    /// `ref` or `mut`: the mode an associated shape is named at,
    /// `D::Step<ref>`.
    Mode(TypeMode),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct TypeGenericArg<'db> {
    pub ty: Partial<TypeId<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct ConstGenericArg<'db> {
    pub value: Partial<Body<'db>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct AssocTypeGenericArg<'db> {
    pub name: Partial<IdentId<'db>>,
    /// The mode an associated shape is bound at: `Step<ref> = ref u256`.
    pub mode: Option<TypeMode>,
    pub ty: Partial<TypeId<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum FuncParamMode {
    /// Default `x: T` (or `x: ref T`): the caller's place is read-accessible
    /// for the call.
    View,
    /// `x: mut T`: the caller's place is exclusively accessible for the call.
    Mut,
    /// `x: own T`: callee takes ownership of the argument.
    Own,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct FuncParam<'db> {
    /// The mode at `ref`. A `mut?` parameter, whose `ty` is `mut? T`, is a
    /// view at `ref` and `mut` at `mut`.
    pub mode: FuncParamMode,
    pub is_mut: bool,
    pub has_ref_prefix: bool,
    pub has_own_prefix: bool,
    /// `mut` before the name, which only `mut self` writes.
    pub has_mut_prefix: bool,
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
                FuncParamName::Underscore
                | FuncParamName::Destructured(_)
                | FuncParamName::Spread => None,
            })
    }

    /// The parameter's name as written: none for `_` or a pattern.
    pub fn name(&self) -> Option<IdentId<'db>> {
        match self.name.to_opt()? {
            FuncParamName::Ident(name) => Some(name),
            FuncParamName::Underscore | FuncParamName::Destructured(_) | FuncParamName::Spread => {
                None
            }
        }
    }

    /// The name the parameter's binding has in its body: its own, or the
    /// hidden name a destructured parameter's body binds its pattern from.
    pub fn binding_name(&self) -> Option<IdentId<'db>> {
        match self.name.to_opt()? {
            FuncParamName::Ident(name) | FuncParamName::Destructured(name) => Some(name),
            FuncParamName::Underscore | FuncParamName::Spread => None,
        }
    }

    pub fn is_self_param(&self, db: &dyn HirDb) -> bool {
        self.name.to_opt().is_some_and(|name| name.is_self(db))
    }

    /// Whether the parameter is a pattern its body destructures it with.
    pub fn is_destructured(&self) -> bool {
        matches!(self.name.to_opt(), Some(FuncParamName::Destructured(_)))
    }

    /// Whether the parameter is a spread, `..A`.
    pub fn is_spread(&self) -> bool {
        matches!(self.name.to_opt(), Some(FuncParamName::Spread))
    }

    /// Whether the parameter is written `mut?`: `mut? self` or `x: mut? T`.
    pub fn is_maybe_mut(&self, db: &dyn HirDb) -> bool {
        self.ty
            .to_opt()
            .is_some_and(|ty| matches!(ty.data(db), TypeKind::Mode(TypeMode::MaybeMut, _)))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct WherePredicate<'db> {
    pub ty: Partial<TypeId<'db>>,
    pub bounds: Vec<TypeBound<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum FuncParamName<'db> {
    Ident(IdentId<'db>),
    Underscore,
    /// A pattern, `_ (a, b): T`, which the body that binds the parameter
    /// (the function's own, or a closure's) begins by destructuring it with:
    /// `let (a, b) = %1`. The name, which source cannot write, is the
    /// parameter's `%` and its index.
    Destructured(IdentId<'db>),
    /// `..A`: the parameters the shape `A`, the parameter's type, lists, in
    /// its place.
    Spread,
}

impl<'db> FuncParamName<'db> {
    pub fn ident(db: &'db dyn HirDb, name: &str) -> Self {
        Self::Ident(IdentId::new(db, name))
    }

    /// The name of destructured parameter `idx`.
    pub fn destructured(db: &'db dyn HirDb, idx: usize) -> IdentId<'db> {
        IdentId::new(db, format!("%{idx}"))
    }

    /// Whether `path` reads a destructured parameter by its name, as the
    /// `let` that destructures it does. Source cannot write such a path.
    pub fn is_destructured_read(db: &'db dyn HirDb, path: PathId<'db>) -> bool {
        path.as_ident(db)
            .is_some_and(|name| name.data(db).starts_with('%'))
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
            FuncParamName::Underscore | FuncParamName::Destructured(_) => "_".to_string(),
            FuncParamName::Spread => "..".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum TypeBound<'db> {
    /// A trait bound, with the modes it grants the trait's `mut?` methods
    /// at.
    Trait(TraitRefId<'db>, ModeMarker),
    Kind(Partial<KindBound>),
}

/// The mode written before an effect or a trait bound: none, `mut`, or
/// `mut?`, the item's mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum ModeMarker {
    /// `uses (c: Ctx)`, `T: Trait`: read only, or the `ref` modes of a
    /// trait's `mut?` methods.
    None,
    /// `uses (s: mut Storage)`, `T: mut Trait`.
    Mut,
    /// `uses (s: mut? Storage)`, `T: mut? Trait`: `mut` where the item is
    /// used at `mut`.
    MaybeMut,
}

impl ModeMarker {
    /// The marker as written before an effect key or a trait, with its
    /// trailing space.
    pub fn prefix(self) -> &'static str {
        match self {
            Self::None => "",
            Self::Mut => "mut ",
            Self::MaybeMut => "mut? ",
        }
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct TraitRefId<'db> {
    /// The path to the trait.
    #[returns(copy)]
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
    /// A `shape A` parameter's kind: a callable's parameter list.
    Shape,
    /// `* -> *`
    Abs(Partial<Box<KindBound>>, Partial<Box<KindBound>>),
}
