use crate::{
    hir_def::{
        Contract, Enum, GenericParamOwner, IdentId, ItemKind, Partial, Struct,
        TypeId as HirTyId, scope_graph::ScopeId,
    },
    span::DynLazySpan,
};
use common::ingot::Ingot;
use salsa::Update;

use super::{
    binder::Binder,
    trait_resolution::constraint::collect_adt_constraints,
    ty_def::{InvalidCause, TyId},
    ty_lower::{GenericParamTypeSet, lower_hir_ty},
};
use crate::analysis::HirAnalysisDb;

/// This struct represents a field of an ADT. If the ADT is an enum, this
/// represents a variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtField<'db> {
    /// Fields of the variant.
    /// If the adt is an struct or contract,
    /// the length of the vector is always 1.
    ///
    /// To allow recursive types, the type of the field is represented as a HIR
    /// type and.
    tys: Vec<Partial<HirTyId<'db>>>,

    scope: ScopeId<'db>,
}
impl<'db> AdtField<'db> {
    pub fn ty(&self, db: &'db dyn HirAnalysisDb, i: usize) -> Binder<TyId<'db>> {
        // Get ADT definition from scope to determine appropriate assumptions
        let assumptions = match self.scope {
            ScopeId::Item(ItemKind::Struct(struct_)) => {
                collect_adt_constraints(db, struct_.into()).instantiate_identity()
            }
            ScopeId::Item(ItemKind::Enum(enum_)) => {
                collect_adt_constraints(db, enum_.into()).instantiate_identity()
            }
            ScopeId::Item(ItemKind::Contract(contract)) => {
                collect_adt_constraints(db, contract.into()).instantiate_identity()
            }
            _ => unreachable!(),
        };

        let ty = if let Some(ty) = self.tys[i].to_opt() {
            lower_hir_ty(db, ty, self.scope, assumptions)
        } else {
            TyId::invalid(db, InvalidCause::ParseError)
        };

        Binder::bind(ty)
    }

    /// Iterates all fields types of the `field`.
    pub fn iter_types<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<TyId<'db>>> + 'a {
        (0..self.num_types()).map(|i| self.ty(db, i))
    }

    pub fn num_types(&self) -> usize {
        self.tys.len()
    }

    pub(crate) fn new(tys: Vec<Partial<HirTyId<'db>>>, scope: ScopeId<'db>) -> Self {
        Self { tys, scope }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, derive_more::From, salsa::Supertype, Update)]
pub enum AdtRef<'db> {
    Enum(Enum<'db>),
    Struct(Struct<'db>),
    Contract(Contract<'db>),
}

impl<'db> AdtRef<'db> {
    pub fn try_from_item(item: ItemKind<'db>) -> Option<Self> {
        match item {
            ItemKind::Enum(x) => Some(x.into()),
            ItemKind::Struct(x) => Some(x.into()),
            ItemKind::Contract(x) => Some(x.into()),
            _ => None,
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            Self::Enum(e) => e.scope(),
            Self::Struct(s) => s.scope(),
            Self::Contract(c) => c.scope(),
        }
    }

    pub fn as_item(self) -> ItemKind<'db> {
        match self {
            AdtRef::Enum(e) => e.into(),
            AdtRef::Struct(s) => s.into(),
            AdtRef::Contract(c) => c.into(),
        }
    }

    pub fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        match self {
            AdtRef::Enum(e) => e.name(db),
            AdtRef::Struct(s) => s.name(db),
            AdtRef::Contract(c) => c.name(db),
        }
        .to_opt()
    }

    pub fn kind_name(self) -> &'static str {
        self.as_item().kind_name()
    }

    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> DynLazySpan<'db> {
        self.scope()
            .name_span(db)
            .unwrap_or_else(DynLazySpan::invalid)
    }

    pub(crate) fn generic_owner(self) -> Option<GenericParamOwner<'db>> {
        match self {
            AdtRef::Enum(e) => Some(e.into()),
            AdtRef::Struct(s) => Some(s.into()),
            AdtRef::Contract(_) => None,
        }
    }

    // Semantic methods that delegate to HIR items directly.
    // These provide a unified interface across Struct/Enum/Contract.

    pub(crate) fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).params(db)
    }

    pub(crate) fn original_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).explicit_params(db)
    }

    pub(crate) fn ingot(self, db: &'db dyn HirAnalysisDb) -> Ingot<'db> {
        match self {
            AdtRef::Enum(e) => e.top_mod(db).ingot(db),
            AdtRef::Struct(s) => s.top_mod(db).ingot(db),
            AdtRef::Contract(c) => c.top_mod(db).ingot(db),
        }
    }

    pub(crate) fn is_struct(self) -> bool {
        matches!(self, AdtRef::Struct(_))
    }

    pub(crate) fn fields(self, db: &'db dyn HirAnalysisDb) -> &'db [AdtField<'db>] {
        match self {
            AdtRef::Enum(e) => e.ty_fields(db),
            AdtRef::Struct(s) => std::slice::from_ref(s.ty_fields(db)),
            AdtRef::Contract(c) => std::slice::from_ref(c.ty_fields(db)),
        }
    }

    pub(crate) fn param_set(self, db: &'db dyn HirAnalysisDb) -> &'db GenericParamTypeSet<'db> {
        match self {
            AdtRef::Enum(e) => e.ty_param_set(db),
            AdtRef::Struct(s) => s.ty_param_set(db),
            AdtRef::Contract(c) => c.ty_param_set(db),
        }
    }

    pub(crate) fn variant_ty_span(
        self,
        db: &'db dyn HirAnalysisDb,
        field_idx: usize,
        ty_idx: usize,
    ) -> DynLazySpan<'db> {
        use crate::hir_def::VariantKind;
        match self {
            AdtRef::Enum(e) => {
                let span = e.variant_span(field_idx);
                match e.variants(db).data(db)[field_idx].kind {
                    VariantKind::Tuple(_) => span.tuple_type().elem_ty(ty_idx).into(),
                    VariantKind::Record(_) => span.fields().field(ty_idx).ty().into(),
                    VariantKind::Unit => unreachable!(),
                }
            }

            AdtRef::Struct(s) => s.span().fields().field(field_idx).ty().into(),

            AdtRef::Contract(c) => c.span().fields().field(field_idx).ty().into(),
        }
    }

    pub(crate) fn as_generic_param_owner(self) -> Option<GenericParamOwner<'db>> {
        self.generic_owner()
    }
}

impl<'db> super::ty_def::HasKind for AdtRef<'db> {
    fn kind(&self, db: &dyn HirAnalysisDb) -> super::ty_def::Kind {
        use super::ty_def::Kind;
        let mut kind = Kind::Star;
        for param in self.params(db).iter().rev() {
            kind = Kind::Abs(Box::new((param.kind(db).clone(), kind)));
        }
        kind
    }
}
