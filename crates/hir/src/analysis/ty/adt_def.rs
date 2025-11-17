use crate::hir_def::{
    self, Contract, Enum, FieldDefListId, GenericParamOwner, IdentId, ItemKind, Partial, Struct,
    TypeId as HirTyId, VariantDefListId, VariantKind, scope_graph::ScopeId,
};
use crate::span::DynLazySpan;
use common::ingot::Ingot;
use salsa::Update;

use super::{
    binder::Binder,
    ty_def::{InvalidCause, TyId},
    ty_lower::{GenericParamTypeSet, collect_generic_params},
};
use crate::analysis::HirAnalysisDb;

/// Lower HIR ADT definition(`struct/enum/contract`) to [`AdtDef`].
#[salsa::tracked]
pub fn lower_adt<'db>(db: &'db dyn HirAnalysisDb, adt: AdtRef<'db>) -> AdtDef<'db> {
    let scope = adt.scope();

    let (params, variants) = match adt {
        AdtRef::Contract(c) => {
            let tys = c.field_tys(db);
            (
                GenericParamTypeSet::empty(db, scope),
                vec![AdtField::new(tys)],
            )
        }
        AdtRef::Struct(s) => {
            let tys = s.field_tys(db);
            (
                collect_generic_params(db, s.into()),
                vec![AdtField::new(tys)],
            )
        }
        AdtRef::Enum(e) => (
            collect_generic_params(db, e.into()),
            collect_enum_variant_types(db, e, scope),
        ),
    };

    AdtDef::new(db, adt, params, variants)
}

fn collect_enum_variant_types<'db>(
    db: &'db dyn HirAnalysisDb,
    e: Enum<'db>,
    scope: ScopeId<'db>,
) -> Vec<AdtField<'db>> {
    e.variants(db)
        .map(|variant| AdtField::new(variant.field_tys(db)))
        .collect()
}

/// Represents a ADT type definition.
#[salsa::tracked]
#[derive(Debug)]
pub struct AdtDef<'db> {
    pub adt_ref: AdtRef<'db>,

    /// Type parameters of the ADT.
    #[return_ref]
    pub param_set: GenericParamTypeSet<'db>,

    /// Fields of the ADT, if the ADT is an enum, this represents variants.
    /// Otherwise, `fields[0]` represents all fields of the struct.
    #[return_ref]
    pub fields: Vec<AdtField<'db>>,
}

impl<'db> AdtDef<'db> {
    pub(crate) fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        self.adt_ref(db).name(db)
    }

    pub fn name_span(self, db: &'db dyn HirAnalysisDb) -> DynLazySpan<'db> {
        self.adt_ref(db).name_span(db)
    }

    pub(crate) fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).params(db)
    }

    pub(crate) fn original_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set(db).explicit_params(db)
    }

    pub(crate) fn is_struct(self, db: &dyn HirAnalysisDb) -> bool {
        matches!(self.adt_ref(db), AdtRef::Struct(_))
    }

    pub fn scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        self.adt_ref(db).scope()
    }

    pub(crate) fn variant_ty_span(
        self,
        db: &'db dyn HirAnalysisDb,
        field_idx: usize,
        ty_idx: usize,
    ) -> DynLazySpan<'db> {
        match self.adt_ref(db) {
            AdtRef::Enum(e) => {
                let span = e.variant_span(field_idx);
                match e
                    .variants(db)
                    .nth(field_idx)
                    .expect("variant not found")
                    .kind(db)
                {
                    VariantKind::Tuple(_) => span.tuple_type().elem_ty(ty_idx).into(),
                    VariantKind::Record(_) => span.fields().field(ty_idx).ty().into(),
                    VariantKind::Unit => unreachable!(),
                }
            }

            AdtRef::Struct(s) => s.span().fields().field(field_idx).ty().into(),

            AdtRef::Contract(c) => c.span().fields().field(field_idx).ty().into(),
        }
    }

    pub(crate) fn ingot(self, db: &'db dyn HirAnalysisDb) -> Ingot<'db> {
        match self.adt_ref(db) {
            AdtRef::Enum(e) => e.top_mod(db).ingot(db),
            AdtRef::Struct(s) => s.top_mod(db).ingot(db),
            AdtRef::Contract(c) => c.top_mod(db).ingot(db),
        }
    }

    pub(crate) fn as_generic_param_owner(
        self,
        db: &'db dyn HirAnalysisDb,
    ) -> Option<GenericParamOwner<'db>> {
        self.adt_ref(db).generic_owner()
    }
}

/// This struct represents a field of an ADT. If the ADT is an enum, this
/// represents a variant. It stores semantic field types, bound to identity
/// parameters.
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtField<'db> {
    tys: Vec<Binder<TyId<'db>>>,
}
impl<'db> AdtField<'db> {
    pub fn ty(&self, _db: &'db dyn HirAnalysisDb, i: usize) -> Binder<TyId<'db>> {
        self.tys[i].clone()
    }

    /// Iterates all field types of this variant.
    pub fn iter_types<'a>(
        &'a self,
        _db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<TyId<'db>>> + 'a {
        self.tys.iter().cloned()
    }

    pub fn num_types(&self) -> usize {
        self.tys.len()
    }

    pub(super) fn new(tys: Vec<Binder<TyId<'db>>>) -> Self {
        Self { tys }
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

    /// Returns the semantic ADT definition for this reference.
    /// Thin wrapper over the tracked `lower_adt` query for ergonomic use at call sites.
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> AdtDef<'db> {
        lower_adt(db, self)
    }

    pub(crate) fn generic_owner(self) -> Option<GenericParamOwner<'db>> {
        match self {
            AdtRef::Enum(e) => Some(e.into()),
            AdtRef::Struct(s) => Some(s.into()),
            AdtRef::Contract(_) => None,
        }
    }
}
