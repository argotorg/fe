use crate::hir_def::{
    Enum, GenericParamOwner, IdentId, ItemKind, Partial, Struct, TypeId as HirTyId, VariantKind,
    scope_graph::ScopeId,
};
use crate::span::DynLazySpan;
use common::ingot::Ingot;
use rustc_hash::FxHashSet;
use salsa::Update;
use std::ops::Range;

use super::{
    binder::Binder,
    const_ty::ConstTyData,
    layout_holes::{
        LayoutPlaceholderPolicy, collect_layout_placeholder_pairs_in_order_with_policy,
        collect_layout_placeholders_in_order_with_policy,
    },
    trait_resolution::constraint::collect_constraints,
    ty_def::{InvalidCause, TyData, TyId},
    ty_lower::{GenericParamTypeSet, lower_hir_ty},
};
use crate::analysis::HirAnalysisDb;

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
        }
    }

    pub(crate) fn ingot(self, db: &'db dyn HirAnalysisDb) -> Ingot<'db> {
        match self.adt_ref(db) {
            AdtRef::Enum(e) => e.top_mod(db).ingot(db),
            AdtRef::Struct(s) => s.top_mod(db).ingot(db),
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
/// represents a variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtField<'db> {
    /// Field types as HIR type refs. To allow recursive types, these are kept
    /// at the HIR level and lowered on demand.
    tys: Vec<Partial<HirTyId<'db>>>,

    /// Scope of the containing ADT item.
    scope: ScopeId<'db>,
}
impl<'db> AdtField<'db> {
    pub fn ty(&self, db: &'db dyn HirAnalysisDb, i: usize) -> Binder<TyId<'db>> {
        use crate::analysis::ty::trait_resolution::PredicateListId;

        let assumptions = match self.scope {
            ScopeId::Item(ItemKind::Struct(struct_)) => {
                collect_constraints(db, GenericParamOwner::Struct(struct_)).instantiate_identity()
            }
            ScopeId::Item(ItemKind::Enum(enum_)) => {
                collect_constraints(db, GenericParamOwner::Enum(enum_)).instantiate_identity()
            }
            ScopeId::Item(ItemKind::Contract(_)) => PredicateListId::empty_list(db),
            _ => PredicateListId::empty_list(db),
        };

        let ty = if let Some(hir_ty) = self.tys[i].to_opt() {
            lower_hir_ty(db, hir_ty, self.scope, assumptions)
        } else {
            TyId::invalid(db, InvalidCause::ParseError)
        };

        Binder::bind(ty)
    }

    /// Iterates all field types of this variant.
    pub fn iter_types<'a>(
        &'a self,
        db: &'db dyn HirAnalysisDb,
    ) -> impl Iterator<Item = Binder<TyId<'db>>> + 'a {
        (0..self.num_types()).map(move |i| self.ty(db, i))
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
}

impl<'db> AdtRef<'db> {
    pub fn try_from_item(item: ItemKind<'db>) -> Option<Self> {
        match item {
            ItemKind::Enum(x) => Some(x.into()),
            ItemKind::Struct(x) => Some(x.into()),
            _ => None,
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            Self::Enum(e) => e.scope(),
            Self::Struct(s) => s.scope(),
        }
    }

    pub fn as_item(self) -> ItemKind<'db> {
        match self {
            AdtRef::Enum(e) => e.into(),
            AdtRef::Struct(s) => s.into(),
        }
    }

    pub fn name(self, db: &'db dyn HirAnalysisDb) -> Option<IdentId<'db>> {
        match self {
            AdtRef::Enum(e) => e.name(db),
            AdtRef::Struct(s) => s.name(db),
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

    pub fn is_must_use(self, db: &'db dyn HirAnalysisDb) -> bool {
        match self {
            AdtRef::Enum(enum_) => enum_.is_must_use(db),
            AdtRef::Struct(struct_) => struct_.is_must_use(db),
        }
    }

    /// Returns the semantic ADT definition for this reference.
    /// Thin wrapper over the tracked `lower_adt` query for ergonomic use at call sites.
    pub fn as_adt(self, db: &'db dyn HirAnalysisDb) -> AdtDef<'db> {
        crate::core::adt_lower::lower_adt(db, self)
    }

    pub(crate) fn generic_owner(self) -> Option<GenericParamOwner<'db>> {
        match self {
            AdtRef::Enum(e) => Some(e.into()),
            AdtRef::Struct(s) => Some(s.into()),
        }
    }
}

/// Struct for downstream diagnostics that refer to cycle members.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, salsa::Update)]
pub struct AdtCycleMember<'db> {
    pub adt: AdtDef<'db>,
    pub field_idx: u16,
    pub ty_idx: u16,
}

/// One derived trailing layout arg of an ADT application.
#[derive(Clone, Copy)]
pub(crate) struct AdtLayoutHoleEntry<'db> {
    /// The placeholder's (fallbacked) const value type.
    pub(crate) hole_ty: TyId<'db>,
    /// The placeholder itself, when the plan occurrence was substituted in
    /// from an explicit generic arg. Reusing it as the trailing arg keeps one
    /// logical hole as one `TyId` instead of minting a parallel identity.
    pub(crate) source: Option<TyId<'db>>,
}

#[derive(Default)]
pub(crate) struct AdtLayoutHolePlan<'db> {
    entries: Vec<AdtLayoutHoleEntry<'db>>,
    field_ranges: Vec<Vec<Range<usize>>>,
}

impl<'db> AdtLayoutHolePlan<'db> {
    pub(crate) fn entries(&self) -> &[AdtLayoutHoleEntry<'db>] {
        &self.entries
    }

    pub(crate) fn len(&self) -> usize {
        self.entries.len()
    }

    pub(crate) fn hole_ty(&self, idx: usize) -> Option<TyId<'db>> {
        self.entries.get(idx).map(|entry| entry.hole_ty)
    }

    pub(crate) fn field_range(&self, variant_idx: usize, field_idx: usize) -> Range<usize> {
        self.field_ranges
            .get(variant_idx)
            .and_then(|variant| variant.get(field_idx))
            .cloned()
            .unwrap_or(0..0)
    }
}

pub(crate) fn adt_layout_hole_plan<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
) -> AdtLayoutHolePlan<'db> {
    adt_layout_hole_plan_with_explicit_args(db, adt, &[])
}

pub(crate) fn adt_layout_hole_plan_with_explicit_args<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    explicit_args: &[TyId<'db>],
) -> AdtLayoutHolePlan<'db> {
    // Placeholders occurring inside the explicit args themselves: a plan
    // occurrence matching one of these was substituted into the field
    // template by instantiation, so the trailing arg can reuse its identity.
    // Template-resident placeholders (minted in the ADT's own field lowering)
    // must NOT be reused: their identity belongs to the definition, and each
    // application needs a fresh trailing hole.
    let explicit_arg_placeholders = explicit_args
        .iter()
        .flat_map(|&arg| {
            collect_layout_placeholders_in_order_with_policy(
                db,
                arg,
                LayoutPlaceholderPolicy::HolesAndImplicitParams,
            )
        })
        .collect::<FxHashSet<_>>();

    let mut entries = Vec::new();
    let field_ranges = adt
        .fields(db)
        .iter()
        .enumerate()
        .map(|(variant_idx, variant)| {
            (0..variant.num_types())
                .map(|field_idx| {
                    let field_ty =
                        instantiated_adt_field_ty(db, adt, variant_idx, field_idx, explicit_args);
                    let start = entries.len();
                    entries.extend(
                        collect_layout_placeholder_pairs_in_order_with_policy(
                            db,
                            field_ty,
                            LayoutPlaceholderPolicy::HolesAndImplicitParams,
                        )
                        .into_iter()
                        .map(|(placeholder, hole_ty)| AdtLayoutHoleEntry {
                            hole_ty,
                            // Every occurrence of an explicit-arg placeholder
                            // reuses it: the user wrote one hole, so all of
                            // its template occurrences are that hole. This
                            // also keeps nested applications stable — an
                            // outer ADT's plan sees the inner application's
                            // already-reused trailing occurrence and must not
                            // re-split it.
                            source: explicit_arg_placeholders
                                .contains(&placeholder)
                                .then_some(placeholder),
                        }),
                    );
                    start..entries.len()
                })
                .collect()
        })
        .collect();

    AdtLayoutHolePlan {
        entries,
        field_ranges,
    }
}

pub(crate) fn instantiated_adt_field_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    adt: AdtDef<'db>,
    variant_idx: usize,
    field_idx: usize,
    explicit_args: &[TyId<'db>],
) -> TyId<'db> {
    let scope = adt.scope(db);
    let param_count = adt.params(db).len();
    adt.fields(db)
        .get(variant_idx)
        .and_then(|variant| (field_idx < variant.num_types()).then(|| variant.ty(db, field_idx)))
        .map(|field_ty| {
            if explicit_args.len() >= param_count {
                return field_ty.instantiate_scoped(db, scope, explicit_args);
            }

            field_ty.instantiate_with(db, |ty| match ty.data(db) {
                TyData::TyParam(param) if param.owner == scope => {
                    explicit_args.get(param.idx).copied().unwrap_or(ty)
                }
                TyData::ConstTy(const_ty) => {
                    if let ConstTyData::TyParam(param, _) = const_ty.data(db)
                        && param.owner == scope
                    {
                        return explicit_args.get(param.idx).copied().unwrap_or(ty);
                    }
                    ty
                }
                _ => ty,
            })
        })
        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
}
