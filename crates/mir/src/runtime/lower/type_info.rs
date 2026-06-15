use hir::{
    analysis::{
        semantic::SemanticInstance,
        ty::{
            ProviderAddressSpace, ProviderKind,
            normalize::normalize_ty,
            provider::provider_semantics,
            trait_resolution::PredicateListId,
            ty_def::{
                BorrowKind, MAX_INLINE_STRING_BYTES, PrimTy, TyBase, TyData, TyId, TyVarSort,
            },
        },
    },
    hir_def::scope_graph::ScopeId,
};
use salsa::Update;

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, LayoutId, RefKind, RefView, RuntimeClass, ScalarClass, ScalarRepr,
        ScalarRole,
    },
};

use super::layout::layout_for_ty_in_context;

#[derive(Clone, Copy)]
pub(crate) struct RuntimeTypeEnv<'db> {
    pub(crate) scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    pub(crate) assumptions: PredicateListId<'db>,
}

impl<'db> RuntimeTypeEnv<'db> {
    pub(crate) fn new(
        scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        Self { scope, assumptions }
    }

    pub(crate) fn for_semantic(db: &'db dyn MirDb, semantic: SemanticInstance<'db>) -> Self {
        Self {
            scope: Some(semantic.key(db).owner(db).scope()),
            assumptions: semantic.assumptions(db),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Update)]
struct RuntimeEffectHandleInfo<'db> {
    target_ty: TyId<'db>,
    space: AddressSpaceKind,
}

#[derive(Clone, Debug, PartialEq, Eq, Update)]
struct RuntimeTypeModel<'db> {
    repr_ty: TyId<'db>,
    shape: RuntimeTypeShape<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Update)]
enum RuntimeTypeShape<'db> {
    Borrow {
        kind: BorrowKind,
        inner: TyId<'db>,
    },
    Capability {
        inner: TyId<'db>,
    },
    EffectHandle {
        info: RuntimeEffectHandleInfo<'db>,
        effect_scope: ScopeId<'db>,
    },
    Scalar(ScalarClass<'db>),
    Aggregate,
    Other,
}

impl<'db> RuntimeTypeModel<'db> {
    fn new(
        db: &'db dyn MirDb,
        ty: TyId<'db>,
        scope: Option<ScopeId<'db>>,
        assumptions: PredicateListId<'db>,
    ) -> Self {
        let repr_ty = runtime_repr_ty_in_context(db, ty, scope, assumptions);
        let effect_scope = scope.or_else(|| repr_ty.as_scope(db));
        let shape = if let Some((kind, inner)) = repr_ty.as_borrow(db) {
            RuntimeTypeShape::Borrow { kind, inner }
        } else if let Some((_, inner)) = repr_ty.as_capability(db) {
            RuntimeTypeShape::Capability { inner }
        } else if let Some(effect_scope) = effect_scope
            && let Some(info) =
                runtime_effect_handle_info(db, repr_ty, Some(effect_scope), assumptions)
        {
            RuntimeTypeShape::EffectHandle { info, effect_scope }
        } else if let Some(scalar) = scalar_class_from_repr_ty(db, repr_ty) {
            RuntimeTypeShape::Scalar(scalar)
        } else if repr_ty.as_enum(db).is_some()
            || repr_ty.is_struct(db)
            || repr_ty.is_array(db)
            || repr_ty.is_tuple(db)
        {
            RuntimeTypeShape::Aggregate
        } else {
            RuntimeTypeShape::Other
        };
        Self { repr_ty, shape }
    }

    fn top_level_class(
        &self,
        db: &'db dyn MirDb,
        default_space: AddressSpaceKind,
        scope: Option<ScopeId<'db>>,
        assumptions: PredicateListId<'db>,
    ) -> Option<RuntimeClass<'db>> {
        match &self.shape {
            RuntimeTypeShape::Borrow { inner, .. } => {
                if runtime_zero_sized_ty(db, *inner, scope, assumptions) {
                    Some(provider_class_for_target_in_context(
                        db,
                        Some(*inner),
                        default_space,
                        scope,
                        assumptions,
                    ))
                } else {
                    Some(object_ref_class_for_target_in_context(
                        db,
                        *inner,
                        scope,
                        assumptions,
                    ))
                }
            }
            RuntimeTypeShape::Capability { inner } => Some(provider_class_for_target_in_context(
                db,
                Some(*inner),
                default_space,
                scope,
                assumptions,
            )),
            RuntimeTypeShape::EffectHandle { info, effect_scope } => Some(
                effect_handle_class_for_info(db, *info, *effect_scope, assumptions),
            ),
            RuntimeTypeShape::Scalar(scalar) => {
                (!runtime_zero_sized_ty(db, self.repr_ty, scope, assumptions))
                    .then(|| RuntimeClass::Scalar(scalar.clone()))
            }
            RuntimeTypeShape::Aggregate => {
                (!runtime_zero_sized_ty(db, self.repr_ty, scope, assumptions)).then(|| {
                    RuntimeClass::AggregateValue {
                        layout: layout_for_ty_in_context(db, self.repr_ty, scope, assumptions),
                    }
                })
            }
            RuntimeTypeShape::Other => None,
        }
    }

    fn stored_class(
        &self,
        db: &'db dyn MirDb,
        scope: Option<ScopeId<'db>>,
        assumptions: PredicateListId<'db>,
    ) -> RuntimeClass<'db> {
        match &self.shape {
            RuntimeTypeShape::Borrow { inner, .. } | RuntimeTypeShape::Capability { inner } => {
                provider_class_for_target_in_context(
                    db,
                    Some(*inner),
                    AddressSpaceKind::Memory,
                    scope,
                    assumptions,
                )
            }
            RuntimeTypeShape::EffectHandle { info, effect_scope } => {
                effect_handle_class_for_info(db, *info, *effect_scope, assumptions)
            }
            RuntimeTypeShape::Scalar(scalar) => RuntimeClass::Scalar(scalar.clone()),
            RuntimeTypeShape::Aggregate | RuntimeTypeShape::Other => RuntimeClass::AggregateValue {
                layout: layout_for_ty_in_context(db, self.repr_ty, scope, assumptions),
            },
        }
    }

    fn transport_sensitive_aggregate(
        &self,
        db: &'db dyn MirDb,
        scope: Option<ScopeId<'db>>,
        assumptions: PredicateListId<'db>,
    ) -> bool {
        match &self.shape {
            RuntimeTypeShape::Borrow { .. } => true,
            RuntimeTypeShape::Capability { .. }
            | RuntimeTypeShape::EffectHandle { .. }
            | RuntimeTypeShape::Scalar(_) => false,
            RuntimeTypeShape::Aggregate => {
                if self.repr_ty.is_array(db) {
                    let (_, args) = self.repr_ty.decompose_ty_app(db);
                    return args.first().copied().is_some_and(|elem| {
                        runtime_transport_sensitive_aggregate(db, elem, scope, assumptions)
                    });
                }
                if self.repr_ty.is_tuple(db) || self.repr_ty.is_struct(db) {
                    return self.repr_ty.field_types(db).into_iter().any(|field| {
                        runtime_transport_sensitive_aggregate(db, field, scope, assumptions)
                    });
                }
                if let Some(enum_) = self.repr_ty.as_enum(db) {
                    let adt = enum_.as_adt(db);
                    let args = self.repr_ty.generic_args(db);
                    return adt
                        .fields(db)
                        .iter()
                        .enumerate()
                        .any(|(variant_idx, variant)| {
                            (0..variant.num_types()).any(|field_idx| {
                                runtime_transport_sensitive_aggregate(
                                    db,
                                    adt.fields(db)[variant_idx]
                                        .ty(db, field_idx)
                                        .instantiate(db, args),
                                    scope,
                                    assumptions,
                                )
                            })
                        });
                }
                false
            }
            RuntimeTypeShape::Other => false,
        }
    }
}

pub(crate) fn runtime_repr_ty_in_context<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    runtime_storage_ty_in_context(db, ty, scope, assumptions)
}

#[salsa::tracked]
fn runtime_interface_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    scope.map_or(ty, |scope| normalize_ty(db, ty, scope, assumptions))
}

pub(crate) fn runtime_interface_ty_in_context<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    runtime_interface_ty(db, ty, scope, assumptions)
}

#[salsa::tracked]
fn runtime_storage_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let mut ty = runtime_interface_ty_in_context(db, ty, scope, assumptions);
    while let Some(inner) = ty.as_view(db) {
        ty = scope.map_or(inner, |scope| normalize_ty(db, inner, scope, assumptions));
    }
    ty
}

pub(crate) fn runtime_storage_ty_in_context<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    runtime_storage_ty(db, ty, scope, assumptions)
}

#[salsa::tracked(
    cycle_fn=runtime_zero_sized_ty_cycle_recover,
    cycle_initial=runtime_zero_sized_ty_cycle_initial
)]
pub(super) fn runtime_zero_sized_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let repr_ty = runtime_repr_ty_in_context(db, ty, scope, assumptions);
    if repr_ty != ty {
        return runtime_zero_sized_ty(db, repr_ty, scope, assumptions);
    }
    if repr_ty.is_never(db)
        || matches!(
            repr_ty.base_ty(db).data(db),
            TyData::TyBase(hir::analysis::ty::ty_def::TyBase::Func(_))
        )
    {
        return true;
    }
    if repr_ty.is_array(db) {
        let (_, args) = repr_ty.decompose_ty_app(db);
        return repr_ty.array_len(db).is_some_and(|len| {
            len == 0
                || args
                    .first()
                    .copied()
                    .is_some_and(|elem| runtime_zero_sized_ty(db, elem, scope, assumptions))
        });
    }
    if repr_ty.is_tuple(db) || repr_ty.is_struct(db) {
        return repr_ty
            .field_types(db)
            .into_iter()
            .all(|field| runtime_zero_sized_ty(db, field, scope, assumptions));
    }
    false
}

pub(super) fn runtime_zero_sized_transport_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    let interface_ty = runtime_interface_ty_in_context(db, ty, scope, assumptions);
    if runtime_zero_sized_ty(db, interface_ty, scope, assumptions) {
        return true;
    }
    if let Some((_, inner)) = interface_ty.as_borrow(db) {
        return runtime_zero_sized_transport_ty(db, inner, scope, assumptions);
    }
    if let Some((_, inner)) = interface_ty.as_capability(db) {
        return runtime_zero_sized_transport_ty(db, inner, scope, assumptions);
    }
    false
}

pub(crate) fn top_level_class_for_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
    default_space: AddressSpaceKind,
) -> Option<RuntimeClass<'db>> {
    top_level_class_for_ty_in_context(db, ty, default_space, env.scope, env.assumptions)
}

pub(crate) fn provider_class_for_target_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    target_ty: Option<TyId<'db>>,
    space: AddressSpaceKind,
) -> RuntimeClass<'db> {
    provider_class_for_target_in_context(db, target_ty, space, env.scope, env.assumptions)
}

pub(crate) fn scalar_class_for_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> Option<ScalarClass<'db>> {
    scalar_class_for_ty_in_context(db, ty, env.scope, env.assumptions)
}

pub(crate) fn top_level_class_for_ty_in_context<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    default_space: AddressSpaceKind,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeClass<'db>> {
    runtime_top_level_class(db, ty, default_space, scope, assumptions)
}

pub(crate) fn stored_class_for_ty_in_context<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    runtime_stored_class(db, ty, scope, assumptions)
}

pub(crate) fn object_ref_class_for_target_in_context<'db>(
    db: &'db dyn MirDb,
    target_ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    let target_ty = runtime_repr_ty_in_context(db, target_ty, scope, assumptions);
    RuntimeClass::Ref {
        pointee: Box::new(stored_class_for_ty_in_context(
            db,
            target_ty,
            scope,
            assumptions,
        )),
        kind: RefKind::Object,
        view: RefView::Whole,
    }
}

pub(crate) fn provider_class_for_target_in_context<'db>(
    db: &'db dyn MirDb,
    target_ty: Option<TyId<'db>>,
    space: AddressSpaceKind,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    match target_ty.map(|ty| runtime_repr_ty_in_context(db, ty, scope, assumptions)) {
        Some(target_ty) => RuntimeClass::Ref {
            pointee: Box::new(stored_class_for_ty_in_context(
                db,
                target_ty,
                scope,
                assumptions,
            )),
            kind: RefKind::Provider {
                provider_ty: TyId::borrow_ref_of(db, target_ty),
                space,
            },
            view: RefView::Whole,
        },
        None => RuntimeClass::RawAddr {
            space,
            target: None,
        },
    }
}

pub(crate) fn scalar_class_for_ty_in_context<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<ScalarClass<'db>> {
    let ty = runtime_repr_ty_in_context(db, ty, scope, assumptions);
    scalar_class_from_repr_ty(db, ty)
}

pub(crate) fn provider_address_space_to_runtime(space: ProviderAddressSpace) -> AddressSpaceKind {
    match space {
        ProviderAddressSpace::Memory => AddressSpaceKind::Memory,
        ProviderAddressSpace::Storage => AddressSpaceKind::Storage,
        ProviderAddressSpace::Transient => AddressSpaceKind::Transient,
        ProviderAddressSpace::Calldata => AddressSpaceKind::Calldata,
        ProviderAddressSpace::Code => AddressSpaceKind::Code,
    }
}

fn scalar_class_from_repr_ty<'db>(db: &'db dyn MirDb, ty: TyId<'db>) -> Option<ScalarClass<'db>> {
    let repr = match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Prim(prim)) => match prim {
            PrimTy::Bool => ScalarRepr::Bool,
            PrimTy::U8 => ScalarRepr::Int {
                bits: 8,
                signed: false,
            },
            PrimTy::U16 => ScalarRepr::Int {
                bits: 16,
                signed: false,
            },
            PrimTy::U32 => ScalarRepr::Int {
                bits: 32,
                signed: false,
            },
            PrimTy::U64 => ScalarRepr::Int {
                bits: 64,
                signed: false,
            },
            PrimTy::U128 => ScalarRepr::Int {
                bits: 128,
                signed: false,
            },
            PrimTy::U256 | PrimTy::Usize => ScalarRepr::Int {
                bits: 256,
                signed: false,
            },
            PrimTy::I8 => ScalarRepr::Int {
                bits: 8,
                signed: true,
            },
            PrimTy::I16 => ScalarRepr::Int {
                bits: 16,
                signed: true,
            },
            PrimTy::I32 => ScalarRepr::Int {
                bits: 32,
                signed: true,
            },
            PrimTy::I64 => ScalarRepr::Int {
                bits: 64,
                signed: true,
            },
            PrimTy::I128 => ScalarRepr::Int {
                bits: 128,
                signed: true,
            },
            PrimTy::I256 | PrimTy::Isize => ScalarRepr::Int {
                bits: 256,
                signed: true,
            },
            PrimTy::String => ScalarRepr::FixedBytes {
                // Fixed strings have at most 31 payload bytes, but casts and
                // `to_word` operate on the full EVM word.
                len: (MAX_INLINE_STRING_BYTES + 1) as u16,
            },
            PrimTy::Array
            | PrimTy::Tuple(_)
            | PrimTy::Ptr
            | PrimTy::View
            | PrimTy::BorrowMut
            | PrimTy::BorrowRef => return None,
        },
        TyData::TyBase(TyBase::Contract(_)) => ScalarRepr::Address { bits: 256 },
        TyData::TyVar(var) if matches!(var.sort, TyVarSort::Integral) => ScalarRepr::Int {
            bits: 256,
            signed: false,
        },
        _ => return None,
    };

    Some(ScalarClass {
        repr,
        role: ScalarRole::Plain,
    })
}

fn effect_handle_class_for_info<'db>(
    db: &'db dyn MirDb,
    info: RuntimeEffectHandleInfo<'db>,
    effect_scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    if info.space == AddressSpaceKind::Memory {
        return RuntimeClass::RawAddr {
            space: info.space,
            target: raw_addr_target_for_ty_in_context(
                db,
                info.target_ty,
                Some(effect_scope),
                assumptions,
            ),
        };
    }
    provider_class_for_target_in_context(
        db,
        Some(info.target_ty),
        info.space,
        Some(effect_scope),
        assumptions,
    )
}

pub(crate) fn effect_handle_class_for_ty_in_context<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeClass<'db>> {
    let repr_ty = runtime_repr_ty_in_context(db, ty, scope, assumptions);
    if repr_ty.as_capability(db).is_some() {
        return None;
    }
    let effect_scope = scope.or_else(|| repr_ty.as_scope(db))?;
    let info = runtime_effect_handle_info(db, repr_ty, Some(effect_scope), assumptions)?;
    Some(effect_handle_class_for_info(
        db,
        info,
        effect_scope,
        assumptions,
    ))
}

fn raw_addr_target_for_ty_in_context<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<LayoutId<'db>> {
    match stored_class_for_ty_in_context(db, ty, scope, assumptions) {
        RuntimeClass::AggregateValue { layout } => Some(layout),
        RuntimeClass::Scalar(_) | RuntimeClass::Ref { .. } | RuntimeClass::RawAddr { .. } => None,
    }
}

#[salsa::tracked]
fn runtime_top_level_class<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    default_space: AddressSpaceKind,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeClass<'db>> {
    runtime_type_model(db, ty, scope, assumptions).top_level_class(
        db,
        default_space,
        scope,
        assumptions,
    )
}

#[salsa::tracked]
fn runtime_type_model<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeTypeModel<'db> {
    RuntimeTypeModel::new(db, ty, scope, assumptions)
}

#[salsa::tracked]
fn runtime_stored_class<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    runtime_type_model(db, ty, scope, assumptions).stored_class(db, scope, assumptions)
}

#[salsa::tracked]
fn runtime_effect_handle_info<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeEffectHandleInfo<'db>> {
    let repr_ty = runtime_repr_ty_in_context(db, ty, scope, assumptions);
    if repr_ty != ty {
        return runtime_effect_handle_info(db, repr_ty, scope, assumptions);
    }
    let scope = scope.or_else(|| repr_ty.as_scope(db))?;
    let semantics = provider_semantics(db, scope, assumptions, repr_ty);
    if matches!(semantics.kind, ProviderKind::RootObject) {
        return None;
    }
    let target_ty = semantics.target_ty?;
    Some(RuntimeEffectHandleInfo {
        target_ty,
        space: provider_address_space_to_runtime(semantics.address_space?),
    })
}

#[salsa::tracked(
    cycle_fn=runtime_transport_sensitive_aggregate_cycle_recover,
    cycle_initial=runtime_transport_sensitive_aggregate_cycle_initial
)]
pub(super) fn runtime_transport_sensitive_aggregate<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> bool {
    runtime_type_model(db, ty, scope, assumptions).transport_sensitive_aggregate(
        db,
        scope,
        assumptions,
    )
}

fn runtime_zero_sized_ty_cycle_initial<'db>(
    _db: &'db dyn MirDb,
    _ty: TyId<'db>,
    _scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    _assumptions: PredicateListId<'db>,
) -> bool {
    false
}

fn runtime_zero_sized_ty_cycle_recover<'db>(
    _db: &'db dyn MirDb,
    _value: &bool,
    _count: u32,
    _ty: TyId<'db>,
    _scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    _assumptions: PredicateListId<'db>,
) -> salsa::CycleRecoveryAction<bool> {
    salsa::CycleRecoveryAction::Iterate
}

fn runtime_transport_sensitive_aggregate_cycle_initial<'db>(
    _db: &'db dyn MirDb,
    _ty: TyId<'db>,
    _scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    _assumptions: PredicateListId<'db>,
) -> bool {
    false
}

fn runtime_transport_sensitive_aggregate_cycle_recover<'db>(
    _db: &'db dyn MirDb,
    _value: &bool,
    _count: u32,
    _ty: TyId<'db>,
    _scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    _assumptions: PredicateListId<'db>,
) -> salsa::CycleRecoveryAction<bool> {
    salsa::CycleRecoveryAction::Iterate
}

#[cfg(test)]
mod tests {
    use driver::DriverDataBase;

    use crate::runtime::{BorrowAccess, RuntimeBoundarySpec};

    use super::super::boundary::boundary_spec_for_ty_in_context;
    use super::*;

    #[test]
    fn plain_runtime_zst_boundary_is_erased() {
        let db = DriverDataBase::default();
        let assumptions = PredicateListId::new(&db, Vec::new());
        let unit = TyId::unit(&db);

        assert_eq!(
            boundary_spec_for_ty_in_context(&db, unit, AddressSpaceKind::Memory, None, assumptions),
            None
        );
        assert_eq!(
            top_level_class_for_ty_in_context(
                &db,
                unit,
                AddressSpaceKind::Memory,
                None,
                assumptions
            ),
            None
        );
        assert!(
            matches!(
                stored_class_for_ty_in_context(&db, unit, None, assumptions),
                RuntimeClass::AggregateValue { .. }
            ),
            "stored ZST layout should remain available for aggregate layout construction",
        );
    }

    #[test]
    fn zst_borrow_boundary_preserves_provider_transport() {
        let db = DriverDataBase::default();
        let assumptions = PredicateListId::new(&db, Vec::new());
        let borrowed_unit = TyId::borrow_mut_of(&db, TyId::unit(&db));
        let boundary = boundary_spec_for_ty_in_context(
            &db,
            borrowed_unit,
            AddressSpaceKind::Memory,
            None,
            assumptions,
        )
        .expect("borrowed ZST should stay runtime-visible as provider transport");
        let RuntimeBoundarySpec::ExactShape(class) = boundary else {
            panic!("borrowed ZST should use exact-shape provider transport: {boundary:#?}");
        };
        assert_memory_provider_ref(&class);

        let top_level = top_level_class_for_ty_in_context(
            &db,
            borrowed_unit,
            AddressSpaceKind::Memory,
            None,
            assumptions,
        )
        .expect("borrowed ZST should have a top-level provider class");
        assert_memory_provider_ref(&top_level);
        assert_memory_provider_ref(&stored_class_for_ty_in_context(
            &db,
            borrowed_unit,
            None,
            assumptions,
        ));
    }

    #[test]
    fn non_zst_borrow_boundary_remains_borrow_like() {
        let db = DriverDataBase::default();
        let assumptions = PredicateListId::new(&db, Vec::new());
        let borrowed_word = TyId::borrow_mut_of(&db, TyId::u256(&db));
        let boundary = boundary_spec_for_ty_in_context(
            &db,
            borrowed_word,
            AddressSpaceKind::Memory,
            None,
            assumptions,
        )
        .expect("non-ZST borrow should stay runtime-visible");
        let RuntimeBoundarySpec::BorrowLike { access, .. } = boundary else {
            panic!("non-ZST borrow should stay borrow-like at boundaries: {boundary:#?}");
        };
        assert_eq!(access, BorrowAccess::ReadWrite);

        let top_level = top_level_class_for_ty_in_context(
            &db,
            borrowed_word,
            AddressSpaceKind::Memory,
            None,
            assumptions,
        )
        .expect("non-ZST borrow should have a top-level object ref class");
        assert!(
            matches!(
                top_level,
                RuntimeClass::Ref {
                    kind: RefKind::Object,
                    ..
                }
            ),
            "non-ZST borrow top-level class should remain an object ref: {top_level:#?}",
        );
        assert_memory_provider_ref(&stored_class_for_ty_in_context(
            &db,
            borrowed_word,
            None,
            assumptions,
        ));
    }

    fn assert_memory_provider_ref(class: &RuntimeClass<'_>) {
        let RuntimeClass::Ref { kind, .. } = class else {
            panic!("expected provider ref, got {class:#?}");
        };
        assert!(
            matches!(
                kind,
                RefKind::Provider {
                    space: AddressSpaceKind::Memory,
                    ..
                }
            ),
            "expected memory provider ref, got {class:#?}",
        );
    }
}
