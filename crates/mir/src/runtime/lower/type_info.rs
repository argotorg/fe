use hir::{
    analysis::{
        semantic::SemanticInstance,
        ty::{
            ProviderAddressSpace, ProviderKind,
            const_ty::{ConcreteArrayLengthError, demand_concrete_array_length},
            normalize::normalize_or_keep,
            provider::{ProviderLayoutEvidence, provider_semantics},
            trait_def::ResolvedImplInstance,
            trait_resolution::PredicateListId,
            ty_def::{
                BorrowKind, MAX_INLINE_STRING_BYTES, PrimTy, TyBase, TyData, TyId, TyVarSort,
            },
        },
    },
    hir_def::scope_graph::ScopeId,
};
use num_traits::ToPrimitive;
use rustc_hash::FxHashSet;
use salsa::Update;

use crate::{
    db::MirDb,
    runtime::{
        AddressSpaceKind, LowerError, RawPointeeId, RawPointeeKey, RefKind, RefView, RuntimeClass,
        ScalarClass, ScalarRepr, ScalarRole,
    },
};

use super::layout::layout_for_ty_in_env;

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
pub(crate) struct RuntimeEffectHandleInfo<'db> {
    pub(crate) target_ty: TyId<'db>,
    pub(crate) space: AddressSpaceKind,
    pub(crate) impl_instance: ResolvedImplInstance<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Update)]
struct RuntimeTypeModel<'db> {
    repr_ty: TyId<'db>,
    shape: RuntimeTypeShape<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, Update)]
enum RuntimeTypeShape<'db> {
    Borrow { kind: BorrowKind, inner: TyId<'db> },
    Capability { inner: TyId<'db> },
    Pointer { target: TyId<'db> },
    Scalar(ScalarClass<'db>),
    Aggregate,
    Other,
}

impl<'db> RuntimeTypeModel<'db> {
    fn new(db: &'db dyn MirDb, ty: TyId<'db>, env: RuntimeTypeEnv<'db>) -> Self {
        let repr_ty = runtime_repr_ty_in_env(db, env, ty);
        let shape = if let Some((kind, inner)) = repr_ty.as_borrow(db) {
            RuntimeTypeShape::Borrow { kind, inner }
        } else if let Some((_, inner)) = repr_ty.as_capability(db) {
            RuntimeTypeShape::Capability { inner }
        } else if let Some(target) = repr_ty.as_ptr(db) {
            RuntimeTypeShape::Pointer { target }
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
        env: RuntimeTypeEnv<'db>,
    ) -> Option<RuntimeClass<'db>> {
        match &self.shape {
            RuntimeTypeShape::Borrow { inner, .. } => {
                if runtime_zero_sized_ty(db, *inner, env.scope, env.assumptions) {
                    Some(provider_class_for_target_in_env(
                        db,
                        env,
                        Some(*inner),
                        default_space,
                    ))
                } else {
                    Some(object_ref_class_for_target_in_env(db, env, *inner))
                }
            }
            RuntimeTypeShape::Capability { inner } => Some(provider_class_for_target_in_env(
                db,
                env,
                Some(*inner),
                default_space,
            )),
            RuntimeTypeShape::Pointer { target } => {
                Some(raw_addr_class_for_ty_in_env(db, env, *target))
            }
            RuntimeTypeShape::Scalar(scalar) => {
                (!runtime_zero_sized_ty(db, self.repr_ty, env.scope, env.assumptions))
                    .then(|| RuntimeClass::Scalar(scalar.clone()))
            }
            RuntimeTypeShape::Aggregate => {
                (!runtime_zero_sized_ty(db, self.repr_ty, env.scope, env.assumptions)).then(|| {
                    RuntimeClass::AggregateValue {
                        layout: layout_for_ty_in_env(db, env, self.repr_ty),
                    }
                })
            }
            RuntimeTypeShape::Other => None,
        }
    }

    fn stored_class(&self, db: &'db dyn MirDb, env: RuntimeTypeEnv<'db>) -> RuntimeClass<'db> {
        match &self.shape {
            RuntimeTypeShape::Borrow { inner, .. } => RuntimeClass::Ref {
                pointee: Box::new(stored_class_for_ty_in_env(db, env, *inner)),
                kind: RefKind::Native,
                view: RefView::Whole,
            },
            RuntimeTypeShape::Capability { inner } => {
                provider_class_for_target_in_env(db, env, Some(*inner), AddressSpaceKind::Memory)
            }
            RuntimeTypeShape::Pointer { target } => raw_addr_class_for_ty_in_env(db, env, *target),
            RuntimeTypeShape::Scalar(scalar) => RuntimeClass::Scalar(scalar.clone()),
            RuntimeTypeShape::Aggregate | RuntimeTypeShape::Other => RuntimeClass::AggregateValue {
                layout: layout_for_ty_in_env(db, env, self.repr_ty),
            },
        }
    }

    fn transport_sensitive_aggregate(&self, db: &'db dyn MirDb, env: RuntimeTypeEnv<'db>) -> bool {
        match &self.shape {
            RuntimeTypeShape::Borrow { .. } => true,
            RuntimeTypeShape::Capability { .. }
            | RuntimeTypeShape::Pointer { .. }
            | RuntimeTypeShape::Scalar(_) => false,
            RuntimeTypeShape::Aggregate => {
                if self.repr_ty.is_array(db) {
                    let (_, args) = self.repr_ty.decompose_ty_app(db);
                    return args.first().copied().is_some_and(|elem| {
                        runtime_transport_sensitive_aggregate(db, elem, env.scope, env.assumptions)
                    });
                }
                if self.repr_ty.is_tuple(db) || self.repr_ty.is_struct(db) {
                    return self.repr_ty.field_types(db).into_iter().any(|field| {
                        runtime_transport_sensitive_aggregate(db, field, env.scope, env.assumptions)
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
                                    env.scope,
                                    env.assumptions,
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

pub(crate) fn runtime_repr_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> TyId<'db> {
    runtime_storage_ty_in_env(db, env, ty)
}

/// Concrete runtime layout demand. Symbolic type checking uses the literal-only
/// `TyId::array_len` accessor; runtime lowering must also reduce specialized
/// abstract const expressions before interpreting an array's representation.
pub(crate) fn runtime_array_len<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
) -> Result<Option<usize>, LowerError> {
    if !ty.is_array(db) {
        return Ok(None);
    }
    let (_, args) = ty.decompose_ty_app(db);
    let Some(&len) = args.get(1) else {
        return Ok(None);
    };
    demand_concrete_array_length(db, len, len)
        .map_err(|error| {
            let reason = match error {
                ConcreteArrayLengthError::Invalid(cause) => cause.pretty_print(db),
                ConcreteArrayLengthError::Mismatch => {
                    "canonical and source array lengths disagree".to_string()
                }
            };
            LowerError::Unsupported(format!(
                "invalid runtime array length in `{}`: {reason}",
                ty.pretty_print(db)
            ))
        })?
        .map(|length| {
            length.to_usize().ok_or_else(|| {
                LowerError::Unsupported(format!(
                    "array length in `{}` exceeds the supported runtime layout range",
                    ty.pretty_print(db)
                ))
            })
        })
        .transpose()
}

pub(crate) fn validate_runtime_array_extents_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> Result<(), LowerError> {
    fn visit<'db>(
        db: &'db dyn MirDb,
        env: RuntimeTypeEnv<'db>,
        ty: TyId<'db>,
        seen: &mut FxHashSet<TyId<'db>>,
    ) -> Result<(), LowerError> {
        let ty = runtime_repr_ty_in_env(db, env, ty);
        if !seen.insert(ty) {
            return Ok(());
        }
        if ty.is_array(db) {
            runtime_array_len(db, ty)?.ok_or_else(|| {
                LowerError::Unsupported(format!(
                    "array length in `{}` is not concrete for runtime layout",
                    ty.pretty_print(db)
                ))
            })?;
            if let Some(elem) = ty.generic_args(db).first().copied() {
                visit(db, env, elem, seen)?;
            }
        } else if let Some((_, inner)) = ty.as_borrow(db) {
            visit(db, env, inner, seen)?;
        } else if let Some((_, inner)) = ty.as_capability(db) {
            visit(db, env, inner, seen)?;
        } else if ty.is_tuple(db) || ty.is_struct(db) {
            for field in ty.field_types(db) {
                visit(db, env, field, seen)?;
            }
        } else if let Some(enum_) = ty.as_enum(db) {
            let args = ty.generic_args(db);
            for variant in enum_.variants(db) {
                for field in variant.field_tys(db) {
                    visit(db, env, field.instantiate(db, args), seen)?;
                }
            }
        }
        Ok(())
    }

    visit(db, env, ty, &mut FxHashSet::default())
}

#[salsa::tracked]
fn runtime_interface_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    scope.map_or(ty, |scope| normalize_or_keep(db, ty, scope, assumptions))
}

pub(crate) fn runtime_interface_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> TyId<'db> {
    runtime_interface_ty(db, ty, env.scope, env.assumptions)
}

#[salsa::tracked]
fn runtime_storage_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let mut ty = runtime_interface_ty_in_env(db, RuntimeTypeEnv::new(scope, assumptions), ty);
    while let Some(inner) = ty.as_view(db) {
        ty = scope.map_or(inner, |scope| {
            normalize_or_keep(db, inner, scope, assumptions)
        });
    }
    ty
}

pub(crate) fn runtime_storage_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> TyId<'db> {
    runtime_storage_ty(db, ty, env.scope, env.assumptions)
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
    let repr_ty = runtime_repr_ty_in_env(db, RuntimeTypeEnv::new(scope, assumptions), ty);
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
        return runtime_array_len(db, repr_ty)
            .ok()
            .flatten()
            .is_some_and(|len| {
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
    if effect_handle_transport_class_for_ty_in_env(db, RuntimeTypeEnv::new(scope, assumptions), ty)
        .is_some()
    {
        return false;
    }
    let interface_ty = runtime_interface_ty_in_env(db, RuntimeTypeEnv::new(scope, assumptions), ty);
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
    runtime_top_level_class(db, ty, default_space, env.scope, env.assumptions)
}

pub(crate) fn stored_class_for_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> RuntimeClass<'db> {
    runtime_stored_class(db, ty, env.scope, env.assumptions)
}

pub(crate) fn object_ref_class_for_target_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    target_ty: TyId<'db>,
) -> RuntimeClass<'db> {
    let target_ty = runtime_repr_ty_in_env(db, env, target_ty);
    RuntimeClass::Ref {
        pointee: Box::new(stored_class_for_ty_in_env(db, env, target_ty)),
        kind: RefKind::Object,
        view: RefView::Whole,
    }
}

pub(crate) fn provider_class_for_target_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    target_ty: Option<TyId<'db>>,
    space: AddressSpaceKind,
) -> RuntimeClass<'db> {
    match target_ty.map(|ty| runtime_repr_ty_in_env(db, env, ty)) {
        Some(target_ty) => RuntimeClass::Ref {
            pointee: Box::new(stored_class_for_ty_in_env(db, env, target_ty)),
            kind: RefKind::Provider {
                provider_ty: TyId::borrow_ref_of(db, target_ty),
                space,
            },
            view: RefView::Whole,
        },
        None => RuntimeClass::opaque_raw_addr(space),
    }
}

pub(crate) fn scalar_class_for_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> Option<ScalarClass<'db>> {
    let ty = runtime_repr_ty_in_env(db, env, ty);
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

fn effect_handle_transport_class_for_info<'db>(
    db: &'db dyn MirDb,
    info: RuntimeEffectHandleInfo<'db>,
    effect_scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    if info.space == AddressSpaceKind::Memory {
        return RuntimeClass::RawAddr {
            space: info.space,
            pointee: raw_addr_pointee_for_ty_in_env(
                db,
                RuntimeTypeEnv::new(Some(effect_scope), assumptions),
                info.target_ty,
            ),
        };
    }
    provider_class_for_target_in_env(
        db,
        RuntimeTypeEnv::new(Some(effect_scope), assumptions),
        Some(info.target_ty),
        info.space,
    )
}

pub(crate) fn effect_handle_transport_class_for_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> Option<RuntimeClass<'db>> {
    let repr_ty = runtime_repr_ty_in_env(db, env, ty);
    if repr_ty.as_capability(db).is_some() {
        return None;
    }
    let effect_scope = env.scope.or_else(|| repr_ty.as_scope(db))?;
    let info = runtime_effect_handle_info(db, repr_ty, Some(effect_scope), env.assumptions)?;
    Some(effect_handle_transport_class_for_info(
        db,
        info,
        effect_scope,
        env.assumptions,
    ))
}

/// The raw pointee for a source target type, created without classifying it.
///
/// The key is the target normalized in the requesting environment and nothing
/// else, so the same closed type yields the same handle in every caller.
/// Scalar targets are classified directly; everything else stays deferred
/// until dereferenced, which is what lets `Node { next: *Node }` name its own
/// target while `layout(Node)` is still being built.
fn raw_addr_pointee_for_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> Option<RawPointeeId<'db>> {
    let ty = runtime_repr_ty_in_env(db, env, ty);
    if ty.has_param(db) || ty.contains_assoc_ty_of_param(db) {
        return None;
    }
    if let Some(scalar) = scalar_class_from_repr_ty(db, ty) {
        return Some(RawPointeeId::exact(db, RuntimeClass::Scalar(scalar)));
    }
    Some(RawPointeeId::new(db, RawPointeeKey::Stored(ty)))
}

/// Classifies a stored raw pointee in a context derived from its type alone:
/// the first ADT declaration scope in the type's syntax (head before
/// arguments), or no scope for ADT-free types, with empty assumptions.
///
/// The key is already normalized in the requesting environment, so resolution
/// only normalizes instantiated field declarations. Coherence admits an impl
/// for a closed self type only in the self type's or the trait's ingot, and
/// impl lookup searches both whatever the origin, so the requester's scope and
/// bounds do not select a different impl.
#[salsa::tracked]
pub(crate) fn resolve_stored_raw_pointee<'db>(
    db: &'db dyn MirDb,
    pointee: RawPointeeId<'db>,
) -> RuntimeClass<'db> {
    let RawPointeeKey::Stored(ty) = pointee.key(db) else {
        panic!("exact raw pointees resolve without classification: {pointee:?}");
    };
    let env = RuntimeTypeEnv::new(
        canonical_pointee_scope(db, *ty),
        PredicateListId::empty_list(db),
    );
    stored_class_for_ty_in_env(db, env, *ty)
}

fn canonical_pointee_scope<'db>(db: &'db dyn MirDb, ty: TyId<'db>) -> Option<ScopeId<'db>> {
    let (base, args) = ty.decompose_ty_app(db);
    if let TyData::TyBase(TyBase::Adt(_)) = base.data(db) {
        return base.as_scope(db);
    }
    args.iter()
        .filter(|arg| !matches!(arg.data(db), TyData::ConstTy(_)))
        .find_map(|arg| canonical_pointee_scope(db, *arg))
}

fn raw_addr_class_for_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> RuntimeClass<'db> {
    RuntimeClass::RawAddr {
        space: AddressSpaceKind::Memory,
        pointee: raw_addr_pointee_for_ty_in_env(db, env, ty),
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
        RuntimeTypeEnv::new(scope, assumptions),
    )
}

#[salsa::tracked]
fn runtime_type_model<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeTypeModel<'db> {
    RuntimeTypeModel::new(db, ty, RuntimeTypeEnv::new(scope, assumptions))
}

#[salsa::tracked]
fn runtime_stored_class<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    runtime_type_model(db, ty, scope, assumptions)
        .stored_class(db, RuntimeTypeEnv::new(scope, assumptions))
}

#[salsa::tracked]
pub(crate) fn runtime_effect_handle_info<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> Option<RuntimeEffectHandleInfo<'db>> {
    let repr_ty = runtime_repr_ty_in_env(db, RuntimeTypeEnv::new(scope, assumptions), ty);
    if repr_ty != ty {
        return runtime_effect_handle_info(db, repr_ty, scope, assumptions);
    }
    let scope = scope.or_else(|| repr_ty.as_scope(db))?;
    let semantics = provider_semantics(db, scope, assumptions, repr_ty);
    if matches!(semantics.kind, ProviderKind::RootObject) {
        return None;
    }
    let ProviderLayoutEvidence::ResolvedHandle(impl_instance) = semantics.evidence else {
        return None;
    };
    let target_ty = semantics.target_ty?;
    Some(RuntimeEffectHandleInfo {
        target_ty,
        space: provider_address_space_to_runtime(semantics.address_space?),
        impl_instance,
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
    runtime_type_model(db, ty, scope, assumptions)
        .transport_sensitive_aggregate(db, RuntimeTypeEnv::new(scope, assumptions))
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
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::{
        analysis::semantic::{get_or_build_semantic_instance, root_semantic_instance_key},
        analysis::ty::ty_check::BodyOwner,
        hir_def::TopLevelMod,
    };
    use url::Url;

    use crate::runtime::{
        BorrowAccess, Layout, LayoutId, LayoutKey, RuntimeBoundarySpec, StructLayout,
        relation::{raw_pointee_matches_class, raw_pointees_equivalent},
    };

    use super::super::boundary::{
        BoundaryMatcher, boundary_spec_for_ty_in_env, default_borrow_transport_set,
    };
    use super::*;

    #[test]
    fn plain_runtime_zst_boundary_is_erased() {
        let db = DriverDataBase::default();
        let assumptions = PredicateListId::new(&db, Vec::new());
        let unit = TyId::unit(&db);

        assert_eq!(
            boundary_spec_for_ty_in_env(
                &db,
                RuntimeTypeEnv::new(None, assumptions),
                unit,
                AddressSpaceKind::Memory
            ),
            None
        );
        assert_eq!(
            top_level_class_for_ty_in_env(
                &db,
                RuntimeTypeEnv::new(None, assumptions),
                unit,
                AddressSpaceKind::Memory
            ),
            None
        );
        assert!(
            matches!(
                stored_class_for_ty_in_env(&db, RuntimeTypeEnv::new(None, assumptions), unit),
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
        let boundary = boundary_spec_for_ty_in_env(
            &db,
            RuntimeTypeEnv::new(None, assumptions),
            borrowed_unit,
            AddressSpaceKind::Memory,
        )
        .expect("borrowed ZST should stay runtime-visible as provider transport");
        let RuntimeBoundarySpec::ExactShape(class) = boundary else {
            panic!("borrowed ZST should use exact-shape provider transport: {boundary:#?}");
        };
        assert_memory_provider_ref(&class);

        let top_level = top_level_class_for_ty_in_env(
            &db,
            RuntimeTypeEnv::new(None, assumptions),
            borrowed_unit,
            AddressSpaceKind::Memory,
        )
        .expect("borrowed ZST should have a top-level provider class");
        assert_memory_provider_ref(&top_level);
        assert!(matches!(
            stored_class_for_ty_in_env(&db, RuntimeTypeEnv::new(None, assumptions), borrowed_unit),
            RuntimeClass::Ref {
                kind: RefKind::Native,
                ..
            }
        ));
    }

    #[test]
    fn non_zst_borrow_boundary_remains_borrow_like() {
        let db = DriverDataBase::default();
        let assumptions = PredicateListId::new(&db, Vec::new());
        let borrowed_word = TyId::borrow_mut_of(&db, TyId::u256(&db));
        let boundary = boundary_spec_for_ty_in_env(
            &db,
            RuntimeTypeEnv::new(None, assumptions),
            borrowed_word,
            AddressSpaceKind::Memory,
        )
        .expect("non-ZST borrow should stay runtime-visible");
        let RuntimeBoundarySpec::BorrowLike { access, .. } = boundary else {
            panic!("non-ZST borrow should stay borrow-like at boundaries: {boundary:#?}");
        };
        assert_eq!(access, BorrowAccess::ReadWrite);

        let top_level = top_level_class_for_ty_in_env(
            &db,
            RuntimeTypeEnv::new(None, assumptions),
            borrowed_word,
            AddressSpaceKind::Memory,
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
        assert!(matches!(
            stored_class_for_ty_in_env(&db, RuntimeTypeEnv::new(None, assumptions), borrowed_word),
            RuntimeClass::Ref {
                kind: RefKind::Native,
                ..
            }
        ));
    }

    #[test]
    fn raw_pointer_targets_resolve_one_level() {
        let db = DriverDataBase::default();
        let assumptions = PredicateListId::new(&db, Vec::new());
        let env = RuntimeTypeEnv::new(None, assumptions);
        let word = stored_class_for_ty_in_env(&db, env, TyId::u256(&db));
        let word_ptr_ty = TyId::ptr_to(&db, TyId::u256(&db));
        let word_ptr =
            top_level_class_for_ty_in_env(&db, env, word_ptr_ty, AddressSpaceKind::Memory)
                .expect("word pointer class");

        assert_eq!(word_ptr.deref_target(&db), Some(word));

        let word_ptr_ptr = top_level_class_for_ty_in_env(
            &db,
            env,
            TyId::ptr_to(&db, word_ptr_ty),
            AddressSpaceKind::Memory,
        )
        .expect("nested word pointer class");

        assert_eq!(word_ptr_ptr.deref_target(&db), Some(word_ptr));
    }

    #[test]
    fn raw_pointer_pointees_do_not_make_the_pointer_an_aggregate_value() {
        let db = DriverDataBase::default();
        let layout = LayoutId::new(
            &db,
            LayoutKey::Struct(StructLayout {
                fields: Vec::new().into(),
            }),
        );
        let aggregate = RuntimeClass::AggregateValue { layout };
        let pointer = RuntimeClass::raw_addr(&db, AddressSpaceKind::Memory, aggregate);

        assert_eq!(pointer.aggregate_layout(), None);
        assert_eq!(
            pointer
                .deref_target(&db)
                .and_then(|pointee| pointee.aggregate_layout()),
            Some(layout),
        );
    }

    const RECURSIVE_SOURCE: &str = r#"
struct Node {
    value: u256,
    next: *Node,
}

struct A {
    value: u256,
    next: *A,
}

struct B {
    value: u256,
    next: *B,
}

struct Odd {
    value: u256,
    next: *Even,
}

struct Even {
    value: u8,
    next: *Odd,
}

struct RefHolder {
    target: *ref Node,
}

trait Word {
    type Repr
}

struct Narrow {}

impl Word for Narrow {
    type Repr = u8
}

struct Projected<T: Word> {
    value: T::Repr,
    next: *Projected<T>,
}

struct Buffer<const N: usize> {
    data: [u8; N],
    next: *Buffer<N>,
}

fn first(_ p: *Node, _ bytes: *u8, _ pair: (u8, Node)) {}
fn normalized(_ projected: *Projected<Narrow>, _ buffer: *Buffer<3>) {}
fn second(_ p: *Node, _ bytes: *u8, _ words: [u8; 2]) {}
fn shapes(_ a: *A, _ b: *B, _ odd: *Odd, _ holder: RefHolder) {}
"#;

    fn with_source<T>(f: impl for<'db> FnOnce(&'db DriverDataBase, TopLevelMod<'db>) -> T) -> T {
        let mut db = DriverDataBase::default();
        let url = Url::parse("file:///recursive_raw_pointees.fe").unwrap();
        db.workspace()
            .touch(&mut db, url.clone(), Some(RECURSIVE_SOURCE.to_string()));
        let file = db
            .workspace()
            .get(&db, &url)
            .expect("file should be loaded");
        let top_mod = db.top_mod(file);
        f(&db, top_mod)
    }

    /// A parameter's type together with its function's own runtime type env.
    fn param<'db>(
        db: &'db DriverDataBase,
        top_mod: TopLevelMod<'db>,
        func: &str,
        idx: usize,
    ) -> (TyId<'db>, RuntimeTypeEnv<'db>) {
        let func = top_mod
            .all_funcs(db)
            .iter()
            .copied()
            .find(|candidate| {
                candidate
                    .name(db)
                    .to_opt()
                    .is_some_and(|name| name.data(db) == func)
            })
            .unwrap_or_else(|| panic!("missing function `{func}`"));
        let key = root_semantic_instance_key(db, BodyOwner::Func(func)).expect("root key");
        let semantic = get_or_build_semantic_instance(db, key);
        let typed_body = semantic.key(db).typed_body(db);
        let binding = typed_body.param_binding(idx).expect("parameter binding");
        let env = RuntimeTypeEnv::for_semantic(db, semantic);
        (
            runtime_repr_ty_in_env(db, env, typed_body.binding_ty(db, binding)),
            env,
        )
    }

    fn raw_pointee<'db>(
        db: &'db DriverDataBase,
        top_mod: TopLevelMod<'db>,
        func: &str,
        idx: usize,
    ) -> RawPointeeId<'db> {
        let (ty, env) = param(db, top_mod, func, idx);
        let class = top_level_class_for_ty_in_env(db, env, ty, AddressSpaceKind::Memory)
            .expect("pointer parameter class");
        class.raw_pointee().expect("known raw pointee")
    }

    fn struct_fields<'db>(
        db: &'db DriverDataBase,
        class: &RuntimeClass<'db>,
    ) -> Vec<RuntimeClass<'db>> {
        let Some(Layout::Struct(layout)) = class.aggregate_layout().map(|layout| layout.data(db))
        else {
            panic!("expected struct class, got {class:?}");
        };
        layout.fields.to_vec()
    }

    #[test]
    fn source_pointees_do_not_depend_on_the_requesting_function() {
        with_source(|db, top_mod| {
            let first = raw_pointee(db, top_mod, "first", 0);
            let second = raw_pointee(db, top_mod, "second", 0);
            assert_eq!(first, second, "same closed type from different functions");
            assert!(matches!(first.key(db), RawPointeeKey::Stored(_)));

            let first_bytes = raw_pointee(db, top_mod, "first", 1);
            assert_eq!(first_bytes, raw_pointee(db, top_mod, "second", 1));
            let byte = RuntimeClass::Scalar(ScalarClass {
                repr: ScalarRepr::Int {
                    bits: 8,
                    signed: false,
                },
                role: ScalarRole::Plain,
            });
            assert_eq!(
                first_bytes,
                RawPointeeId::exact(db, byte),
                "scalar source targets share the exact address-of target"
            );
        });
    }

    #[test]
    fn recursive_pointees_are_named_before_their_layout_exists() {
        with_source(|db, top_mod| {
            let (ptr_ty, env) = param(db, top_mod, "first", 0);
            let node_ty = ptr_ty.as_ptr(db).expect("pointer parameter");
            let node = stored_class_for_ty_in_env(db, env, node_ty);
            let fields = struct_fields(db, &node);
            let RuntimeClass::RawAddr {
                space: AddressSpaceKind::Memory,
                pointee: Some(next),
            } = &fields[1]
            else {
                panic!("expected a raw next field, got {:?}", fields[1]);
            };
            assert_eq!(*next, raw_pointee(db, top_mod, "first", 0));
            assert_eq!(
                next.target(db),
                node,
                "canonical resolution agrees with the caller"
            );
        });
    }

    #[test]
    fn canonical_resolution_matches_caller_classification() {
        with_source(|db, top_mod| {
            for idx in 0..2 {
                let (ptr_ty, env) = param(db, top_mod, "normalized", idx);
                let target_ty = ptr_ty.as_ptr(db).expect("pointer parameter");
                let caller = stored_class_for_ty_in_env(db, env, target_ty);
                let pointee = raw_pointee(db, top_mod, "normalized", idx);
                assert_eq!(pointee.target(db), caller, "parameter {idx}");
                let fields = struct_fields(db, &caller);
                assert_eq!(fields[1].raw_pointee(), Some(pointee), "parameter {idx}");
            }
            let (projected_ty, env) = param(db, top_mod, "normalized", 0);
            let projected = stored_class_for_ty_in_env(db, env, projected_ty.as_ptr(db).unwrap());
            assert!(matches!(
                struct_fields(db, &projected)[0],
                RuntimeClass::Scalar(ScalarClass {
                    repr: ScalarRepr::Int { bits: 8, .. },
                    ..
                })
            ));
            let (buffer_ty, env) = param(db, top_mod, "normalized", 1);
            let buffer = stored_class_for_ty_in_env(db, env, buffer_ty.as_ptr(db).unwrap());
            assert_eq!(struct_fields(db, &buffer)[0].array_len(db), Some(3));
        });
    }

    #[test]
    fn referent_pointees_stay_deferred() {
        with_source(|db, top_mod| {
            let (holder_ty, env) = param(db, top_mod, "shapes", 3);
            let holder = stored_class_for_ty_in_env(db, env, holder_ty);
            let field = struct_fields(db, &holder).remove(0);
            let pointee = field.raw_pointee().expect("known raw pointee");
            assert!(matches!(pointee.key(db), RawPointeeKey::Stored(_)));
            assert!(matches!(
                pointee.target(db),
                RuntimeClass::Ref {
                    kind: RefKind::Native,
                    ..
                }
            ));
        });
    }

    #[test]
    fn canonical_scope_is_the_first_adt_in_type_syntax() {
        with_source(|db, top_mod| {
            let (pair_ty, _) = param(db, top_mod, "first", 2);
            let (ptr_ty, _) = param(db, top_mod, "first", 0);
            let node_ty = ptr_ty.as_ptr(db).unwrap();
            assert_eq!(canonical_pointee_scope(db, pair_ty), node_ty.as_scope(db));
            let (words_ty, _) = param(db, top_mod, "second", 2);
            assert_eq!(canonical_pointee_scope(db, words_ty), None);
        });
    }

    #[test]
    fn source_and_exact_spellings_of_one_target_are_equivalent() {
        with_source(|db, top_mod| {
            let stored = raw_pointee(db, top_mod, "first", 0);
            let exact = RawPointeeId::exact(db, stored.target(db));
            assert_ne!(stored, exact);
            assert!(raw_pointees_equivalent(db, stored, exact));
            assert!(raw_pointee_matches_class(db, stored, &exact.target(db)));
        });
    }

    #[test]
    fn isomorphic_recursive_pointees_are_equivalent_by_structure() {
        with_source(|db, top_mod| {
            let a = raw_pointee(db, top_mod, "shapes", 0);
            let b = raw_pointee(db, top_mod, "shapes", 1);
            let odd = raw_pointee(db, top_mod, "shapes", 2);
            assert_ne!(a.target(db), b.target(db));
            assert!(raw_pointees_equivalent(db, a, b));
            assert!(raw_pointees_equivalent(db, b, a));
            // The first edge cycles back consistently, but the second node's
            // scalar differs.
            assert!(!raw_pointees_equivalent(db, a, odd));
            assert!(!raw_pointees_equivalent(db, odd, a));
            // No provisional result leaked from the failed query.
            assert!(raw_pointees_equivalent(db, a, b));
        });
    }

    #[test]
    fn borrow_boundaries_select_the_exact_boundary_target() {
        with_source(|db, top_mod| {
            let stored = raw_pointee(db, top_mod, "first", 0);
            let node = stored.target(db);
            let boundary = RuntimeBoundarySpec::BorrowLike {
                pointee: node.clone(),
                access: BorrowAccess::ReadWrite,
                allow: default_borrow_transport_set(
                    BorrowAccess::ReadWrite,
                    AddressSpaceKind::Memory,
                ),
            };
            let source_spelled = RuntimeClass::RawAddr {
                space: AddressSpaceKind::Memory,
                pointee: Some(stored),
            };
            let exact_spelled = RuntimeClass::raw_addr(db, AddressSpaceKind::Memory, node.clone());
            let selected = BoundaryMatcher::selected_class(db, &source_spelled, &boundary);
            assert_eq!(selected.as_ref(), Some(&exact_spelled));
            assert_eq!(
                BoundaryMatcher::selected_class(db, &exact_spelled, &boundary),
                Some(exact_spelled.clone())
            );
            let storage = RuntimeClass::RawAddr {
                space: AddressSpaceKind::Storage,
                pointee: Some(stored),
            };
            assert_eq!(
                BoundaryMatcher::selected_class(db, &storage, &boundary),
                Some(RuntimeClass::raw_addr(db, AddressSpaceKind::Storage, node)),
                "the accepted address space is kept"
            );
        });
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
