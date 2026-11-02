use hir::analysis::{
    semantic::SemanticInstance,
    ty::{
        AddressSpace,
        const_ty::{ConcreteArrayLengthError, demand_concrete_array_length},
        normalize::normalize_ty,
        trait_resolution::PredicateListId,
        ty_def::{
            BorrowKind, MAX_INLINE_STRING_BYTES, PrimTy, TyBase, TyData, TyId, TyVarSort,
            prim_int_bits,
        },
    },
};
use num_traits::ToPrimitive;
use rustc_hash::FxHashSet;
use salsa::SalsaValue;

use crate::{
    db::MirDb,
    runtime::{
        ConstScalar, LowerError, RefKind, RefView, RuntimeClass, ScalarClass, ScalarRepr,
        ScalarRole,
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

#[derive(Clone, Debug, PartialEq, Eq, SalsaValue)]
struct RuntimeTypeModel<'db> {
    repr_ty: TyId<'db>,
    shape: RuntimeTypeShape<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq, SalsaValue)]
enum RuntimeTypeShape<'db> {
    Borrow { kind: BorrowKind, inner: TyId<'db> },
    Capability { inner: TyId<'db> },
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
        } else if let Some(scalar) = scalar_class_from_repr_ty(db, repr_ty) {
            RuntimeTypeShape::Scalar(scalar)
        } else if repr_ty.as_enum(db).is_some() || repr_ty.is_product(db) || repr_ty.is_array(db) {
            RuntimeTypeShape::Aggregate
        } else {
            RuntimeTypeShape::Other
        };
        Self { repr_ty, shape }
    }

    fn top_level_class(
        &self,
        db: &'db dyn MirDb,
        default_space: AddressSpace,
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
                provider_class_for_target_in_env(db, env, Some(*inner), AddressSpace::Memory)
            }
            RuntimeTypeShape::Scalar(scalar) => RuntimeClass::Scalar(scalar.clone()),
            RuntimeTypeShape::Aggregate | RuntimeTypeShape::Other => RuntimeClass::AggregateValue {
                layout: layout_for_ty_in_env(db, env, self.repr_ty),
            },
        }
    }

    fn transport_sensitive_aggregate(&self, db: &'db dyn MirDb, env: RuntimeTypeEnv<'db>) -> bool {
        match &self.shape {
            RuntimeTypeShape::Borrow { .. } => true,
            RuntimeTypeShape::Capability { .. } | RuntimeTypeShape::Scalar(_) => false,
            RuntimeTypeShape::Aggregate => {
                if self.repr_ty.is_array(db) {
                    let (_, args) = self.repr_ty.decompose_ty_app(db);
                    return args.first().copied().is_some_and(|elem| {
                        runtime_transport_sensitive_aggregate(db, elem, env.scope, env.assumptions)
                    });
                }
                if self.repr_ty.is_product(db) {
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
        } else if ty.is_product(db) {
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

#[salsa::tracked(returns(copy))]
fn runtime_interface_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    scope.map_or(ty, |scope| normalize_ty(db, ty, scope, assumptions))
}

pub(crate) fn runtime_interface_ty_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    ty: TyId<'db>,
) -> TyId<'db> {
    runtime_interface_ty(db, ty, env.scope, env.assumptions)
}

#[salsa::tracked(returns(copy))]
fn runtime_storage_ty<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let mut ty = runtime_interface_ty_in_env(db, RuntimeTypeEnv::new(scope, assumptions), ty);
    while let Some(inner) = ty.as_view(db) {
        ty = scope.map_or(inner, |scope| normalize_ty(db, inner, scope, assumptions));
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

#[salsa::tracked(returns(copy), cycle_initial=runtime_zero_sized_ty_cycle_initial)]
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
    // A pinned type's root number identifies its region.
    if repr_ty.is_product(db) && repr_ty.pin(db).is_none() {
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
    default_space: AddressSpace,
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
    space: AddressSpace,
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

fn scalar_class_from_repr_ty<'db>(db: &'db dyn MirDb, ty: TyId<'db>) -> Option<ScalarClass<'db>> {
    let repr = match ty.base_ty(db).data(db) {
        TyData::TyBase(TyBase::Prim(prim)) => match prim {
            PrimTy::Bool => ScalarRepr::Bool,
            PrimTy::Ptr(space) => return Some(pointer_class(db, *space)),
            PrimTy::String => ScalarRepr::FixedBytes {
                // Fixed strings have at most 31 payload bytes, but casts and
                // `to_word` operate on the full EVM word.
                len: (MAX_INLINE_STRING_BYTES + 1) as u16,
            },
            _ => ScalarRepr::Int {
                bits: prim_int_bits(*prim, db.compilation_settings().target(db))? as u16,
                signed: prim.is_signed_int(),
            },
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

/// The integer an address in `space` converts to. A memory address is a
/// `usize`, which takes the target's width; the EVM's other spaces are
/// addressed by word.
pub(crate) fn address_int_class<'db>(db: &dyn MirDb, space: AddressSpace) -> ScalarClass<'db> {
    let bits = if space == AddressSpace::Memory {
        db.compilation_settings().target(db).usize_bits() as u16
    } else {
        256
    };
    ScalarClass {
        repr: ScalarRepr::Int {
            bits,
            signed: false,
        },
        role: ScalarRole::Plain,
    }
}

/// The memory address or size `value` as a constant of the memory
/// `address_int_class`, the target's `usize`.
pub(crate) fn memory_int_const(db: &dyn MirDb, value: u64) -> ConstScalar {
    let ScalarRepr::Int { bits, .. } = address_int_class(db, AddressSpace::Memory).repr else {
        unreachable!("a memory integer is an integer");
    };
    ConstScalar::Int {
        bits,
        signed: false,
        words: value
            .to_be_bytes()
            .into_iter()
            .skip_while(|byte| *byte == 0)
            .collect(),
    }
}

/// The scalar a pointer into `space` is: its address integer, which for
/// memory is a pointer.
pub(crate) fn pointer_class<'db>(db: &dyn MirDb, space: AddressSpace) -> ScalarClass<'db> {
    let class = address_int_class(db, space);
    if space == AddressSpace::Memory {
        ScalarClass {
            role: ScalarRole::Pointer,
            ..class
        }
    } else {
        class
    }
}

/// The transport a pointer of type `pointer_ty` converts to when
/// dereferenced: a raw memory address, or a state space's provider reference,
/// as for a `StateIndex` entry.
pub(crate) fn deref_transport_class_in_env<'db>(
    db: &'db dyn MirDb,
    env: RuntimeTypeEnv<'db>,
    pointer_ty: TyId<'db>,
) -> Option<RuntimeClass<'db>> {
    let (target, space) = runtime_repr_ty_in_env(db, env, pointer_ty).as_ptr(db)?;
    Some(match space {
        AddressSpace::Memory => RuntimeClass::raw_addr(
            db,
            AddressSpace::Memory,
            stored_class_for_ty_in_env(db, env, target),
        ),
        space => provider_class_for_target_in_env(db, env, Some(target), space),
    })
}

#[salsa::tracked(returns(clone))]
fn runtime_top_level_class<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    default_space: AddressSpace,
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

#[salsa::tracked(returns(clone))]
fn runtime_stored_class<'db>(
    db: &'db dyn MirDb,
    ty: TyId<'db>,
    scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    assumptions: PredicateListId<'db>,
) -> RuntimeClass<'db> {
    runtime_type_model(db, ty, scope, assumptions)
        .stored_class(db, RuntimeTypeEnv::new(scope, assumptions))
}

#[salsa::tracked(returns(copy), cycle_initial=runtime_transport_sensitive_aggregate_cycle_initial)]
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
    _: salsa::Id,
    _ty: TyId<'db>,
    _scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    _assumptions: PredicateListId<'db>,
) -> bool {
    false
}

fn runtime_transport_sensitive_aggregate_cycle_initial<'db>(
    _db: &'db dyn MirDb,
    _: salsa::Id,
    _ty: TyId<'db>,
    _scope: Option<hir::hir_def::scope_graph::ScopeId<'db>>,
    _assumptions: PredicateListId<'db>,
) -> bool {
    false
}

#[cfg(test)]
mod tests {
    use common::InputDb;
    use driver::DriverDataBase;
    use hir::{
        analysis::semantic::root_semantic_instance_key, analysis::ty::ty_check::BodyOwner,
        hir_def::TopLevelMod,
    };
    use url::Url;

    use crate::runtime::{
        BorrowAccess, Layout, LayoutId, LayoutKey, RuntimeBoundarySpec, StructLayout,
    };

    use super::super::boundary::boundary_spec_for_ty_in_env;
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
                AddressSpace::Memory
            ),
            None
        );
        assert_eq!(
            top_level_class_for_ty_in_env(
                &db,
                RuntimeTypeEnv::new(None, assumptions),
                unit,
                AddressSpace::Memory
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
            AddressSpace::Memory,
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
            AddressSpace::Memory,
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
            AddressSpace::Memory,
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
            AddressSpace::Memory,
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
    fn pointers_are_address_scalars() {
        let db = DriverDataBase::default();
        let assumptions = PredicateListId::new(&db, Vec::new());
        let env = RuntimeTypeEnv::new(None, assumptions);
        let word = stored_class_for_ty_in_env(&db, env, TyId::u256(&db));
        let word_ptr_ty = TyId::ptr_to(&db, TyId::u256(&db));
        let word_ptr_ptr_ty = TyId::ptr_to(&db, word_ptr_ty);
        let slot_ty = TyId::ptr_in(&db, AddressSpace::Storage, TyId::u256(&db));

        // A memory pointer is a pointer, and a storage pointer its slot.
        let pointer = RuntimeClass::Scalar(pointer_class(&db, AddressSpace::Memory));
        for (ty, class) in [
            (word_ptr_ty, &pointer),
            (word_ptr_ptr_ty, &pointer),
            (slot_ty, &word),
        ] {
            assert_eq!(
                top_level_class_for_ty_in_env(&db, env, ty, AddressSpace::Memory).as_ref(),
                Some(class)
            );
        }
        assert_eq!(
            deref_transport_class_in_env(&db, env, word_ptr_ty),
            Some(RuntimeClass::raw_addr(
                &db,
                AddressSpace::Memory,
                word.clone()
            ))
        );
        assert_eq!(
            deref_transport_class_in_env(&db, env, word_ptr_ptr_ty),
            Some(RuntimeClass::raw_addr(&db, AddressSpace::Memory, pointer))
        );
        assert_eq!(
            deref_transport_class_in_env(&db, env, slot_ty),
            Some(provider_class_for_target_in_env(
                &db,
                env,
                Some(TyId::u256(&db)),
                AddressSpace::Storage,
            ))
        );
    }

    #[test]
    fn raw_pointer_pointees_do_not_make_the_pointer_an_aggregate_value() {
        let db = DriverDataBase::default();
        let layout = LayoutId::new(
            &db,
            LayoutKey::Struct(StructLayout {
                fields: Vec::new().into(),
                pinned: false,
            }),
        );
        let aggregate = RuntimeClass::AggregateValue { layout };
        let pointer = RuntimeClass::raw_addr(&db, AddressSpace::Memory, aggregate);

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
fn shapes(_ a: *A, _ b: *B, _ odd: *Odd) {}
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
        let key = root_semantic_instance_key(db, BodyOwner::Func(func, None)).expect("root key");
        let semantic = SemanticInstance::new(db, key);
        let typed_body = semantic.key(db).typed_body(db);
        let binding = typed_body.param_binding(idx).expect("parameter binding");
        let env = RuntimeTypeEnv::for_semantic(db, semantic);
        (
            runtime_repr_ty_in_env(db, env, typed_body.binding_carrier_ty(db, binding)),
            env,
        )
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
    fn recursive_pointer_fields_are_pointers() {
        with_source(|db, top_mod| {
            let (ptr_ty, env) = param(db, top_mod, "first", 0);
            let node = deref_transport_class_in_env(db, env, ptr_ty)
                .and_then(|transport| transport.deref_target(db))
                .expect("pointer parameter");
            let word = stored_class_for_ty_in_env(db, env, TyId::u256(db));
            let pointer = RuntimeClass::Scalar(pointer_class(db, AddressSpace::Memory));
            assert_eq!(struct_fields(db, &node), vec![word, pointer]);
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
                    space: AddressSpace::Memory,
                    ..
                }
            ),
            "expected memory provider ref, got {class:#?}",
        );
    }
}
