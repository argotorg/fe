use crate::{analysis::HirAnalysisDb, core::hir_def::EnumVariant};

use super::{
    adt_def::instantiate_adt_field_shape,
    ty_def::{BorrowKind, CapabilityKind, InvalidCause, TyId},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatternDestructureMode {
    Owned,
    Borrow(BorrowKind),
}

#[derive(Debug, Clone, Copy)]
pub enum PatternProjectionStep<'db> {
    Field(usize),
    VariantField {
        variant: EnumVariant<'db>,
        field_idx: usize,
    },
}

pub fn destructure_pattern_source<'db>(
    db: &'db dyn HirAnalysisDb,
    source_ty: TyId<'db>,
) -> (TyId<'db>, PatternDestructureMode) {
    if let Some((kind, inner)) = source_ty.as_capability(db) {
        let borrow_kind = match kind {
            CapabilityKind::Mut => BorrowKind::Mut,
            CapabilityKind::Ref | CapabilityKind::View => BorrowKind::Ref,
        };
        (inner, PatternDestructureMode::Borrow(borrow_kind))
    } else {
        (source_ty, PatternDestructureMode::Owned)
    }
}

pub fn pattern_match_expected_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    source_ty: TyId<'db>,
) -> TyId<'db> {
    destructure_pattern_source(db, source_ty).0
}

pub fn apply_pattern_borrow_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    mode: PatternDestructureMode,
    child_source_ty: TyId<'db>,
) -> TyId<'db> {
    match mode {
        PatternDestructureMode::Owned => child_source_ty,
        PatternDestructureMode::Borrow(_) if child_source_ty.as_capability(db).is_some() => {
            child_source_ty
        }
        PatternDestructureMode::Borrow(BorrowKind::Mut) => TyId::borrow_mut_of(db, child_source_ty),
        PatternDestructureMode::Borrow(BorrowKind::Ref) => TyId::borrow_ref_of(db, child_source_ty),
    }
}

pub fn project_pattern_child_source_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    parent_match_container_ty: TyId<'db>,
    projection: PatternProjectionStep<'db>,
) -> TyId<'db> {
    match projection {
        PatternProjectionStep::Field(field_idx) => parent_match_container_ty
            .field_types(db)
            .get(field_idx)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),
        PatternProjectionStep::VariantField { variant, field_idx } => parent_match_container_ty
            .adt_def(db)
            .filter(|adt_def| (variant.idx as usize) < adt_def.fields(db).len())
            .and_then(|adt_def| {
                adt_def
                    .fields(db)
                    .get(variant.idx as usize)
                    .filter(|fields| field_idx < fields.num_types())
                    .map(|_| {
                        instantiate_adt_field_shape(
                            db,
                            adt_def,
                            variant.idx as usize,
                            field_idx,
                            parent_match_container_ty.generic_args(db),
                        )
                    })
            })
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other)),
    }
}

pub fn project_pattern_child_carrier_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    parent_carrier_ty: TyId<'db>,
    projection: PatternProjectionStep<'db>,
) -> TyId<'db> {
    let (container_match_ty, mode) = destructure_pattern_source(db, parent_carrier_ty);
    let child_source_ty = project_pattern_child_source_ty(db, container_match_ty, projection);
    apply_pattern_borrow_mode(db, mode, child_source_ty)
}

#[cfg(test)]
mod tests {
    use crate::{
        analysis::ty::{
            pattern_types::{
                PatternProjectionStep, destructure_pattern_source, pattern_match_expected_ty,
                project_pattern_child_carrier_ty,
            },
            ty_check::check_func_body,
            ty_def::TyId,
        },
        core::hir_def::EnumVariant,
        hir_def::ItemKind,
        test_db::HirAnalysisTestDb,
    };

    use super::PatternDestructureMode;

    fn with_func_param_ty(
        src: &str,
        func_name: &str,
        param_idx: usize,
        f: impl for<'db> FnOnce(&'db HirAnalysisTestDb, crate::analysis::ty::ty_def::TyId<'db>),
    ) {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(format!("{func_name}.fe").into(), src);
        let (top_mod, _) = db.top_mod(file);
        let func = top_mod
            .all_items(&db)
            .iter()
            .find_map(|item| match item {
                ItemKind::Func(func)
                    if func
                        .name(&db)
                        .to_opt()
                        .is_some_and(|name| name.data(&db) == func_name) =>
                {
                    Some(*func)
                }
                _ => None,
            })
            .expect("missing function");
        let (diags, typed_body) = check_func_body(&db, func).clone();
        assert!(diags.is_empty(), "{diags:?}");
        let binding = typed_body
            .param_binding(param_idx)
            .expect("missing param binding");
        let ty = typed_body.binding_ty(&db, binding);
        f(&db, ty);
    }

    #[test]
    fn pattern_match_expected_ty_strips_capabilities() {
        with_func_param_ty(
            r#"
struct Pair { a: u256 }
fn field(x: ref Pair) {}
"#,
            "field",
            0,
            |db, pair_ref_ty| {
                assert!(
                    pattern_match_expected_ty(db, pair_ref_ty)
                        .as_capability(db)
                        .is_none()
                );
                assert!(matches!(
                    destructure_pattern_source(db, pair_ref_ty),
                    (_, PatternDestructureMode::Borrow(_))
                ));
            },
        );
        let db = HirAnalysisTestDb::default();
        assert_eq!(
            pattern_match_expected_ty(&db, TyId::borrow_mut_of(&db, TyId::u256(&db)))
                .pretty_print(&db)
                .to_string(),
            "u256"
        );
        assert_eq!(
            pattern_match_expected_ty(&db, TyId::view_of(&db, TyId::u256(&db)))
                .pretty_print(&db)
                .to_string(),
            "u256"
        );
    }

    #[test]
    fn projected_pattern_child_carrier_types_preserve_destructuring_policy() {
        with_func_param_ty(
            r#"
struct Pair { a: u256 }
fn field(x: ref Pair) {}
"#,
            "field",
            0,
            |db, pair_ref_ty| {
                assert_eq!(
                    project_pattern_child_carrier_ty(
                        db,
                        pair_ref_ty,
                        PatternProjectionStep::Field(0)
                    )
                    .pretty_print(db)
                    .to_string(),
                    "ref u256"
                );
            },
        );
        with_func_param_ty(
            r#"
fn payload(x: Option<mut u256>) {}
"#,
            "payload",
            0,
            |db, option_ty| {
                let option_variant = EnumVariant::new(
                    pattern_match_expected_ty(db, option_ty)
                        .as_enum(db)
                        .expect("option enum"),
                    0,
                );
                assert_eq!(
                    project_pattern_child_carrier_ty(
                        db,
                        option_ty,
                        PatternProjectionStep::VariantField {
                            variant: option_variant,
                            field_idx: 0,
                        },
                    )
                    .pretty_print(db)
                    .to_string(),
                    "mut u256"
                );
            },
        );
        with_func_param_ty(
            r#"
fn wrapped(x: ref Option<mut u256>) {}
"#,
            "wrapped",
            0,
            |db, wrapped_option_ty| {
                let option_variant = EnumVariant::new(
                    pattern_match_expected_ty(db, wrapped_option_ty)
                        .as_enum(db)
                        .expect("option enum"),
                    0,
                );
                assert_eq!(
                    project_pattern_child_carrier_ty(
                        db,
                        wrapped_option_ty,
                        PatternProjectionStep::VariantField {
                            variant: option_variant,
                            field_idx: 0,
                        },
                    )
                    .pretty_print(db)
                    .to_string(),
                    "mut u256"
                );
            },
        );
        with_func_param_ty(
            r#"
struct Pair { a: u256 }
fn viewed(x: Pair) {}
"#,
            "viewed",
            0,
            |db, viewed_pair_ty| {
                assert_eq!(
                    project_pattern_child_carrier_ty(
                        db,
                        TyId::view_of(db, pattern_match_expected_ty(db, viewed_pair_ty)),
                        PatternProjectionStep::Field(0),
                    )
                    .pretty_print(db)
                    .to_string(),
                    "ref u256"
                );
            },
        );
    }
}
