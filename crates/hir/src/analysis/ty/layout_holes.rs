use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    const_ty::{ConstTyData, ConstTyId, LayoutHoleId},
    fold::{TyFoldable, TyFolder},
    ty_def::{TyData, TyId},
    ty_lower::{
        CallableInputLayoutHoleOrigin, callable_input_layout_hole_groups, collect_generic_params,
    },
    visitor::{TyVisitable, TyVisitor, walk_ty},
};
use crate::analysis::HirAnalysisDb;
use crate::hir_def::{CallableDef, Func};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum LayoutPlaceholderPolicy {
    HolesOnly,
    HolesAndImplicitParams,
}

pub(crate) fn layout_hole_fallback_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    hole_ty: TyId<'db>,
) -> TyId<'db> {
    if hole_ty.has_invalid(db) {
        TyId::u256(db)
    } else {
        hole_ty
    }
}

pub(crate) fn layout_hole_with_fallback_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    hole_ty: TyId<'db>,
    hole_id: LayoutHoleId<'db>,
) -> TyId<'db> {
    TyId::const_ty(
        db,
        match hole_id {
            LayoutHoleId::Opaque => {
                ConstTyId::hole_with_ty(db, layout_hole_fallback_ty(db, hole_ty))
            }
            LayoutHoleId::PathArg { path, arg_idx } => ConstTyId::hole_with_path_arg(
                db,
                layout_hole_fallback_ty(db, hole_ty),
                path,
                arg_idx,
            ),
        },
    )
}

fn is_layout_placeholder<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    policy: LayoutPlaceholderPolicy,
) -> bool {
    let TyData::ConstTy(const_ty) = ty.data(db) else {
        return false;
    };

    match const_ty.data(db) {
        ConstTyData::Hole(..) => true,
        ConstTyData::TyParam(param, _)
            if policy == LayoutPlaceholderPolicy::HolesAndImplicitParams && param.is_implicit() =>
        {
            true
        }
        _ => false,
    }
}

pub fn ty_contains_const_hole<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    struct HoleFinder<'db> {
        db: &'db dyn HirAnalysisDb,
        found: bool,
    }

    impl<'db> TyVisitor<'db> for HoleFinder<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if self.found {
                return;
            }

            if let TyData::ConstTy(const_ty) = ty.data(self.db)
                && matches!(const_ty.data(self.db), ConstTyData::Hole(..))
            {
                self.found = true;
                return;
            }

            walk_ty(self, ty);
        }
    }

    let mut finder = HoleFinder { db, found: false };
    ty.visit_with(&mut finder);
    finder.found
}

pub(crate) fn collect_layout_placeholders_in_order<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    collect_layout_placeholders_in_order_with_policy(db, value, LayoutPlaceholderPolicy::HolesOnly)
}

pub(crate) fn collect_layout_placeholders_in_order_with_policy<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    policy: LayoutPlaceholderPolicy,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    struct LayoutPlaceholderCollector<'a, 'db> {
        db: &'db dyn HirAnalysisDb,
        policy: LayoutPlaceholderPolicy,
        out: &'a mut Vec<TyId<'db>>,
    }

    impl<'a, 'db> TyVisitor<'db> for LayoutPlaceholderCollector<'a, 'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            if is_layout_placeholder(self.db, ty, self.policy) {
                self.out.push(ty);
            }
            walk_ty(self, ty);
        }
    }

    let mut out = Vec::new();
    value.visit_with(&mut LayoutPlaceholderCollector {
        db,
        policy,
        out: &mut out,
    });
    out
}

pub(crate) fn substitute_layout_placeholders_in_order<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    layout_args: &[TyId<'db>],
    policy: LayoutPlaceholderPolicy,
) -> T
where
    T: TyFoldable<'db>,
{
    if layout_args.is_empty() {
        return value;
    }

    struct LayoutPlaceholderSubst<'a, 'db> {
        db: &'db dyn HirAnalysisDb,
        args: &'a [TyId<'db>],
        policy: LayoutPlaceholderPolicy,
        next: usize,
    }

    impl<'a, 'db> TyFolder<'db> for LayoutPlaceholderSubst<'a, 'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            if is_layout_placeholder(self.db, ty, self.policy)
                && let Some(arg) = self.args.get(self.next).copied()
            {
                self.next += 1;
                return arg;
            }

            ty.super_fold_with(db, self)
        }
    }

    let mut folder = LayoutPlaceholderSubst {
        db,
        args: layout_args,
        policy,
        next: 0,
    };
    value.fold_with(db, &mut folder)
}

pub(crate) fn substitute_layout_placeholders_by_identity<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    layout_args: &FxHashMap<TyId<'db>, TyId<'db>>,
    policy: LayoutPlaceholderPolicy,
) -> T
where
    T: TyFoldable<'db>,
{
    if layout_args.is_empty() {
        return value;
    }

    struct LayoutPlaceholderIdentitySubst<'a, 'db> {
        db: &'db dyn HirAnalysisDb,
        args: &'a FxHashMap<TyId<'db>, TyId<'db>>,
        policy: LayoutPlaceholderPolicy,
    }

    impl<'a, 'db> TyFolder<'db> for LayoutPlaceholderIdentitySubst<'a, 'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            if is_layout_placeholder(self.db, ty, self.policy)
                && let Some(arg) = self.args.get(&ty).copied()
            {
                return arg;
            }

            ty.super_fold_with(db, self)
        }
    }

    let mut folder = LayoutPlaceholderIdentitySubst {
        db,
        args: layout_args,
        policy,
    };
    value.fold_with(db, &mut folder)
}

pub(crate) fn substitute_layout_holes_in<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    layout_args: &[TyId<'db>],
) -> T
where
    T: TyFoldable<'db>,
{
    substitute_layout_placeholders_in_order(
        db,
        value,
        layout_args,
        LayoutPlaceholderPolicy::HolesOnly,
    )
}

pub(crate) fn substitute_layout_holes<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    layout_args: &[TyId<'db>],
) -> TyId<'db> {
    substitute_layout_holes_in(db, ty, layout_args)
}

pub(crate) fn substitute_layout_holes_by_identity_in<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    layout_args: &FxHashMap<TyId<'db>, TyId<'db>>,
) -> T
where
    T: TyFoldable<'db>,
{
    substitute_layout_placeholders_by_identity(
        db,
        value,
        layout_args,
        LayoutPlaceholderPolicy::HolesOnly,
    )
}

pub(crate) fn substitute_layout_holes_by_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    layout_args: &FxHashMap<TyId<'db>, TyId<'db>>,
) -> TyId<'db> {
    substitute_layout_holes_by_identity_in(db, ty, layout_args)
}

fn collect_unique_layout_placeholders_in_order_with_policy<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    policy: LayoutPlaceholderPolicy,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    let mut seen = FxHashSet::default();
    collect_layout_placeholders_in_order_with_policy(db, value, policy)
        .into_iter()
        .filter(|ty| seen.insert(*ty))
        .collect()
}

pub(crate) fn collect_unique_layout_placeholders_in_order<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    collect_unique_layout_placeholders_in_order_with_policy(
        db,
        value,
        LayoutPlaceholderPolicy::HolesOnly,
    )
}

pub(crate) fn collect_layout_hole_tys_in_order<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    collect_layout_placeholders_in_order(db, value)
        .into_iter()
        .filter_map(|hole| {
            let TyData::ConstTy(const_ty) = hole.data(db) else {
                return None;
            };
            let ConstTyData::Hole(hole_ty, _) = const_ty.data(db) else {
                return None;
            };
            Some(layout_hole_fallback_ty(db, *hole_ty))
        })
        .collect()
}

pub(crate) fn alpha_rename_hidden_layout_placeholders<'db, T>(
    db: &'db dyn HirAnalysisDb,
    expected: T,
    actual: T,
) -> T
where
    T: TyFoldable<'db> + TyVisitable<'db> + Copy,
{
    let expected_hidden = collect_unique_layout_placeholders_in_order_with_policy(
        db,
        expected,
        LayoutPlaceholderPolicy::HolesAndImplicitParams,
    );
    let actual_hidden = collect_unique_layout_placeholders_in_order_with_policy(
        db,
        actual,
        LayoutPlaceholderPolicy::HolesAndImplicitParams,
    );
    if expected_hidden.len() != actual_hidden.len() {
        return expected;
    }

    let layout_args = expected_hidden
        .into_iter()
        .zip(actual_hidden)
        .collect::<FxHashMap<_, _>>();
    substitute_layout_placeholders_by_identity(
        db,
        expected,
        &layout_args,
        LayoutPlaceholderPolicy::HolesAndImplicitParams,
    )
}

/// Returns the implicit layout const-params inserted for a callable's input layout holes.
///
/// These args are ordered to match `callable_input_layout_hole_groups`.
#[salsa::tracked(return_ref)]
pub(crate) fn callable_input_layout_implicit_args<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> Vec<TyId<'db>> {
    collect_generic_params(db, func.into())
        .params(db)
        .iter()
        .copied()
        .filter(|ty| {
            if let TyData::ConstTy(const_ty) = ty.data(db)
                && let ConstTyData::TyParam(param, _) = const_ty.data(db)
            {
                param.is_implicit()
            } else {
                false
            }
        })
        .collect()
}

pub(crate) fn callable_input_layout_bindings_by_origin<'db>(
    db: &'db dyn HirAnalysisDb,
    method: CallableDef<'db>,
) -> FxHashMap<CallableInputLayoutHoleOrigin, Vec<(TyId<'db>, TyId<'db>)>> {
    let CallableDef::Func(func) = method else {
        return FxHashMap::default();
    };
    let groups = callable_input_layout_hole_groups(db, func);
    let implicit_layout_args = callable_input_layout_implicit_args(db, func);
    let expected_layout_arg_count = groups
        .iter()
        .map(|group| group.placeholders.len())
        .sum::<usize>();
    assert_eq!(
        implicit_layout_args.len(),
        expected_layout_arg_count,
        "layout-hole binder count mismatch while elaborating callable input types"
    );

    let mut out = FxHashMap::default();
    let mut next_layout_arg = 0usize;
    for group in groups {
        let mut bindings = Vec::with_capacity(group.placeholders.len());
        for placeholder in group.placeholders {
            let implicit_param = implicit_layout_args[next_layout_arg];
            next_layout_arg += 1;
            bindings.push((placeholder, implicit_param));
        }
        out.insert(group.origin, bindings);
    }
    assert_eq!(
        next_layout_arg,
        implicit_layout_args.len(),
        "layout-hole binder walk did not consume all hidden layout arguments"
    );
    out
}

pub(crate) fn callable_input_layout_implicit_params_by_origin<'db>(
    db: &'db dyn HirAnalysisDb,
    method: CallableDef<'db>,
) -> FxHashMap<CallableInputLayoutHoleOrigin, Vec<TyId<'db>>> {
    callable_input_layout_bindings_by_origin(db, method)
        .into_iter()
        .map(|(origin, bindings)| {
            (
                origin,
                bindings
                    .into_iter()
                    .map(|(_, implicit_param)| implicit_param)
                    .collect(),
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;

    use super::alpha_rename_hidden_layout_placeholders;
    use crate::analysis::ty::{
        const_ty::{ConstTyData, ConstTyId},
        ty_def::{Kind, TyId, TyParam},
    };
    use crate::hir_def::{IdentId, ItemKind};
    use crate::test_db::HirAnalysisTestDb;

    #[test]
    fn alpha_rename_preserves_repeated_placeholder_identity() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("alpha_rename_preserves_repeated_placeholder_identity.fe"),
            "fn f() {}",
        );
        let (top_mod, _) = db.top_mod(file);
        let func = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func),
                _ => None,
            })
            .expect("missing `f` function");
        let scope = func.scope();

        let mk_param_ty = |idx, name: &str| {
            let param = TyParam::implicit_param(
                IdentId::new(&db, name.to_string()),
                idx,
                Kind::Star,
                scope,
            );
            TyId::const_ty(
                &db,
                ConstTyId::new(&db, ConstTyData::TyParam(param, TyId::u256(&db))),
            )
        };

        let expected =
            TyId::tuple_with_elems(&db, &[mk_param_ty(0, "__p0"), mk_param_ty(0, "__p0")]);
        let actual = TyId::tuple_with_elems(&db, &[mk_param_ty(1, "__p1"), mk_param_ty(2, "__p2")]);
        let renamed = alpha_rename_hidden_layout_placeholders(&db, expected, actual);

        assert_ne!(renamed, actual);
    }
}
