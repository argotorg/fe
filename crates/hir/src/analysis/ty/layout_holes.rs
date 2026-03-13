use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    const_ty::{CallableInputLayoutHoleOrigin, ConstTyData, ConstTyId, LayoutHoleId},
    fold::{TyFoldable, TyFolder},
    ty_def::{TyData, TyId},
    ty_lower::func_implicit_param_plan,
    visitor::{TyVisitable, TyVisitor, walk_ty},
};
use crate::analysis::HirAnalysisDb;
use crate::hir_def::CallableDef;

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
        ConstTyId::hole_with_layout_id(db, layout_hole_fallback_ty(db, hole_ty), hole_id),
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

    let mut next = 0usize;
    substitute_layout_placeholders_with(db, value, policy, |placeholder| {
        let _ = placeholder;
        let arg = layout_args.get(next).copied();
        next += usize::from(arg.is_some());
        arg
    })
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

    substitute_layout_placeholders_with(db, value, policy, |placeholder| {
        layout_args.get(&placeholder).copied()
    })
}

fn substitute_layout_placeholders_with<'db, T, F>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    policy: LayoutPlaceholderPolicy,
    mut lookup: F,
) -> T
where
    T: TyFoldable<'db>,
    F: FnMut(TyId<'db>) -> Option<TyId<'db>>,
{
    struct LayoutPlaceholderSubst<'a, 'db, F> {
        db: &'db dyn HirAnalysisDb,
        policy: LayoutPlaceholderPolicy,
        lookup: &'a mut F,
    }

    impl<'a, 'db, F> TyFolder<'db> for LayoutPlaceholderSubst<'a, 'db, F>
    where
        F: FnMut(TyId<'db>) -> Option<TyId<'db>>,
    {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            if is_layout_placeholder(self.db, ty, self.policy)
                && let Some(arg) = (self.lookup)(ty)
            {
                return arg;
            }

            ty.super_fold_with(db, self)
        }

        fn fold_ty_app(
            &mut self,
            db: &'db dyn HirAnalysisDb,
            abs: TyId<'db>,
            arg: TyId<'db>,
        ) -> TyId<'db> {
            TyId::new(db, TyData::TyApp(abs, arg))
        }
    }

    let mut folder = LayoutPlaceholderSubst {
        db,
        policy,
        lookup: &mut lookup,
    };
    value.fold_with(db, &mut folder)
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

pub(crate) fn collect_unique_layout_placeholders_in_order_with_policy<'db, T>(
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
    collect_layout_placeholder_tys_in_order_with_policy(
        db,
        value,
        LayoutPlaceholderPolicy::HolesOnly,
    )
}

pub(crate) fn collect_layout_placeholder_tys_in_order_with_policy<'db, T>(
    db: &'db dyn HirAnalysisDb,
    value: T,
    policy: LayoutPlaceholderPolicy,
) -> Vec<TyId<'db>>
where
    T: TyVisitable<'db>,
{
    collect_layout_placeholders_in_order_with_policy(db, value, policy)
        .into_iter()
        .filter_map(|placeholder| {
            let TyData::ConstTy(const_ty) = placeholder.data(db) else {
                return None;
            };
            match const_ty.data(db) {
                ConstTyData::Hole(hole_ty, _) => Some(layout_hole_fallback_ty(db, *hole_ty)),
                ConstTyData::TyParam(param, ty) if param.is_implicit() => Some(*ty),
                _ => None,
            }
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

pub(crate) fn callable_input_layout_bindings_by_origin<'db>(
    db: &'db dyn HirAnalysisDb,
    method: CallableDef<'db>,
) -> FxHashMap<CallableInputLayoutHoleOrigin, Vec<(TyId<'db>, TyId<'db>)>> {
    let CallableDef::Func(func) = method else {
        return FxHashMap::default();
    };
    func_implicit_param_plan(db, func).bindings_by_origin
}

#[cfg(test)]
mod tests {
    use camino::Utf8PathBuf;
    use rustc_hash::FxHashMap;

    use super::{
        LayoutPlaceholderPolicy, alpha_rename_hidden_layout_placeholders,
        substitute_layout_placeholders_by_identity, substitute_layout_placeholders_in_order,
    };
    use crate::analysis::ty::{
        const_ty::{ConstTyData, ConstTyId},
        ty_def::{Kind, PrimTy, TyBase, TyData, TyId, TyParam},
    };
    use crate::hir_def::{IdentId, ItemKind, scope_graph::ScopeId};
    use crate::test_db::HirAnalysisTestDb;

    fn usize_ty<'db>(db: &'db HirAnalysisTestDb) -> TyId<'db> {
        TyId::new(db, TyData::TyBase(TyBase::Prim(PrimTy::Usize)))
    }

    fn mk_implicit_param_ty<'db>(
        db: &'db HirAnalysisTestDb,
        scope: ScopeId<'db>,
        idx: usize,
        name: &str,
    ) -> TyId<'db> {
        let param =
            TyParam::implicit_param(IdentId::new(db, name.to_string()), idx, Kind::Star, scope);
        TyId::const_ty(
            db,
            ConstTyId::new(db, ConstTyData::TyParam(param, usize_ty(db))),
        )
    }

    fn mk_hole_ty<'db>(db: &'db HirAnalysisTestDb) -> TyId<'db> {
        TyId::const_ty(db, ConstTyId::hole_with_ty(db, usize_ty(db)))
    }

    fn mk_array_with_len<'db>(db: &'db HirAnalysisTestDb, len: TyId<'db>) -> TyId<'db> {
        TyId::app(db, TyId::array(db, TyId::u256(db)), len)
    }

    #[test]
    fn substitute_layout_placeholders_in_order_replaces_repeated_holes_by_occurrence() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("layout_holes_test_scope.fe"), "fn f() {}");
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");
        let repeated_hole = mk_hole_ty(&db);
        let left = mk_implicit_param_ty(&db, scope, 0, "__left");
        let right = mk_implicit_param_ty(&db, scope, 1, "__right");
        let value = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, repeated_hole),
                mk_array_with_len(&db, repeated_hole),
            ],
        );

        let substituted = substitute_layout_placeholders_in_order(
            &db,
            value,
            &[left, right],
            LayoutPlaceholderPolicy::HolesOnly,
        );

        let fields = substituted.field_types(&db);
        assert_eq!(fields[0].generic_args(&db)[1], left);
        assert_eq!(fields[1].generic_args(&db)[1], right);
    }

    #[test]
    fn substitute_layout_placeholders_in_order_respects_policy_and_traversal_order() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("layout_holes_test_scope.fe"), "fn f() {}");
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");
        let implicit = mk_implicit_param_ty(&db, scope, 0, "__implicit");
        let hole = mk_hole_ty(&db);
        let implicit_replacement = mk_implicit_param_ty(&db, scope, 1, "__implicit_replacement");
        let hole_replacement = mk_implicit_param_ty(&db, scope, 2, "__hole_replacement");
        let value = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, implicit),
                mk_array_with_len(&db, hole),
            ],
        );

        let holes_only = substitute_layout_placeholders_in_order(
            &db,
            value,
            &[hole_replacement],
            LayoutPlaceholderPolicy::HolesOnly,
        );
        let holes_and_implicit = substitute_layout_placeholders_in_order(
            &db,
            value,
            &[implicit_replacement, hole_replacement],
            LayoutPlaceholderPolicy::HolesAndImplicitParams,
        );

        let holes_only_fields = holes_only.field_types(&db);
        assert_eq!(holes_only_fields[0].generic_args(&db)[1], implicit);
        assert_eq!(holes_only_fields[1].generic_args(&db)[1], hole_replacement);
        let holes_and_implicit_fields = holes_and_implicit.field_types(&db);
        assert_eq!(
            holes_and_implicit_fields[0].generic_args(&db)[1],
            implicit_replacement
        );
        assert_eq!(
            holes_and_implicit_fields[1].generic_args(&db)[1],
            hole_replacement
        );
    }

    #[test]
    fn substitute_layout_placeholders_by_identity_reuses_mapped_placeholder_identity() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("layout_holes_test_scope.fe"), "fn f() {}");
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");
        let repeated_hole = mk_hole_ty(&db);
        let replacement = mk_implicit_param_ty(&db, scope, 0, "__replacement");
        let value = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, repeated_hole),
                mk_array_with_len(&db, repeated_hole),
            ],
        );
        let layout_args = FxHashMap::from_iter([(repeated_hole, replacement)]);

        let substituted = substitute_layout_placeholders_by_identity(
            &db,
            value,
            &layout_args,
            LayoutPlaceholderPolicy::HolesOnly,
        );

        let fields = substituted.field_types(&db);
        assert_eq!(fields[0].generic_args(&db)[1], replacement);
        assert_eq!(fields[1].generic_args(&db)[1], replacement);
    }

    #[test]
    fn substitute_layout_placeholders_by_identity_leaves_unmatched_placeholders_and_respects_policy()
     {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(Utf8PathBuf::from("layout_holes_test_scope.fe"), "fn f() {}");
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");
        let implicit = mk_implicit_param_ty(&db, scope, 0, "__implicit");
        let hole = mk_hole_ty(&db);
        let unmatched_implicit = mk_implicit_param_ty(&db, scope, 3, "__unmatched_implicit");
        let implicit_replacement = mk_implicit_param_ty(&db, scope, 1, "__implicit_replacement");
        let hole_replacement = mk_implicit_param_ty(&db, scope, 2, "__hole_replacement");
        let value = TyId::tuple_with_elems(
            &db,
            &[
                mk_array_with_len(&db, implicit),
                mk_array_with_len(&db, hole),
                mk_array_with_len(&db, unmatched_implicit),
            ],
        );
        let layout_args =
            FxHashMap::from_iter([(implicit, implicit_replacement), (hole, hole_replacement)]);

        let holes_only = substitute_layout_placeholders_by_identity(
            &db,
            value,
            &layout_args,
            LayoutPlaceholderPolicy::HolesOnly,
        );
        let holes_and_implicit = substitute_layout_placeholders_by_identity(
            &db,
            value,
            &layout_args,
            LayoutPlaceholderPolicy::HolesAndImplicitParams,
        );

        let holes_only_fields = holes_only.field_types(&db);
        assert_eq!(holes_only_fields[0].generic_args(&db)[1], implicit);
        assert_eq!(holes_only_fields[1].generic_args(&db)[1], hole_replacement);
        assert_eq!(
            holes_only_fields[2].generic_args(&db)[1],
            unmatched_implicit
        );
        let holes_and_implicit_fields = holes_and_implicit.field_types(&db);
        assert_eq!(
            holes_and_implicit_fields[0].generic_args(&db)[1],
            implicit_replacement
        );
        assert_eq!(
            holes_and_implicit_fields[1].generic_args(&db)[1],
            hole_replacement
        );
        assert_eq!(
            holes_and_implicit_fields[2].generic_args(&db)[1],
            unmatched_implicit
        );
    }

    #[test]
    fn alpha_rename_preserves_repeated_placeholder_identity() {
        let mut db = HirAnalysisTestDb::default();
        let file = db.new_stand_alone(
            Utf8PathBuf::from("alpha_rename_preserves_repeated_placeholder_identity.fe"),
            "fn f() {}",
        );
        let (top_mod, _) = db.top_mod(file);
        let scope = top_mod
            .children_non_nested(&db)
            .find_map(|item| match item {
                ItemKind::Func(func) => Some(func.scope()),
                _ => None,
            })
            .expect("missing `f` function");

        let expected = TyId::tuple_with_elems(
            &db,
            &[
                mk_implicit_param_ty(&db, scope, 0, "__p0"),
                mk_implicit_param_ty(&db, scope, 0, "__p0"),
            ],
        );
        let actual = TyId::tuple_with_elems(
            &db,
            &[
                mk_implicit_param_ty(&db, scope, 1, "__p1"),
                mk_implicit_param_ty(&db, scope, 2, "__p2"),
            ],
        );
        let renamed = alpha_rename_hidden_layout_placeholders(&db, expected, actual);

        assert_ne!(renamed, actual);
    }
}
