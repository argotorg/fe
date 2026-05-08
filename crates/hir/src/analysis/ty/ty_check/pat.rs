use std::ops::Range;

use crate::core::hir_def::{
    IdentId, LitKind, Partial, Pat, PatId, PathId, TupleTypeId, VariantKind,
};
use either::Either;

use super::{ConstRef, RecordLike, TyChecker, env::LocalBinding, path::RecordInitChecker};
use crate::analysis::{
    name_resolution::{ExpectedPathKind, PathRes, ResolvedVariant},
    semantic::{
        SemConstScalar, SemConstValue, SemOrigin, eval_const_ref,
        instance::resolve_semantic_const_ref,
    },
    ty::adt_def::AdtRef,
    ty::{
        assoc_const::AssocConstUse,
        binder::Binder,
        diagnostics::{BodyDiag, TraitConstraintDiag, TyDiagCollection},
        fold::TyFoldable,
        pattern_ir::{
            BindingRef, ConstructorKind, PatternAnalysisStatus, ValidatedPat, ValidatedPatKind,
        },
        pattern_types::pattern_match_expected_ty,
        trait_def::TraitInstId,
        trait_resolution::{GoalSatisfiability, TraitSolveCx, is_goal_satisfiable},
        ty_def::{InvalidCause, Kind, TyId, TyVarSort},
        ty_lower::lower_hir_ty,
    },
};

enum TupleVariantResolution<'db> {
    Resolved(ResolvedVariant<'db>, TupleTypeId<'db>),
    Invalid,
    UnresolvedPath,
}

struct UnpackedRestPat<'db> {
    elem_tys: Vec<TyId<'db>>,
    rest_range: Range<usize>,
    is_valid: bool,
}

pub(super) struct PatCheckResult<'db> {
    pub(super) ty: TyId<'db>,
    pub(super) analysis: PatternAnalysisStatus,
}

impl<'db> TyChecker<'db> {
    pub(super) fn check_pat(&mut self, pat: PatId, expected: TyId<'db>) -> PatCheckResult<'db> {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return self.finish_pat_check(
                pat,
                expected,
                TyId::invalid(self.db, InvalidCause::ParseError),
                PatternAnalysisStatus::Invalid,
            );
        };

        match pat_data {
            Pat::WildCard => {
                let ty_var = self.table.new_var(TyVarSort::General, &Kind::Star);
                let analysis = self.ready_wildcard(expected, None);
                self.finish_pat_check(pat, expected, ty_var, analysis)
            }

            Pat::Rest => {
                self.push_diag(BodyDiag::UnexpectedRestPat(pat.span(self.body()).into()));
                self.finish_pat_check(
                    pat,
                    expected,
                    TyId::invalid(self.db, InvalidCause::Other),
                    PatternAnalysisStatus::Invalid,
                )
            }
            Pat::Lit(..) => self.check_lit_pat(pat, pat_data, expected),
            Pat::Tuple(..) => self.check_tuple_pat(pat, pat_data, expected),
            Pat::Path(..) => self.check_path_pat(pat, pat_data, expected),
            Pat::PathTuple(..) => self.check_path_tuple_pat(pat, pat_data, expected),
            Pat::Record(..) => self.check_record_pat(pat, pat_data, expected),

            Pat::Or(lhs_pat, rhs_pat) => {
                let lhs = self.check_pat(*lhs_pat, expected);
                let rhs = self.check_pat(*rhs_pat, expected);
                let analysis =
                    if self.pattern_binds_any(*lhs_pat) || self.pattern_binds_any(*rhs_pat) {
                        self.push_diag(BodyDiag::BindingsInOrPat(pat.span(self.body()).into()));
                        self.discard_local_bindings_in_pat(*lhs_pat);
                        self.discard_local_bindings_in_pat(*rhs_pat);
                        PatternAnalysisStatus::Invalid
                    } else {
                        self.ready_or(expected, [lhs.analysis, rhs.analysis])
                    };
                self.finish_pat_check(pat, expected, rhs.ty, analysis)
            }
        }
    }

    fn finish_pat_check(
        &mut self,
        pat: PatId,
        expected: TyId<'db>,
        actual: TyId<'db>,
        analysis: PatternAnalysisStatus,
    ) -> PatCheckResult<'db> {
        let ty = self.unify_ty(pat, actual, expected);
        let analysis = if ty.has_invalid(self.db) {
            PatternAnalysisStatus::Invalid
        } else {
            analysis
        };
        self.env.set_pattern_status(pat, analysis);
        PatCheckResult { ty, analysis }
    }

    fn ready_wildcard(
        &mut self,
        ty: TyId<'db>,
        binding: Option<BindingRef<'db>>,
    ) -> PatternAnalysisStatus {
        PatternAnalysisStatus::Ready(self.env.alloc_validated_pat(ValidatedPat::new(
            ty,
            ValidatedPatKind::Wildcard { binding },
        )))
    }

    fn ready_constructor(
        &mut self,
        ty: TyId<'db>,
        ctor: ConstructorKind<'db>,
        fields: Vec<PatternAnalysisStatus>,
    ) -> PatternAnalysisStatus {
        let fields = match self.collect_ready_roots(fields) {
            Ok(fields) => fields,
            Err(status) => return status,
        };
        PatternAnalysisStatus::Ready(self.env.alloc_validated_pat(ValidatedPat::new(
            ty,
            ValidatedPatKind::Constructor { ctor, fields },
        )))
    }

    fn ready_or(
        &mut self,
        ty: TyId<'db>,
        pats: [PatternAnalysisStatus; 2],
    ) -> PatternAnalysisStatus {
        let pats = match self.collect_ready_roots(pats) {
            Ok(pats) => pats,
            Err(status) => return status,
        };
        PatternAnalysisStatus::Ready(
            self.env
                .alloc_validated_pat(ValidatedPat::new(ty, ValidatedPatKind::Or(pats))),
        )
    }

    fn collect_ready_roots(
        &self,
        statuses: impl IntoIterator<Item = PatternAnalysisStatus>,
    ) -> Result<Vec<crate::analysis::ty::pattern_ir::ValidatedPatId>, PatternAnalysisStatus> {
        let mut roots = Vec::new();
        let mut unsupported = false;
        for status in statuses {
            match status {
                PatternAnalysisStatus::Ready(root) => roots.push(root),
                PatternAnalysisStatus::Invalid => return Err(PatternAnalysisStatus::Invalid),
                PatternAnalysisStatus::Unsupported => unsupported = true,
            }
        }
        if unsupported {
            Err(PatternAnalysisStatus::Unsupported)
        } else {
            Ok(roots)
        }
    }

    fn binding_ref(&self, pat: PatId, name: IdentId<'db>) -> BindingRef<'db> {
        BindingRef {
            name,
            representative_pat: pat,
        }
    }

    fn canonical_pattern_ty(&mut self, resolved_ty: TyId<'db>, expected: TyId<'db>) -> TyId<'db> {
        let resolved_ty = resolved_ty.fold_with(self.db, &mut self.table);
        let resolved_ty = self.normalize_ty(resolved_ty);
        let expected = expected.fold_with(self.db, &mut self.table);
        let expected = self.normalize_ty(expected);
        if !expected.has_invalid(self.db) {
            let (expected_base, _) = expected.decompose_ty_app(self.db);
            let (resolved_base, _) = resolved_ty.decompose_ty_app(self.db);
            if expected_base == resolved_base {
                return expected;
            }
        }
        resolved_ty
    }

    fn type_constructor_kind(
        &mut self,
        resolved_ty: TyId<'db>,
        expected: TyId<'db>,
    ) -> ConstructorKind<'db> {
        ConstructorKind::Type(self.canonical_pattern_ty(resolved_ty, expected))
    }

    fn literal_constructor_status(
        &mut self,
        ty: TyId<'db>,
        lit: LitKind<'db>,
    ) -> PatternAnalysisStatus {
        self.ready_constructor(ty, ConstructorKind::Literal(lit, ty), Vec::new())
    }

    fn eval_const_pattern_literal(
        &self,
        cref: ConstRef<'db>,
        expected: TyId<'db>,
    ) -> Option<LitKind<'db>> {
        let expected = expected
            .as_capability(self.db)
            .map_or(expected, |(_, inner)| inner);
        let cref = resolve_semantic_const_ref(self.db, cref, expected, SemOrigin::Synthetic)?;
        match eval_const_ref(self.db, cref).ok()?.value(self.db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => {
                let (_, bytes) = value.to_bytes_be();
                Some(LitKind::Int(crate::hir_def::IntegerId::new(
                    self.db,
                    num_bigint::BigUint::from_bytes_be(&bytes),
                )))
            }
            SemConstValue::Scalar {
                value: SemConstScalar::Bool(flag),
                ..
            } => Some(LitKind::Bool(flag)),
            SemConstValue::Unit
            | SemConstValue::TypeLevel { .. }
            | SemConstValue::Tuple { .. }
            | SemConstValue::Struct { .. }
            | SemConstValue::Array { .. }
            | SemConstValue::Enum { .. }
            | SemConstValue::Scalar {
                value: SemConstScalar::Bytes(_),
                ..
            } => None,
        }
    }

    fn consume_rest_pat(&mut self, pat: PatId, ty: TyId<'db>) {
        self.env.type_pat(pat, ty);
        self.env
            .set_pattern_status(pat, PatternAnalysisStatus::Invalid);
    }

    fn discard_local_bindings_in_pat(&mut self, pat: PatId) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            return;
        };

        match pat_data {
            Pat::Path(..) => self.env.discard_pat_binding(pat),
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for pat in pats {
                    self.discard_local_bindings_in_pat(*pat);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.discard_local_bindings_in_pat(field.pat);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.discard_local_bindings_in_pat(*lhs);
                self.discard_local_bindings_in_pat(*rhs);
            }
            Pat::WildCard | Pat::Rest | Pat::Lit(..) => {}
        }
    }

    fn check_lit_pat(
        &mut self,
        pat: PatId,
        pat_data: &Pat<'db>,
        expected: TyId<'db>,
    ) -> PatCheckResult<'db> {
        let Pat::Lit(lit) = pat_data else {
            unreachable!()
        };

        match lit {
            Partial::Present(lit) => {
                let lit_ty = self.lit_ty(lit);
                let analysis = self.literal_constructor_status(expected, *lit);
                self.finish_pat_check(pat, expected, lit_ty, analysis)
            }
            Partial::Absent => self.finish_pat_check(
                pat,
                expected,
                TyId::invalid(self.db, InvalidCause::ParseError),
                PatternAnalysisStatus::Invalid,
            ),
        }
    }

    fn check_tuple_pat(
        &mut self,
        pat: PatId,
        pat_data: &Pat<'db>,
        expected: TyId<'db>,
    ) -> PatCheckResult<'db> {
        let Pat::Tuple(pat_tup) = pat_data else {
            unreachable!()
        };

        let expected_len = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_tuple(self.db) => Some(args.len()),
            _ => None,
        };
        let UnpackedRestPat {
            elem_tys,
            rest_range,
            is_valid,
        } = self.unpack_rest_pat(pat_tup, expected_len);
        let actual = TyId::tuple_with_elems(self.db, &elem_tys);

        let unified = self.equate_ty(actual, expected, pat.span(self.body()).into());
        if unified.has_invalid(self.db) {
            self.check_tuple_like_pattern_elems(pat_tup, &[], Range::default(), None);
            return self.finish_pat_check(pat, expected, unified, PatternAnalysisStatus::Invalid);
        }

        let elem_tys = unified.decompose_ty_app(self.db).1.to_vec();
        let fields = self.check_tuple_like_pattern_elems(pat_tup, &elem_tys, rest_range, None);
        let ctor = self.type_constructor_kind(unified, expected);
        let analysis = if is_valid {
            self.ready_constructor(expected, ctor, fields)
        } else {
            PatternAnalysisStatus::Invalid
        };
        self.finish_pat_check(pat, expected, unified, analysis)
    }

    fn check_path_pat(
        &mut self,
        pat: PatId,
        pat_data: &Pat<'db>,
        expected: TyId<'db>,
    ) -> PatCheckResult<'db> {
        let Pat::Path(path, is_mut) = pat_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return self.finish_pat_check(
                pat,
                expected,
                TyId::invalid(self.db, InvalidCause::ParseError),
                PatternAnalysisStatus::Invalid,
            );
        };

        if let Some(expected) = self.expected_msg_variant_for_named_recv_pat(pat, *path, expected) {
            if !expected.field_types(self.db).is_empty() {
                let record_like = RecordLike::from_ty(expected);
                let actual = self.emit_unit_variant_expected(pat, record_like);
                return self.finish_pat_check(
                    pat,
                    expected,
                    actual,
                    PatternAnalysisStatus::Invalid,
                );
            }
            let analysis =
                self.ready_constructor(expected, ConstructorKind::Type(expected), Vec::new());
            return self.finish_pat_check(pat, expected, expected, analysis);
        }

        let span = pat.span(self.body()).into_path_pat();
        let res = self.resolve_path(*path, true, span.clone().path());

        // Bare identifiers that don't resolve to a type/variant are local bindings,
        // unless the expected type is a msg type, in which case we try to resolve
        // the identifier as a msg variant first.
        if let Some(name) = path.as_ident(self.db)
            && matches!(
                res,
                Err(_)
                    | Ok(PathRes::Trait(_)
                        | PathRes::Mod(_)
                        | PathRes::Func(_)
                        | PathRes::TraitMethod(..)
                        | PathRes::Method(..)
                        | PathRes::FuncParam(..))
            )
        {
            let binding = LocalBinding::local(pat, *is_mut);
            let mut is_valid = true;
            if let Some(LocalBinding::Local {
                pat: conflict_with, ..
            }) = self.env.register_pending_binding(name, binding)
            {
                let diag = BodyDiag::DuplicatedBinding {
                    primary: span.into(),
                    conflicat_with: conflict_with.span(self.body()).into(),
                    name,
                };
                self.push_diag(diag);
                is_valid = false;
            }
            let actual = self.fresh_ty();
            let analysis = if is_valid {
                self.ready_wildcard(expected, Some(self.binding_ref(pat, name)))
            } else {
                PatternAnalysisStatus::Invalid
            };
            return self.finish_pat_check(pat, expected, actual, analysis);
        }

        let (actual, analysis) = match res {
            Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty) | PathRes::Func(ty)) => {
                let record_like = RecordLike::from_ty(ty);
                if record_like.is_record(self.db) {
                    (
                        self.emit_unit_variant_expected(pat, record_like),
                        PatternAnalysisStatus::Invalid,
                    )
                } else {
                    {
                        let ctor = self.type_constructor_kind(ty, expected);
                        (ty, self.ready_constructor(expected, ctor, Vec::new()))
                    }
                }
            }

            Ok(PathRes::Const(const_def, ty)) => (
                ty,
                self.eval_const_pattern_literal(ConstRef::Const(const_def), expected)
                    .map(|lit| self.literal_constructor_status(expected, lit))
                    .unwrap_or(PatternAnalysisStatus::Unsupported),
            ),

            Ok(PathRes::TraitConst(recv_ty, inst, name)) => {
                let mut args = inst.args(self.db).clone();
                if let Some(self_arg) = args.first_mut() {
                    *self_arg = recv_ty;
                }
                let inst = TraitInstId::new(
                    self.db,
                    inst.def(self.db),
                    args,
                    inst.assoc_type_bindings(self.db).clone(),
                );

                let trait_ = inst.def(self.db);
                if !super::trait_const_goal_has_foreign_params(self.db, inst, self.env.scope())
                    && let GoalSatisfiability::UnSat(_) = is_goal_satisfiable(
                        self.db,
                        TraitSolveCx::new(self.db, self.env.scope())
                            .with_assumptions(self.env.assumptions()),
                        inst,
                    )
                {
                    self.push_diag(TyDiagCollection::from(
                        TraitConstraintDiag::TraitBoundNotSat {
                            span: span.clone().into(),
                            primary_goal: inst,
                            unsat_subgoal: None,
                            required_by: None,
                        },
                    ));
                    return self.finish_pat_check(
                        pat,
                        expected,
                        TyId::invalid(self.db, InvalidCause::Other),
                        PatternAnalysisStatus::Invalid,
                    );
                }

                if let Some(const_view) = trait_.const_(self.db, name)
                    && let Some(ty_binder) = const_view.ty_binder(self.db)
                {
                    let instantiated = ty_binder.instantiate(self.db, inst.args(self.db));
                    let ty = self.table.instantiate_to_term(instantiated);
                    let cref = ConstRef::TraitConst(AssocConstUse::new(
                        self.env.scope(),
                        self.env.assumptions(),
                        inst,
                        name,
                    ));
                    (
                        ty,
                        self.eval_const_pattern_literal(cref, expected)
                            .map(|lit| self.literal_constructor_status(expected, lit))
                            .unwrap_or(PatternAnalysisStatus::Unsupported),
                    )
                } else {
                    (
                        TyId::invalid(self.db, InvalidCause::Other),
                        PatternAnalysisStatus::Invalid,
                    )
                }
            }

            Ok(PathRes::Trait(trait_)) => {
                let diag = BodyDiag::NotValue {
                    primary: span.into(),
                    given: Either::Left(trait_.def(self.db).into()),
                };
                self.push_diag(diag);
                (
                    TyId::invalid(self.db, InvalidCause::Other),
                    PatternAnalysisStatus::Invalid,
                )
            }

            Ok(PathRes::EnumVariant(variant)) => {
                if matches!(variant.kind(self.db), VariantKind::Unit) {
                    let ty = self.table.instantiate_to_term(variant.ty);
                    let semantic_ty = self.equate_ty(ty, expected, pat.span(self.body()).into());
                    let semantic_ty = if semantic_ty.has_invalid(self.db) {
                        semantic_ty
                    } else {
                        self.canonical_pattern_ty(semantic_ty, expected)
                    };
                    (
                        semantic_ty,
                        if semantic_ty.has_invalid(self.db) {
                            PatternAnalysisStatus::Invalid
                        } else {
                            self.ready_constructor(
                                semantic_ty,
                                ConstructorKind::Variant(variant.variant, semantic_ty),
                                Vec::new(),
                            )
                        },
                    )
                } else {
                    (
                        self.emit_unit_variant_expected(pat, RecordLike::from_variant(variant)),
                        PatternAnalysisStatus::Invalid,
                    )
                }
            }

            Ok(PathRes::Mod(scope_id)) => {
                let diag = BodyDiag::NotValue {
                    primary: span.into(),
                    given: Either::Left(scope_id.item()),
                };
                self.push_diag(diag);
                (
                    TyId::invalid(self.db, InvalidCause::Other),
                    PatternAnalysisStatus::Invalid,
                )
            }

            Ok(PathRes::TraitMethod(..) | PathRes::Method(..) | PathRes::FuncParam(..)) => (
                TyId::invalid(self.db, InvalidCause::Other),
                PatternAnalysisStatus::Invalid,
            ),

            Err(err) => {
                if let Some(diag) =
                    err.into_diag(self.db, *path, span.clone().path(), ExpectedPathKind::Pat)
                {
                    self.push_diag(diag);
                }
                (
                    TyId::invalid(self.db, InvalidCause::Other),
                    PatternAnalysisStatus::Invalid,
                )
            }
        };

        self.finish_pat_check(pat, expected, actual, analysis)
    }

    fn emit_unit_variant_expected(
        &mut self,
        pat: PatId,
        record_like: RecordLike<'db>,
    ) -> TyId<'db> {
        let diag =
            BodyDiag::unit_variant_expected(self.db, pat.span(self.body()).into(), record_like);
        self.push_diag(diag);
        TyId::invalid(self.db, InvalidCause::Other)
    }

    fn check_path_tuple_pat(
        &mut self,
        pat: PatId,
        pat_data: &Pat<'db>,
        expected: TyId<'db>,
    ) -> PatCheckResult<'db> {
        let Pat::PathTuple(Partial::Present(path), elems) = pat_data else {
            return self.finish_pat_check(
                pat,
                expected,
                TyId::invalid(self.db, InvalidCause::ParseError),
                PatternAnalysisStatus::Invalid,
            );
        };

        let (variant, expected_elems) = match self.resolve_tuple_variant_pat(pat, *path) {
            TupleVariantResolution::Resolved(variant, expected_elems) => (variant, expected_elems),
            TupleVariantResolution::Invalid => {
                self.check_tuple_like_pattern_elems(elems, &[], Range::default(), None);
                return self.finish_pat_check(
                    pat,
                    TyId::invalid(self.db, InvalidCause::Other),
                    TyId::invalid(self.db, InvalidCause::Other),
                    PatternAnalysisStatus::Invalid,
                );
            }
            TupleVariantResolution::UnresolvedPath => {
                self.check_tuple_like_pattern_elems(elems, &[], Range::default(), None);
                return self.finish_pat_check(
                    pat,
                    TyId::invalid(self.db, InvalidCause::Other),
                    TyId::invalid(self.db, InvalidCause::Other),
                    PatternAnalysisStatus::Invalid,
                );
            }
        };

        let semantic_ty = self.equate_ty(variant.ty, expected, pat.span(self.body()).into());
        let semantic_ty = if semantic_ty.has_invalid(self.db) {
            semantic_ty
        } else {
            self.canonical_pattern_ty(semantic_ty, expected)
        };
        let variant_ty = if semantic_ty.has_invalid(self.db) {
            self.canonical_pattern_ty(variant.ty, expected)
        } else {
            semantic_ty
        };
        let expected_len = expected_elems.len(self.db);
        let UnpackedRestPat {
            elem_tys: actual_elems,
            rest_range,
            is_valid,
        } = self.unpack_rest_pat(elems, Some(expected_len));
        let elem_tys = self.instantiate_tuple_variant_elem_tys(variant, variant_ty, expected_elems);
        let fields = self.check_tuple_like_pattern_elems(
            elems,
            &elem_tys,
            rest_range.clone(),
            Some(variant_ty),
        );
        if actual_elems.len() != expected_len {
            let diag = BodyDiag::MismatchedFieldCount {
                primary: pat.span(self.body()).into(),
                expected: expected_len,
                given: actual_elems.len(),
            };

            self.push_diag(diag);
            return self.finish_pat_check(
                pat,
                expected,
                semantic_ty,
                PatternAnalysisStatus::Invalid,
            );
        }

        let analysis = if semantic_ty.has_invalid(self.db) || !is_valid {
            PatternAnalysisStatus::Invalid
        } else {
            self.ready_constructor(
                semantic_ty,
                ConstructorKind::Variant(variant.variant, semantic_ty),
                fields,
            )
        };
        self.finish_pat_check(pat, expected, semantic_ty, analysis)
    }

    fn resolve_tuple_variant_pat(
        &mut self,
        pat: PatId,
        path: PathId<'db>,
    ) -> TupleVariantResolution<'db> {
        let span = pat.span(self.body()).into_path_tuple_pat();
        match self.resolve_path(path, true, span.clone().path()) {
            Ok(res) => match res {
                PathRes::Ty(ty)
                | PathRes::TyAlias(_, ty)
                | PathRes::Func(ty)
                | PathRes::Const(_, ty)
                | PathRes::TraitConst(ty, ..) => {
                    self.push_diag(BodyDiag::tuple_variant_expected(
                        self.db,
                        pat.span(self.body()).into(),
                        Some(RecordLike::Type(ty)),
                    ));
                    TupleVariantResolution::Invalid
                }
                PathRes::Trait(trait_) => {
                    self.push_diag(BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(trait_.def(self.db).into()),
                    });
                    TupleVariantResolution::Invalid
                }
                PathRes::EnumVariant(variant) => match variant.kind(self.db) {
                    VariantKind::Tuple(elems) => TupleVariantResolution::Resolved(variant, elems),
                    _ => {
                        self.push_diag(BodyDiag::tuple_variant_expected(
                            self.db,
                            pat.span(self.body()).into(),
                            Some(RecordLike::from_variant(variant)),
                        ));
                        TupleVariantResolution::Invalid
                    }
                },
                PathRes::Mod(scope) => {
                    self.push_diag(BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(scope.item()),
                    });
                    TupleVariantResolution::Invalid
                }
                PathRes::TraitMethod(..) | PathRes::Method(..) | PathRes::FuncParam(..) => {
                    self.push_diag(BodyDiag::tuple_variant_expected(self.db, span.into(), None));
                    TupleVariantResolution::Invalid
                }
            },
            Err(err) => {
                if !path.is_bare_ident(self.db)
                    && let Some(diag) =
                        err.into_diag(self.db, path, span.clone().path(), ExpectedPathKind::Pat)
                {
                    self.push_diag(diag);
                }
                TupleVariantResolution::UnresolvedPath
            }
        }
    }

    fn instantiate_tuple_variant_elem_tys(
        &mut self,
        variant: ResolvedVariant<'db>,
        variant_ty: TyId<'db>,
        elems: TupleTypeId<'db>,
    ) -> Vec<TyId<'db>> {
        elems
            .data(self.db)
            .iter()
            .map(|hir_ty| match hir_ty.to_opt() {
                Some(ty) => {
                    let ty = lower_hir_ty(
                        self.db,
                        ty,
                        variant.enum_(self.db).scope(),
                        self.env.assumptions(),
                    );
                    let instantiated =
                        Binder::bind(ty).instantiate(self.db, variant_ty.generic_args(self.db));
                    self.normalize_ty(instantiated)
                }
                None => TyId::invalid(self.db, InvalidCause::ParseError),
            })
            .collect()
    }

    fn check_tuple_like_pattern_elems(
        &mut self,
        source_pats: &[PatId],
        elem_tys: &[TyId<'db>],
        rest_range: Range<usize>,
        rest_expected: Option<TyId<'db>>,
    ) -> Vec<PatternAnalysisStatus> {
        let mut analyses = Vec::with_capacity(elem_tys.len());
        let mut pat_idx = 0;
        for (i, &elem_ty) in elem_tys.iter().enumerate() {
            while pat_idx < source_pats.len() && source_pats[pat_idx].is_rest(self.db, self.body())
            {
                let rest_ty =
                    rest_expected.unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other));
                self.consume_rest_pat(source_pats[pat_idx], rest_ty);
                pat_idx += 1;
            }
            if rest_range.contains(&i) {
                analyses
                    .push(self.ready_wildcard(pattern_match_expected_ty(self.db, elem_ty), None));
                continue;
            }
            if pat_idx >= source_pats.len() {
                analyses
                    .push(self.ready_wildcard(pattern_match_expected_ty(self.db, elem_ty), None));
                continue;
            }

            let pat = source_pats[pat_idx];
            let (pat_expected, mode) = self.destructure_source_mode(elem_ty);
            let result = self.check_pat(pat, pat_expected);
            if let super::PatternDestructureMode::Borrow(kind) = mode {
                self.retype_pattern_bindings_for_borrow(pat, kind);
            }
            analyses.push(result.analysis);
            pat_idx += 1;
        }

        while pat_idx < source_pats.len() {
            let pat = source_pats[pat_idx];
            if pat.is_rest(self.db, self.body()) {
                let rest_ty =
                    rest_expected.unwrap_or_else(|| TyId::invalid(self.db, InvalidCause::Other));
                self.consume_rest_pat(pat, rest_ty);
            } else {
                self.check_pat(pat, TyId::invalid(self.db, InvalidCause::Other));
            }
            pat_idx += 1;
        }

        analyses
    }

    fn check_record_pat(
        &mut self,
        pat: PatId,
        pat_data: &Pat<'db>,
        expected: TyId<'db>,
    ) -> PatCheckResult<'db> {
        let Pat::Record(Partial::Present(path), _) = pat_data else {
            return self.finish_pat_check(
                pat,
                expected,
                TyId::invalid(self.db, InvalidCause::ParseError),
                PatternAnalysisStatus::Invalid,
            );
        };

        let span = pat.span(self.body()).into_record_pat();

        if let Some(expected) = self.expected_msg_variant_for_named_recv_pat(pat, *path, expected) {
            let record_like = RecordLike::from_ty(expected);
            if record_like.is_record(self.db) {
                let analysis = self.check_record_pat_fields(record_like, pat, expected);
                return self.finish_pat_check(pat, expected, expected, analysis);
            }

            let diag =
                BodyDiag::record_expected(self.db, pat.span(self.body()).into(), Some(record_like));
            self.push_diag(diag);
            return self.finish_pat_check(
                pat,
                expected,
                TyId::invalid(self.db, InvalidCause::Other),
                PatternAnalysisStatus::Invalid,
            );
        }

        let (actual, analysis) = match self.resolve_path(*path, true, span.clone().path()) {
            Ok(reso) => match reso {
                PathRes::Ty(ty) | PathRes::TyAlias(_, ty)
                    if RecordLike::from_ty(ty).is_record(self.db) =>
                {
                    let semantic_ty = self.equate_ty(ty, expected, pat.span(self.body()).into());
                    let semantic_ty = if semantic_ty.has_invalid(self.db) {
                        semantic_ty
                    } else {
                        self.canonical_pattern_ty(semantic_ty, expected)
                    };
                    let record_like = if semantic_ty.has_invalid(self.db) {
                        RecordLike::Type(ty)
                    } else {
                        RecordLike::Type(semantic_ty)
                    };
                    (
                        semantic_ty,
                        self.check_record_pat_fields(record_like, pat, semantic_ty),
                    )
                }

                PathRes::Ty(ty)
                | PathRes::TyAlias(_, ty)
                | PathRes::Func(ty)
                | PathRes::Const(_, ty)
                | PathRes::TraitConst(ty, ..) => {
                    let diag = BodyDiag::record_expected(
                        self.db,
                        pat.span(self.body()).into(),
                        Some(RecordLike::Type(ty)),
                    );
                    self.push_diag(diag);
                    (
                        TyId::invalid(self.db, InvalidCause::Other),
                        PatternAnalysisStatus::Invalid,
                    )
                }

                PathRes::Trait(trait_) => {
                    let diag = BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(trait_.def(self.db).into()),
                    };
                    self.push_diag(diag);
                    (
                        TyId::invalid(self.db, InvalidCause::Other),
                        PatternAnalysisStatus::Invalid,
                    )
                }

                PathRes::EnumVariant(variant) => {
                    let ty = variant.ty;
                    let record_like = RecordLike::from_variant(variant);
                    if record_like.is_record(self.db) {
                        let semantic_ty =
                            self.equate_ty(ty, expected, pat.span(self.body()).into());
                        let semantic_ty = if semantic_ty.has_invalid(self.db) {
                            semantic_ty
                        } else {
                            self.canonical_pattern_ty(semantic_ty, expected)
                        };
                        let record_like = if semantic_ty.has_invalid(self.db) {
                            record_like
                        } else {
                            RecordLike::EnumVariant(ResolvedVariant {
                                ty: semantic_ty,
                                ..variant
                            })
                        };
                        (
                            semantic_ty,
                            self.check_record_pat_fields(record_like, pat, semantic_ty),
                        )
                    } else {
                        let diag = BodyDiag::record_expected(
                            self.db,
                            pat.span(self.body()).into(),
                            Some(record_like),
                        );
                        self.push_diag(diag);
                        (
                            TyId::invalid(self.db, InvalidCause::Other),
                            PatternAnalysisStatus::Invalid,
                        )
                    }
                }
                PathRes::Mod(scope) => {
                    let diag = BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(scope.item()),
                    };
                    self.push_diag(diag);
                    (
                        TyId::invalid(self.db, InvalidCause::Other),
                        PatternAnalysisStatus::Invalid,
                    )
                }

                PathRes::TraitMethod(..) | PathRes::Method(..) | PathRes::FuncParam(..) => {
                    let diag =
                        BodyDiag::record_expected(self.db, pat.span(self.body()).into(), None);
                    self.push_diag(diag);
                    (
                        TyId::invalid(self.db, InvalidCause::Other),
                        PatternAnalysisStatus::Invalid,
                    )
                }
            },
            Err(err) => {
                // Check if expected type is a struct from a desugared msg module
                // that matches the pattern path
                if let Some(adt_def) = expected.adt_def(self.db)
                    && let AdtRef::Struct(struct_) = adt_def.adt_ref(self.db)
                {
                    // Check if the path matches the struct name
                    if let Some(struct_name) = struct_.name(self.db).to_opt()
                        && path.as_ident(self.db) == Some(struct_name)
                    {
                        // The pattern matches the expected struct type
                        let record_like = RecordLike::from_ty(expected);
                        if record_like.is_record(self.db) {
                            let analysis = self.check_record_pat_fields(record_like, pat, expected);
                            return self.finish_pat_check(pat, expected, expected, analysis);
                        }
                    }
                }

                if !path.is_bare_ident(self.db)
                    && let Some(diag) =
                        err.into_diag(self.db, *path, span.clone().path(), ExpectedPathKind::Pat)
                {
                    self.push_diag(diag);
                }
                (
                    TyId::invalid(self.db, InvalidCause::Other),
                    PatternAnalysisStatus::Invalid,
                )
            }
        };

        self.finish_pat_check(pat, expected, actual, analysis)
    }

    fn expected_msg_variant_for_named_recv_pat(
        &self,
        pat: PatId,
        path: PathId<'db>,
        expected: TyId<'db>,
    ) -> Option<TyId<'db>> {
        if !matches!(self.env.owner().recv_arm(self.db), Some(arm) if arm.pat == pat) {
            return None;
        }
        self.env.owner().recv_msg_path(self.db)?;
        let path_ident = path.as_ident(self.db)?;
        let adt_def = expected.adt_def(self.db)?;
        let AdtRef::Struct(struct_) = adt_def.adt_ref(self.db) else {
            return None;
        };
        let struct_name = struct_.name(self.db).to_opt()?;
        (path_ident == struct_name).then_some(expected)
    }

    fn check_record_pat_fields(
        &mut self,
        record_like: RecordLike<'db>,
        pat: PatId,
        semantic_ty: TyId<'db>,
    ) -> PatternAnalysisStatus {
        let Partial::Present(Pat::Record(_, fields)) = pat.data(self.db, self.body()) else {
            unreachable!()
        };

        let hir_db = self.db;
        let mut contains_rest = false;
        let mut invalid = false;
        let mut field_status_by_idx = rustc_hash::FxHashMap::default();

        let pat_span = pat.span(self.body()).into_record_pat();
        let mut rec_checker = RecordInitChecker::new(self, &record_like);

        for (i, field_pat) in fields.iter().enumerate() {
            if field_pat.pat.is_rest(hir_db, rec_checker.tc.body()) {
                rec_checker.tc.consume_rest_pat(
                    field_pat.pat,
                    TyId::invalid(rec_checker.tc.db, InvalidCause::Other),
                );
                if contains_rest {
                    let diag = BodyDiag::DuplicatedRestPat(
                        field_pat.pat.span(rec_checker.tc.body()).into(),
                    );
                    rec_checker.tc.push_diag(diag);
                    invalid = true;
                    continue;
                }

                contains_rest = true;
                continue;
            }

            let label = field_pat.label(hir_db, rec_checker.tc.body());
            let expected =
                match rec_checker.feed_label(label, pat_span.clone().fields().field(i).into()) {
                    Ok(ty) => ty,
                    Err(diag) => {
                        rec_checker.tc.push_diag(diag);
                        invalid = true;
                        TyId::invalid(rec_checker.tc.db, InvalidCause::Other)
                    }
                };

            let (pat_expected, mode) = rec_checker.tc.destructure_source_mode(expected);
            let result = rec_checker.tc.check_pat(field_pat.pat, pat_expected);
            if let super::PatternDestructureMode::Borrow(kind) = mode {
                rec_checker
                    .tc
                    .retype_pattern_bindings_for_borrow(field_pat.pat, kind);
            }
            if let Some(label) = label
                && let Some(field_idx) = record_like.record_field_idx(hir_db, label)
            {
                field_status_by_idx.insert(field_idx, result.analysis);
            }
        }

        if let Err(diag) = rec_checker.finalize(pat_span.fields().into(), contains_rest) {
            self.push_diag(diag);
            invalid = true;
        }

        let ctor = match record_like {
            RecordLike::Type(ty) => self.type_constructor_kind(ty, semantic_ty),
            RecordLike::EnumVariant(variant) => {
                ConstructorKind::Variant(variant.variant, variant.ty)
            }
        };
        let field_tys = ctor.field_types(self.db);
        let mut canonical_fields = Vec::with_capacity(field_tys.len());
        for (field_idx, field_ty) in field_tys.into_iter().enumerate() {
            match field_status_by_idx.remove(&field_idx) {
                Some(status) => canonical_fields.push(status),
                None if contains_rest => canonical_fields
                    .push(self.ready_wildcard(pattern_match_expected_ty(self.db, field_ty), None)),
                None => {
                    invalid = true;
                    canonical_fields.push(
                        self.ready_wildcard(pattern_match_expected_ty(self.db, field_ty), None),
                    );
                }
            }
        }

        if invalid {
            PatternAnalysisStatus::Invalid
        } else {
            self.ready_constructor(semantic_ty, ctor, canonical_fields)
        }
    }

    fn unpack_rest_pat(
        &mut self,
        pat_tup: &[PatId],
        expected_len: Option<usize>,
    ) -> UnpackedRestPat<'db> {
        let mut rest_start = None;
        for (i, &pat) in pat_tup.iter().enumerate() {
            if pat.is_rest(self.db, self.body()) && rest_start.replace(i).is_some() {
                let span = pat.span(self.body());
                self.push_diag(BodyDiag::DuplicatedRestPat(span.into()));
                return UnpackedRestPat {
                    elem_tys: self.fresh_tys_n(expected_len.unwrap_or(0)),
                    rest_range: Range::default(),
                    is_valid: false,
                };
            }
        }

        match rest_start {
            Some(rest_start) => {
                let expected_len = expected_len.unwrap_or(0);
                let minimum_len = pat_tup.len() - 1;

                if minimum_len <= expected_len {
                    let diff = expected_len - minimum_len;
                    let range = rest_start..rest_start + diff;
                    UnpackedRestPat {
                        elem_tys: self.fresh_tys_n(expected_len),
                        rest_range: range,
                        is_valid: true,
                    }
                } else {
                    UnpackedRestPat {
                        elem_tys: self.fresh_tys_n(minimum_len),
                        rest_range: Range::default(),
                        is_valid: true,
                    }
                }
            }

            None => UnpackedRestPat {
                elem_tys: self.fresh_tys_n(pat_tup.len()),
                rest_range: Range::default(),
                is_valid: true,
            },
        }
    }
}
