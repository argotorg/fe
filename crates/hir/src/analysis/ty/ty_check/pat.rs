use std::ops::Range;

use crate::core::hir_def::{
    BindingMarker, Func, IdentId, IntegerId, ItemKind, LitKind, Partial, Pat, PatId, PathId,
    TupleTypeId, VariantKind, scope_graph::ScopeId,
};
use crate::span::{DynLazySpan, HirOrigin};
use either::Either;

use super::{
    BodyOwner, ConstRef, RecordLike, TyChecker, env::LocalBinding, path::RecordInitChecker,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        ExpectedPathKind, NameDomain, NameRes, NameResolutionError, PathRes, PathResError,
        ResolvedVariant, resolve_ident_to_bucket,
    },
    semantic::{
        EvalOutcome, SemConstScalar, SemConstValue, SemOrigin, eval_const_ref,
        instance::resolve_semantic_const_ref, normalize_int_to_shape,
    },
    ty::adt_def::instantiate_adt_field_shape,
    ty::{
        assoc_const::{AssocConstUse, InherentConstUse},
        const_ty::{
            LoweringContext, instantiate_inherent_const_decl_ty, invalid_cause_from_eval_failure,
        },
        diagnostics::{BodyDiag, RevealedBy, TraitConstraintDiag, TyDiagCollection, TyLowerDiag},
        fold::TyFoldable,
        pattern_ir::{
            BindingRef, ConstructorKind, PatternAnalysisStatus, ValidatedPat, ValidatedPatKind,
        },
        trait_def::TraitInstId,
        trait_resolution::{GoalSatisfiability, TraitSolveCx, is_goal_satisfiable},
        ty_def::{InvalidCause, Kind, TyFlags, TyId, TyVarSort},
        visitor::collect_flags,
    },
};

enum TupleVariantResolution<'db> {
    Resolved(ResolvedVariant<'db>, TupleTypeId<'db>),
    /// A tuple struct type, e.g. `Month` in `Month(m)`.
    TupleStruct(TyId<'db>),
    Invalid,
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

/// Whether `func` is generated from a declaration, like a msg variant's
/// `decode_payload`. Checking a body depends on this rather than on `func`'s
/// origin, which moves with the source text.
#[salsa::tracked(returns(copy))]
fn is_generated_func<'db>(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> bool {
    matches!(func.origin(db), HirOrigin::Desugared(_))
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
        statuses
            .into_iter()
            .map(|status| match status {
                PatternAnalysisStatus::Ready(root) => Ok(root),
                PatternAnalysisStatus::Invalid => Err(PatternAnalysisStatus::Invalid),
            })
            .collect()
    }

    fn binding_ref(&self, pat: PatId, name: IdentId<'db>) -> BindingRef<'db> {
        BindingRef {
            name,
            representative_pat: pat,
        }
    }

    pub(super) fn canonical_nominal_ty_from_expected(
        &mut self,
        resolved_ty: TyId<'db>,
        expected: TyId<'db>,
    ) -> TyId<'db> {
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
        ConstructorKind::Type(self.canonical_nominal_ty_from_expected(resolved_ty, expected))
    }

    fn literal_constructor_status(
        &mut self,
        ty: TyId<'db>,
        lit: LitKind<'db>,
    ) -> PatternAnalysisStatus {
        self.ready_constructor(ty, ConstructorKind::Literal(lit, ty), Vec::new())
    }

    /// The analysis of `pat`, which names the constant `cref` of type `ty`: a
    /// literal, for an integer or `bool`. A pattern can't compare against any
    /// other value, nor against one that depends on generic parameters.
    fn const_pattern_status(
        &mut self,
        pat: PatId,
        cref: ConstRef<'db>,
        ty: TyId<'db>,
        expected: TyId<'db>,
    ) -> PatternAnalysisStatus {
        let value_ty = expected
            .as_capability(self.db)
            .map_or(expected, |(_, inner)| inner);
        let primary: DynLazySpan<'db> = pat.span(self.body()).into();
        let lit = match resolve_semantic_const_ref(self.db, cref, value_ty, SemOrigin::Synthetic)
            .map(|cref| {
                let owner = cref.instance(self.db).owner(self.db);
                (owner, eval_const_ref(self.db, cref))
            }) {
            Some((_, EvalOutcome::Ready(value))) => match value.value(self.db) {
                SemConstValue::Scalar {
                    value: SemConstScalar::Int { value },
                    ..
                } => {
                    // A literal holds an unsigned integer, so a negative value
                    // is held as its two's complement in 256 bits, the widest
                    // integer type. The pattern store narrows it to its type.
                    let (_, bits) = normalize_int_to_shape(value.clone(), 256, false).into_parts();
                    LitKind::Int(IntegerId::new(self.db, bits))
                }
                SemConstValue::Scalar {
                    value: SemConstScalar::Bool(flag),
                    ..
                } => LitKind::Bool(*flag),
                SemConstValue::Unit
                | SemConstValue::Description(..)
                | SemConstValue::Tuple { .. }
                | SemConstValue::Struct { .. }
                | SemConstValue::Array { .. }
                | SemConstValue::Enum { .. }
                | SemConstValue::Scalar {
                    value: SemConstScalar::Bytes(_),
                    ..
                } => {
                    self.push_diag(BodyDiag::ConstPatternType { primary, ty });
                    return PatternAnalysisStatus::Invalid;
                }
            },
            Some((owner, EvalOutcome::Failed(failure))) => {
                // Reported here even if the declaration reports it too: a
                // failure can depend on the generic arguments of this use,
                // as `G<0>::C` does with `const C: u256 = 1 / N`.
                let cause = invalid_cause_from_eval_failure(self.db, owner, failure);
                match TyId::invalid(self.db, cause).emit_diag(self.db, primary.clone()) {
                    Some(fault) => {
                        self.push_diag(TyDiagCollection::from(TyLowerDiag::RevealedConstFault {
                            site: primary,
                            revealed_by: RevealedBy::Evaluation,
                            fault: Box::new(fault),
                        }))
                    }
                    None => self.push_diag(BodyDiag::InvalidConstPattern { primary }),
                }
                return PatternAnalysisStatus::Invalid;
            }
            Some((_, EvalOutcome::Blocked(_))) => {
                self.push_diag(BodyDiag::GenericConstPattern { primary });
                return PatternAnalysisStatus::Invalid;
            }
            // No value to evaluate: the use's generic parameters would select
            // it, or there is none, as for a constant that an impl omits.
            None => {
                let generic = match cref {
                    ConstRef::Const(_) => false,
                    ConstRef::TraitConst { use_, .. } => {
                        collect_flags(self.db, use_.inst()).contains(TyFlags::HAS_PARAM)
                    }
                    ConstRef::InherentConst(use_) => use_.receiver_ty().has_param(self.db),
                };
                self.push_diag(if generic {
                    BodyDiag::GenericConstPattern { primary }
                } else {
                    BodyDiag::InvalidConstPattern { primary }
                });
                return PatternAnalysisStatus::Invalid;
            }
        };
        self.literal_constructor_status(expected, lit)
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
        // Against an invalid type, the elements are invalid too, so they do
        // not report unknown types of their own.
        if expected.invalid_cause(self.db).is_some() {
            self.check_tuple_like_pattern_elems(
                pat_tup,
                &vec![expected; pat_tup.len()],
                Range::default(),
                Some(expected),
            );
            return self.finish_pat_check(pat, expected, expected, PatternAnalysisStatus::Invalid);
        }

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
        let Pat::Path(path, marker) = pat_data else {
            unreachable!()
        };
        if *marker == BindingMarker::MaybeMut && self.env.owner().mode().is_none() {
            self.push_diag(BodyDiag::MaybeMutWithoutMode {
                primary: pat.span(self.body()).into(),
            });
        }

        let Partial::Present(path) = path else {
            return self.finish_pat_check(
                pat,
                expected,
                TyId::invalid(self.db, InvalidCause::ParseError),
                PatternAnalysisStatus::Invalid,
            );
        };

        let span = pat.span(self.body()).into_path_pat();

        // A bare identifier is a local binding unless it names something a
        // pattern matches, like a constant or a unit variant; a function,
        // module, or trait of its name is shadowed. One that is ambiguous
        // among things a pattern matches is an error. A generated function's
        // bindings are fresh names, which bind whatever else is in scope: its
        // declaration avoids only the names it mentions.
        if let Some(name) = path.as_ident(self.db) {
            let generated = matches!(
                self.env.owner(),
                BodyOwner::Func(func, _) if is_generated_func(self.db, func)
            );
            let shadowed = |res: &NameRes<'db>| {
                matches!(
                    res.scope(),
                    Some(
                        ScopeId::Item(
                            ItemKind::Func(_)
                                | ItemKind::Mod(_)
                                | ItemKind::TopMod(_)
                                | ItemKind::Trait(_)
                        ) | ScopeId::FuncParam(..)
                    )
                )
            };
            let named = if generated {
                None
            } else {
                let bucket = resolve_ident_to_bucket(self.db, *path, self.env.scope());
                [NameDomain::VALUE, NameDomain::TYPE]
                    .into_iter()
                    .find_map(|domain| match bucket.pick(domain) {
                        Ok(res) => Some(Ok(res)),
                        Err(err @ NameResolutionError::Ambiguous(cands))
                            if !cands.iter().all(shadowed) =>
                        {
                            Some(Err(err))
                        }
                        Err(_) => None,
                    })
            };
            match named {
                Some(Err(err)) => {
                    let err = PathResError::from_name_res_error(err.clone(), *path);
                    if let Some(diag) =
                        err.into_diag(self.db, *path, span.clone().path(), ExpectedPathKind::Pat)
                    {
                        self.push_diag(diag);
                    }
                    let invalid = TyId::invalid(self.db, InvalidCause::Other);
                    return self.finish_pat_check(
                        pat,
                        expected,
                        invalid,
                        PatternAnalysisStatus::Invalid,
                    );
                }
                Some(Ok(res)) if !shadowed(res) => {}
                _ => {
                    let binding = LocalBinding::local(
                        pat,
                        matches!(
                            self.binding_marker(pat, *marker),
                            BindingMarker::Var | BindingMarker::Mut
                        ),
                    );
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
            }
        }

        let lowering_cx = LoweringContext::new();
        let res = self.resolve_path(
            *path,
            true,
            span.clone().path(),
            span.clone().into(),
            &lowering_cx,
        );

        let (actual, analysis) = match res {
            Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty) | PathRes::Func(ty)) => {
                let record_like = RecordLike::from_ty(ty);
                // Tuple structs must be destructured with `Name(..)`.
                if record_like.is_record(self.db) || ty.as_tuple_struct(self.db).is_some() {
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
                self.const_pattern_status(pat, ConstRef::Const(const_def), ty, expected),
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
                    inst.mode(self.db),
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
                    let cref = ConstRef::TraitConst {
                        use_: AssocConstUse::new(
                            self.env.scope(),
                            self.env.assumptions(),
                            inst,
                            name,
                        ),
                        evidence: None,
                        bound_evidence: None,
                    };
                    (ty, self.const_pattern_status(pat, cref, ty, expected))
                } else {
                    (
                        TyId::invalid(self.db, InvalidCause::Other),
                        PatternAnalysisStatus::Invalid,
                    )
                }
            }

            Ok(PathRes::InherentConst(recv_ty, impl_, name)) => {
                if let Some(ty) = instantiate_inherent_const_decl_ty(
                    self.db,
                    &mut self.table,
                    impl_,
                    recv_ty,
                    name,
                ) {
                    let cref = ConstRef::InherentConst(InherentConstUse::new(
                        self.env.scope(),
                        self.env.assumptions(),
                        impl_,
                        recv_ty,
                        name,
                    ));
                    (ty, self.const_pattern_status(pat, cref, ty, expected))
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
                        self.canonical_nominal_ty_from_expected(semantic_ty, expected)
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

            Ok(PathRes::TraitMethod(..) | PathRes::Method(..) | PathRes::FuncParam(..)) => {
                let diag =
                    BodyDiag::unit_variant_expected(self.db, pat.span(self.body()).into(), None);
                self.push_diag(diag);
                (
                    TyId::invalid(self.db, InvalidCause::Other),
                    PatternAnalysisStatus::Invalid,
                )
            }

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
        let diag = BodyDiag::unit_variant_expected(
            self.db,
            pat.span(self.body()).into(),
            Some(record_like),
        );
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

        let pat_span: crate::span::DynLazySpan<'db> = pat.span(self.body()).into();
        match self.resolve_tuple_variant_pat(pat, *path) {
            TupleVariantResolution::Resolved(variant, expected_elems) => {
                let (semantic_ty, variant_ty) =
                    self.equate_nominal_pat_ty(variant.ty, expected, pat_span);
                let elem_tys =
                    self.instantiate_tuple_variant_elem_tys(variant, variant_ty, expected_elems);
                let ctor = ConstructorKind::Variant(variant.variant, semantic_ty);
                self.check_tuple_like_pat_fields(
                    pat,
                    elems,
                    expected,
                    (semantic_ty, variant_ty),
                    elem_tys,
                    ctor,
                    true,
                )
            }
            TupleVariantResolution::TupleStruct(struct_ty) => {
                let fields_visible =
                    self.check_tuple_struct_fields_visible(struct_ty, pat_span.clone());
                let (semantic_ty, struct_ty) =
                    self.equate_nominal_pat_ty(struct_ty, expected, pat_span);
                let ctor = self.type_constructor_kind(struct_ty, semantic_ty);
                let elem_tys = ctor.field_types(self.db);
                self.check_tuple_like_pat_fields(
                    pat,
                    elems,
                    expected,
                    (semantic_ty, struct_ty),
                    elem_tys,
                    ctor,
                    fields_visible,
                )
            }
            TupleVariantResolution::Invalid => {
                self.check_tuple_like_pattern_elems(elems, &[], Range::default(), None);
                let invalid = TyId::invalid(self.db, InvalidCause::Other);
                self.finish_pat_check(pat, invalid, invalid, PatternAnalysisStatus::Invalid)
            }
        }
    }

    /// The type a pattern of the nominal type `nominal_ty` has against
    /// `expected`, and the nominal type its fields are instantiated from:
    /// both the canonical unified type, or `nominal_ty` canonicalized alone
    /// when it doesn't unify.
    fn equate_nominal_pat_ty(
        &mut self,
        nominal_ty: TyId<'db>,
        expected: TyId<'db>,
        span: crate::span::DynLazySpan<'db>,
    ) -> (TyId<'db>, TyId<'db>) {
        let semantic_ty = self.equate_ty(nominal_ty, expected, span);
        let semantic_ty = if semantic_ty.has_invalid(self.db) {
            semantic_ty
        } else {
            self.canonical_nominal_ty_from_expected(semantic_ty, expected)
        };
        if semantic_ty.has_invalid(self.db) {
            let nominal_ty = self.canonical_nominal_ty_from_expected(nominal_ty, expected);
            (semantic_ty, nominal_ty)
        } else {
            (semantic_ty, semantic_ty)
        }
    }

    /// Checks the elements `elems` of a tuple variant or tuple struct pattern
    /// of type `semantic_ty` against its fields `elem_tys`.
    #[allow(clippy::too_many_arguments)]
    fn check_tuple_like_pat_fields(
        &mut self,
        pat: PatId,
        elems: &[PatId],
        expected: TyId<'db>,
        (semantic_ty, nominal_ty): (TyId<'db>, TyId<'db>),
        elem_tys: Vec<TyId<'db>>,
        ctor: ConstructorKind<'db>,
        is_valid: bool,
    ) -> PatCheckResult<'db> {
        let expected_len = elem_tys.len();
        let unpacked = self.unpack_rest_pat(elems, Some(expected_len));
        let fields = self.check_tuple_like_pattern_elems(
            elems,
            &elem_tys,
            unpacked.rest_range,
            Some(nominal_ty),
        );
        let given = unpacked.elem_tys.len();
        let analysis = if given != expected_len {
            self.push_diag(BodyDiag::MismatchedFieldCount {
                primary: pat.span(self.body()).into(),
                expected: expected_len,
                given,
            });
            PatternAnalysisStatus::Invalid
        } else if semantic_ty.has_invalid(self.db) || !unpacked.is_valid || !is_valid {
            PatternAnalysisStatus::Invalid
        } else {
            self.ready_constructor(semantic_ty, ctor, fields)
        };
        self.finish_pat_check(pat, expected, semantic_ty, analysis)
    }

    fn resolve_tuple_variant_pat(
        &mut self,
        pat: PatId,
        path: PathId<'db>,
    ) -> TupleVariantResolution<'db> {
        let span = pat.span(self.body()).into_path_tuple_pat();
        let lowering_cx = LoweringContext::new();
        match self.resolve_path(
            path,
            true,
            span.clone().path(),
            span.clone().into(),
            &lowering_cx,
        ) {
            Ok(res) => match res {
                PathRes::Ty(ty) if ty.as_tuple_struct(self.db).is_some() => {
                    TupleVariantResolution::TupleStruct(ty)
                }
                PathRes::Ty(ty)
                | PathRes::TyAlias(_, ty)
                | PathRes::Func(ty)
                | PathRes::Const(_, ty)
                | PathRes::TraitConst(ty, ..)
                | PathRes::InherentConst(ty, ..) => {
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
                PathRes::EnumVariant(variant) => {
                    if let VariantKind::Tuple(elems) = variant.kind(self.db) {
                        TupleVariantResolution::Resolved(variant, elems)
                    } else {
                        self.push_diag(BodyDiag::tuple_variant_expected(
                            self.db,
                            pat.span(self.body()).into(),
                            Some(RecordLike::from_variant(variant)),
                        ));
                        TupleVariantResolution::Invalid
                    }
                }
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
                if let Some(diag) =
                    err.into_diag(self.db, path, span.clone().path(), ExpectedPathKind::Pat)
                {
                    self.push_diag(diag);
                }
                TupleVariantResolution::Invalid
            }
        }
    }

    fn instantiate_tuple_variant_elem_tys(
        &mut self,
        variant: ResolvedVariant<'db>,
        variant_ty: TyId<'db>,
        elems: TupleTypeId<'db>,
    ) -> Vec<TyId<'db>> {
        // The variant's field types are lowered under the enum's own bounds.
        let Some(adt) = variant_ty.adt_def(self.db) else {
            return vec![TyId::invalid(self.db, InvalidCause::Other); elems.len(self.db)];
        };
        let args = variant_ty.generic_args(self.db);
        (0..elems.len(self.db))
            .map(|idx| {
                let ty = instantiate_adt_field_shape(
                    self.db,
                    adt,
                    usize::from(variant.variant.idx),
                    idx,
                    args,
                );
                self.normalize_ty(ty)
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
                analyses.push(self.ready_wildcard(elem_ty, None));
                continue;
            }
            if pat_idx >= source_pats.len() {
                analyses.push(self.ready_wildcard(elem_ty, None));
                continue;
            }

            let pat = source_pats[pat_idx];
            let result = self.check_pat(pat, elem_ty);
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
        let pat_span: crate::span::DynLazySpan<'db> = pat.span(self.body()).into();
        let invalid = TyId::invalid(self.db, InvalidCause::Other);

        // The record the pattern names, with its type, or the diagnostic for
        // a path that names no record.
        let lowering_cx = LoweringContext::new();
        let (actual, record_like) = match self.resolve_path(
            *path,
            true,
            span.clone().path(),
            span.clone().into(),
            &lowering_cx,
        ) {
            Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty))
                if RecordLike::from_ty(ty).is_record(self.db) =>
            {
                let (semantic_ty, record_ty) = self.equate_nominal_pat_ty(ty, expected, pat_span);
                (semantic_ty, Some(RecordLike::Type(record_ty)))
            }
            Ok(PathRes::EnumVariant(variant))
                if RecordLike::from_variant(variant).is_record(self.db) =>
            {
                let (semantic_ty, ty) = self.equate_nominal_pat_ty(variant.ty, expected, pat_span);
                (
                    semantic_ty,
                    Some(RecordLike::EnumVariant(ResolvedVariant { ty, ..variant })),
                )
            }
            Ok(reso) => {
                let diag = match reso {
                    PathRes::Trait(trait_) => BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(trait_.def(self.db).into()),
                    },
                    PathRes::Mod(scope) => BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(scope.item()),
                    },
                    PathRes::EnumVariant(variant) => BodyDiag::record_expected(
                        self.db,
                        pat_span,
                        Some(RecordLike::from_variant(variant)),
                    ),
                    PathRes::Ty(ty)
                    | PathRes::TyAlias(_, ty)
                    | PathRes::Func(ty)
                    | PathRes::Const(_, ty)
                    | PathRes::TraitConst(ty, ..)
                    | PathRes::InherentConst(ty, ..) => {
                        BodyDiag::record_expected(self.db, pat_span, Some(RecordLike::Type(ty)))
                    }
                    PathRes::TraitMethod(..) | PathRes::Method(..) | PathRes::FuncParam(..) => {
                        BodyDiag::record_expected(self.db, pat_span, None)
                    }
                };
                self.push_diag(diag);
                (invalid, None)
            }
            Err(err) => {
                if let Some(diag) =
                    err.into_diag(self.db, *path, span.clone().path(), ExpectedPathKind::Pat)
                {
                    self.push_diag(diag);
                }
                (invalid, None)
            }
        };

        let analysis = match record_like {
            Some(record_like) => self.check_record_pat_fields(record_like, pat, actual),
            None => {
                self.check_unknown_record_pat_fields(pat);
                PatternAnalysisStatus::Invalid
            }
        };
        self.finish_pat_check(pat, expected, actual, analysis)
    }

    /// Checks the pattern of a recv arm against `expected`, the msg variant
    /// that the recv block resolved the pattern's path to, so that the path
    /// isn't resolved a second time, where it could name something else.
    /// `expected` is invalid when the path names no variant, which checking
    /// the recv block reports.
    pub(super) fn check_recv_arm_pat(
        &mut self,
        pat: PatId,
        expected: TyId<'db>,
    ) -> PatCheckResult<'db> {
        let invalid = TyId::invalid(self.db, InvalidCause::Other);
        let record_like = RecordLike::from_ty(expected);
        let (actual, analysis) = match pat.data(self.db, self.body()) {
            Partial::Present(Pat::Path(..)) if expected.has_invalid(self.db) => {
                (invalid, PatternAnalysisStatus::Invalid)
            }
            Partial::Present(Pat::Path(..)) if expected.field_types(self.db).is_empty() => {
                let ctor = ConstructorKind::Type(expected);
                (expected, self.ready_constructor(expected, ctor, Vec::new()))
            }
            Partial::Present(Pat::Path(..)) => (
                self.emit_unit_variant_expected(pat, record_like),
                PatternAnalysisStatus::Invalid,
            ),
            Partial::Present(Pat::Record(..)) if record_like.is_record(self.db) => (
                expected,
                self.check_record_pat_fields(record_like, pat, expected),
            ),
            Partial::Present(Pat::Record(..)) => {
                self.check_unknown_record_pat_fields(pat);
                (invalid, PatternAnalysisStatus::Invalid)
            }
            // The parser rejects a tuple pattern in a recv arm.
            Partial::Present(Pat::PathTuple(_, elems)) => {
                self.check_tuple_like_pattern_elems(elems, &[], Range::default(), None);
                (invalid, PatternAnalysisStatus::Invalid)
            }
            _ => return self.check_pat(pat, expected),
        };
        self.finish_pat_check(pat, expected, actual, analysis)
    }

    /// Checks the fields of the record pattern `pat`, whose path names no
    /// record, against an invalid type, so that they still bind their names.
    fn check_unknown_record_pat_fields(&mut self, pat: PatId) {
        let Partial::Present(Pat::Record(_, fields)) = pat.data(self.db, self.body()) else {
            unreachable!()
        };
        let invalid = TyId::invalid(self.db, InvalidCause::Other);
        for field in fields {
            if field.pat.is_rest(self.db, self.body()) {
                self.consume_rest_pat(field.pat, invalid);
            } else {
                self.check_pat(field.pat, invalid);
            }
        }
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
            let field_idx = label.and_then(|label| record_like.record_field_idx(hir_db, label));

            let result = rec_checker.tc.check_pat(field_pat.pat, expected);
            if let Some(field_idx) = field_idx {
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
                None if contains_rest => canonical_fields.push(self.ready_wildcard(field_ty, None)),
                None => {
                    invalid = true;
                    canonical_fields.push(self.ready_wildcard(field_ty, None));
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
