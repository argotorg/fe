use std::ops::Range;

use crate::core::hir_def::{Partial, Pat, PatId, PathId, TupleTypeId, VariantKind};
use either::Either;

use super::{RecordLike, TyChecker, env::LocalBinding, path::RecordInitChecker};
use crate::analysis::{
    name_resolution::{PathRes, ResolvedVariant},
    ty::adt_def::AdtRef,
    ty::{
        binder::Binder,
        diagnostics::BodyDiag,
        ty_def::{InvalidCause, Kind, TyId, TyVarSort},
        ty_lower::lower_hir_ty,
    },
};

enum TupleVariantResolution<'db> {
    Resolved(ResolvedVariant<'db>, TupleTypeId<'db>),
    Invalid,
    UnresolvedPath,
}

impl<'db> TyChecker<'db> {
    pub(super) fn check_pat(&mut self, pat: PatId, expected: TyId<'db>) -> TyId<'db> {
        let Partial::Present(pat_data) = pat.data(self.db, self.body()) else {
            let actual = TyId::invalid(self.db, InvalidCause::ParseError);
            return self.unify_ty(pat, actual, expected);
        };

        let ty = match pat_data {
            Pat::WildCard => {
                let ty_var = self.table.new_var(TyVarSort::General, &Kind::Star);
                self.unify_ty(pat, ty_var, expected)
            }

            Pat::Rest => expected, // rest pattern type checking?
            Pat::Lit(..) => self.check_lit_pat(pat, pat_data),
            Pat::Tuple(..) => self.check_tuple_pat(pat, pat_data, expected),
            Pat::Path(..) => self.check_path_pat(pat, pat_data, expected),
            Pat::PathTuple(..) => self.check_path_tuple_pat(pat, pat_data),
            Pat::Record(..) => self.check_record_pat(pat, pat_data, expected),

            Pat::Or(lhs, rhs) => {
                self.check_pat(*lhs, expected);
                self.check_pat(*rhs, expected)
            }
        };

        self.unify_ty(pat, ty, expected)
    }

    fn check_lit_pat(&mut self, _pat: PatId, pat_data: &Pat<'db>) -> TyId<'db> {
        let Pat::Lit(lit) = pat_data else {
            unreachable!()
        };

        match lit {
            Partial::Present(lit) => self.lit_ty(lit),
            Partial::Absent => TyId::invalid(self.db, InvalidCause::ParseError),
        }
    }

    fn check_tuple_pat(
        &mut self,
        pat: PatId,
        pat_data: &Pat<'db>,
        expected: TyId<'db>,
    ) -> TyId<'db> {
        let Pat::Tuple(pat_tup) = pat_data else {
            unreachable!()
        };

        let expected_len = match expected.decompose_ty_app(self.db) {
            (base, args) if base.is_tuple(self.db) => Some(args.len()),
            _ => None,
        };
        let (actual, rest_range) = self.unpack_rest_pat(pat_tup, expected_len);
        let actual = TyId::tuple_with_elems(self.db, &actual);

        let unified = self.unify_ty(pat, actual, expected);
        if unified.has_invalid(self.db) {
            // Even when unification fails, we need to check patterns to ensure
            // variable binding works correctly
            pat_tup.iter().for_each(|&pat| {
                self.check_pat(pat, TyId::invalid(self.db, InvalidCause::Other));
            });
            return unified;
        }

        let elem_tys = unified.decompose_ty_app(self.db).1.to_vec();
        self.check_tuple_like_pattern_elems(pat_tup, &elem_tys, rest_range, None);

        unified
    }

    fn check_path_pat(
        &mut self,
        pat: PatId,
        pat_data: &Pat<'db>,
        _expected: TyId<'db>,
    ) -> TyId<'db> {
        let Pat::Path(path, is_mut) = pat_data else {
            unreachable!()
        };

        let Partial::Present(path) = path else {
            return TyId::invalid(self.db, InvalidCause::ParseError);
        };

        let span = pat.span(self.body()).into_path_pat();
        let res = self.resolve_path(*path, true, span.clone().path());

        // Bare identifiers that don't resolve to a type/variant are local bindings,
        // unless the expected type is a msg type, in which case we try to resolve
        // the identifier as a msg variant first.
        if let Some(name) = path.as_ident(self.db)
            && !matches!(
                res,
                Ok(PathRes::Ty(..) | PathRes::TyAlias(..) | PathRes::EnumVariant(..))
            )
        {
            let binding = LocalBinding::local(pat, *is_mut);
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
            }
            return self.fresh_ty();
        }

        match res {
            Ok(
                PathRes::Ty(ty)
                | PathRes::TyAlias(_, ty)
                | PathRes::Func(ty)
                | PathRes::Const(_, ty),
            ) => {
                let record_like = RecordLike::from_ty(ty);
                if record_like.is_record(self.db) {
                    self.emit_unit_variant_expected(pat, record_like)
                } else {
                    ty
                }
            }

            Ok(PathRes::TraitConst(_recv_ty, inst, name)) => {
                let trait_ = inst.def(self.db);
                if let Some(const_view) = trait_.const_(self.db, name)
                    && let Some(ty_binder) = const_view.ty_binder(self.db)
                {
                    let instantiated = ty_binder.instantiate(self.db, inst.args(self.db));
                    self.table.instantiate_to_term(instantiated)
                } else {
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            }

            Ok(PathRes::Trait(trait_)) => {
                let diag = BodyDiag::NotValue {
                    primary: span.into(),
                    given: Either::Left(trait_.def(self.db).into()),
                };
                self.push_diag(diag);
                TyId::invalid(self.db, InvalidCause::Other)
            }

            Ok(PathRes::EnumVariant(variant)) => {
                if matches!(variant.kind(self.db), VariantKind::Unit) {
                    self.table.instantiate_to_term(variant.ty)
                } else {
                    self.emit_unit_variant_expected(pat, RecordLike::from_variant(variant))
                }
            }

            Ok(PathRes::Mod(scope_id)) => {
                let diag = BodyDiag::NotValue {
                    primary: span.into(),
                    given: Either::Left(scope_id.item()),
                };
                self.push_diag(diag);
                TyId::invalid(self.db, InvalidCause::Other)
            }

            Ok(PathRes::TraitMethod(..) | PathRes::Method(..) | PathRes::FuncParam(..)) => {
                TyId::invalid(self.db, InvalidCause::Other)
            }

            Err(_) => TyId::invalid(self.db, InvalidCause::Other),
        }
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

    fn check_path_tuple_pat(&mut self, pat: PatId, pat_data: &Pat<'db>) -> TyId<'db> {
        let Pat::PathTuple(Partial::Present(path), elems) = pat_data else {
            return TyId::invalid(self.db, InvalidCause::ParseError);
        };

        let (variant, expected_elems) = match self.resolve_tuple_variant_pat(pat, *path) {
            TupleVariantResolution::Resolved(variant, expected_elems) => (variant, expected_elems),
            TupleVariantResolution::Invalid => return TyId::invalid(self.db, InvalidCause::Other),
            TupleVariantResolution::UnresolvedPath => {
                for &elem_pat in elems {
                    self.check_pat(elem_pat, TyId::invalid(self.db, InvalidCause::Other));
                }
                return TyId::invalid(self.db, InvalidCause::Other);
            }
        };

        let expected_len = expected_elems.len(self.db);
        let (actual_elems, rest_range) = self.unpack_rest_pat(elems, Some(expected_len));
        if actual_elems.len() != expected_len {
            let diag = BodyDiag::MismatchedFieldCount {
                primary: pat.span(self.body()).into(),
                expected: expected_len,
                given: actual_elems.len(),
            };

            self.push_diag(diag);
            return variant.ty;
        };

        let elem_tys = self.instantiate_tuple_variant_elem_tys(variant, expected_elems);
        self.check_tuple_like_pattern_elems(elems, &elem_tys, rest_range, Some(variant.ty));

        variant.ty
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
            Err(_) => TupleVariantResolution::UnresolvedPath,
        }
    }

    fn instantiate_tuple_variant_elem_tys(
        &mut self,
        variant: ResolvedVariant<'db>,
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
                        Binder::bind(ty).instantiate(self.db, variant.ty.generic_args(self.db));
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
    ) {
        let mut pat_idx = 0;
        for (i, &elem_ty) in elem_tys.iter().enumerate() {
            if pat_idx >= source_pats.len() {
                break;
            }
            let pat = source_pats[pat_idx];
            if pat.is_rest(self.db, self.body()) {
                if let Some(rest_expected) = rest_expected {
                    self.check_pat(pat, rest_expected);
                }
                pat_idx += 1;
                continue;
            }
            if rest_range.contains(&i) {
                continue;
            }

            let (pat_expected, mode) = self.destructure_source_mode(elem_ty);
            self.check_pat(pat, pat_expected);
            if let super::DestructureSourceMode::Borrow(kind) = mode {
                self.retype_pattern_bindings_for_borrow(pat, kind);
            }
            pat_idx += 1;
        }
    }

    fn check_record_pat(
        &mut self,
        pat: PatId,
        pat_data: &Pat<'db>,
        expected: TyId<'db>,
    ) -> TyId<'db> {
        let Pat::Record(Partial::Present(path), _) = pat_data else {
            return TyId::invalid(self.db, InvalidCause::ParseError);
        };

        let span = pat.span(self.body()).into_record_pat();

        match self.resolve_path(*path, true, span.clone().path()) {
            Ok(reso) => match reso {
                PathRes::Ty(ty) | PathRes::TyAlias(_, ty)
                    if RecordLike::from_ty(ty).is_record(self.db) =>
                {
                    self.check_record_pat_fields(RecordLike::from_ty(ty), pat);
                    ty
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
                    TyId::invalid(self.db, InvalidCause::Other)
                }

                PathRes::Trait(trait_) => {
                    let diag = BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(trait_.def(self.db).into()),
                    };
                    self.push_diag(diag);
                    TyId::invalid(self.db, InvalidCause::Other)
                }

                PathRes::EnumVariant(variant) => {
                    let ty = variant.ty;
                    let record_like = RecordLike::from_variant(variant);
                    if record_like.is_record(self.db) {
                        self.check_record_pat_fields(record_like, pat);
                    }
                    ty
                }
                PathRes::Mod(scope) => {
                    let diag = BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(scope.item()),
                    };
                    self.push_diag(diag);
                    TyId::invalid(self.db, InvalidCause::Other)
                }

                PathRes::TraitMethod(..) | PathRes::Method(..) | PathRes::FuncParam(..) => {
                    let diag =
                        BodyDiag::record_expected(self.db, pat.span(self.body()).into(), None);
                    self.push_diag(diag);
                    TyId::invalid(self.db, InvalidCause::Other)
                }
            },
            Err(_) => {
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
                            self.check_record_pat_fields(record_like, pat);
                        }
                        return expected;
                    }
                }

                TyId::invalid(self.db, InvalidCause::Other)
            }
        }
    }

    fn check_record_pat_fields(&mut self, record_like: RecordLike<'db>, pat: PatId) {
        let Partial::Present(Pat::Record(_, fields)) = pat.data(self.db, self.body()) else {
            unreachable!()
        };

        let hir_db = self.db;
        let mut contains_rest = false;

        let pat_span = pat.span(self.body()).into_record_pat();
        let mut rec_checker = RecordInitChecker::new(self, &record_like);

        for (i, field_pat) in fields.iter().enumerate() {
            if field_pat.pat.is_rest(hir_db, rec_checker.tc.body()) {
                if contains_rest {
                    let diag = BodyDiag::DuplicatedRestPat(
                        field_pat.pat.span(rec_checker.tc.body()).into(),
                    );
                    rec_checker.tc.push_diag(diag);
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
                        TyId::invalid(rec_checker.tc.db, InvalidCause::Other)
                    }
                };

            let (pat_expected, mode) = rec_checker.tc.destructure_source_mode(expected);
            rec_checker.tc.check_pat(field_pat.pat, pat_expected);
            if let super::DestructureSourceMode::Borrow(kind) = mode {
                rec_checker
                    .tc
                    .retype_pattern_bindings_for_borrow(field_pat.pat, kind);
            }
        }

        if let Err(diag) = rec_checker.finalize(pat_span.fields().into(), contains_rest) {
            self.push_diag(diag);
        }
    }

    fn unpack_rest_pat(
        &mut self,
        pat_tup: &[PatId],
        expected_len: Option<usize>,
    ) -> (Vec<TyId<'db>>, std::ops::Range<usize>) {
        let mut rest_start = None;
        for (i, &pat) in pat_tup.iter().enumerate() {
            if pat.is_rest(self.db, self.body()) && rest_start.replace(i).is_some() {
                let span = pat.span(self.body());
                self.push_diag(BodyDiag::DuplicatedRestPat(span.into()));
                return (
                    self.fresh_tys_n(expected_len.unwrap_or(0)),
                    Range::default(),
                );
            }
        }

        match rest_start {
            Some(rest_start) => {
                let expected_len = expected_len.unwrap_or(0);
                let minimum_len = pat_tup.len() - 1;

                if minimum_len <= expected_len {
                    let diff = expected_len - minimum_len;
                    let range = rest_start..rest_start + diff;
                    (self.fresh_tys_n(expected_len), range)
                } else {
                    (self.fresh_tys_n(minimum_len), Range::default())
                }
            }

            None => (self.fresh_tys_n(pat_tup.len()), Range::default()),
        }
    }
}
