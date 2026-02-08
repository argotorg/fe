use std::ops::Range;

use crate::core::hir_def::{Partial, Pat, PatId, VariantKind};
use either::Either;

use super::{RecordLike, TyChecker, env::LocalBinding, path::RecordInitChecker};
use crate::analysis::{
    name_resolution::PathRes,
    ty::adt_def::AdtRef,
    ty::{
        binder::Binder,
        diagnostics::BodyDiag,
        trait_def::TraitInstId,
        ty_def::{InvalidCause, Kind, TyId, TyVarSort},
        ty_lower::lower_hir_ty,
    },
};

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

        let mut pat_idx = 0;
        for (i, &pat_ty) in unified.decompose_ty_app(self.db).1.iter().enumerate() {
            if pat_idx >= pat_tup.len() {
                break;
            };

            if pat_tup[pat_idx].is_rest(self.db, self.body()) {
                pat_idx += 1;
                continue;
            }

            if rest_range.contains(&i) {
                continue;
            }

            self.check_pat(pat_tup[pat_idx], pat_ty);
            pat_idx += 1;
        }

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

        let span = pat.span(self.body()).into_path_tuple_pat();

        let (variant, expected_elems) = match self.resolve_path(*path, true, span.clone().path()) {
            Ok(res) => match res {
                PathRes::Ty(ty)
                | PathRes::TyAlias(_, ty)
                | PathRes::Func(ty)
                | PathRes::Const(_, ty)
                | PathRes::TraitConst(ty, ..) => {
                    let diag = BodyDiag::tuple_variant_expected(
                        self.db,
                        pat.span(self.body()).into(),
                        Some(RecordLike::Type(ty)),
                    );
                    self.push_diag(diag);
                    return TyId::invalid(self.db, InvalidCause::Other);
                }

                PathRes::Trait(trait_) => {
                    let diag = BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(trait_.def(self.db).into()),
                    };
                    self.push_diag(diag);
                    return TyId::invalid(self.db, InvalidCause::Other);
                }
                PathRes::EnumVariant(variant) => match variant.kind(self.db) {
                    VariantKind::Tuple(elems) => (variant, elems),
                    _ => {
                        let diag = BodyDiag::tuple_variant_expected(
                            self.db,
                            pat.span(self.body()).into(),
                            Some(RecordLike::from_variant(variant)),
                        );
                        self.push_diag(diag);
                        return TyId::invalid(self.db, InvalidCause::Other);
                    }
                },
                PathRes::Mod(scope) => {
                    let diag = BodyDiag::NotValue {
                        primary: span.into(),
                        given: Either::Left(scope.item()),
                    };
                    self.push_diag(diag);
                    return TyId::invalid(self.db, InvalidCause::Other);
                }

                PathRes::TraitMethod(..) | PathRes::Method(..) | PathRes::FuncParam(..) => {
                    let diag = BodyDiag::tuple_variant_expected(self.db, span.into(), None);
                    self.push_diag(diag);
                    return TyId::invalid(self.db, InvalidCause::Other);
                }
            },
            Err(_) => {
                // Even when constructor resolution fails (including ambiguity),
                // still walk element patterns so that variable bindings inside
                // the tuple pattern are registered. This mirrors the recovery
                // strategy used for tuple patterns in `check_tuple_pat`.
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

        let mut arg_idx = 0;
        for (i, &hir_ty) in expected_elems.data(self.db).iter().enumerate() {
            if arg_idx >= elems.len() {
                break;
            }

            let current_pat_id = elems[arg_idx];
            let elem_ty = match hir_ty.to_opt() {
                Some(ty) => {
                    let ty = lower_hir_ty(
                        self.db,
                        ty,
                        variant.enum_(self.db).scope(),
                        self.env.assumptions(),
                    );
                    let instantiated =
                        Binder::bind(ty).instantiate(self.db, variant.ty.generic_args(self.db));
                    // Normalize the type to resolve associated types
                    self.normalize_ty(instantiated)
                }
                _ => TyId::invalid(self.db, InvalidCause::ParseError),
            };

            // Call check_pat for the current source pattern element (current_pat_id).
            // If current_pat_id is Pat::Rest, its type will be unified with elem_ty (the type of the variant field it starts covering).
            // If the current variant field 'i' is covered by rest_range (meaning '..' covers it),
            // but current_pat_id is *not* Pat::Rest, it means this current_pat_id is for a field *after* the '..'.
            // In that case, we only proceed to check_pat if 'i' is NOT in rest_range.
            if current_pat_id.is_rest(self.db, self.body()) {
                // For rest patterns, use the variant's type
                self.check_pat(current_pat_id, variant.ty);
                // The '..' pattern from the source is consumed.
                // Subsequent iterations of the outer loop will skip variant fields covered by `rest_range`.
                arg_idx += 1;
            } else if !rest_range.contains(&i) {
                // This is an explicit pattern from the source (not '..'),
                // and it corresponds to a variant field not covered by any '..'.
                self.check_pat(current_pat_id, elem_ty);
                arg_idx += 1;
            }
            // If rest_range.contains(&i) and current_pat_id is not Pat::Rest,
            // it means this variant field `i` is covered by a `..` that has already been processed (or will be).
            // We do nothing for this `elem_ty` and `current_pat_id` pair, and `arg_idx` is not incremented,
            // allowing `current_pat_id` to be matched against a subsequent variant field.
        }

        variant.ty
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

            rec_checker.tc.check_pat(field_pat.pat, expected);
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
