use crate::core::hir_def::{
    Body, GenericArg, GenericArgListId, GenericParam, GenericParamOwner, GenericParamView, IdentId,
    KindBound as HirKindBound, Partial, PathId, TypeAlias as HirTypeAlias, TypeBound,
    TypeId as HirTyId, TypeKind as HirTyKind, scope_graph::ScopeId,
};
use salsa::Update;
use smallvec::smallvec;

use super::{
    const_expr::{ConstExpr, ConstExprId},
    const_ty::{ConstTyData, ConstTyId},
    effects::{EffectKeyKind, effect_key_kind},
    fold::{TyFoldable, TyFolder},
    trait_def::TraitInstId,
    trait_resolution::{PredicateListId, constraint::collect_constraints},
    ty_def::{InvalidCause, Kind, TyData, TyId, TyParam},
};
use crate::analysis::name_resolution::{PathRes, PathResErrorKind, resolve_path};
use crate::analysis::{HirAnalysisDb, ty::binder::Binder};

/// Lowers the given HirTy to `TyId`.
#[salsa::tracked(cycle_fn=lower_hir_ty_cycle_recover, cycle_initial=lower_hir_ty_cycle_initial)]
pub fn lower_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: HirTyId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    match ty.data(db) {
        HirTyKind::Ptr(pointee) => {
            let pointee = lower_opt_hir_ty(db, *pointee, scope, assumptions);
            let ptr = TyId::ptr(db);
            TyId::app(db, ptr, pointee)
        }

        HirTyKind::Path(path) => lower_path(db, scope, *path, assumptions),

        HirTyKind::Tuple(tuple_id) => {
            let elems = tuple_id.data(db);
            let len = elems.len();
            let tuple = TyId::tuple(db, len);
            elems.iter().fold(tuple, |acc, &elem| {
                let elem_ty = lower_opt_hir_ty(db, elem, scope, assumptions);
                if !elem_ty.has_star_kind(db) {
                    return TyId::invalid(db, InvalidCause::NotFullyApplied);
                }

                TyId::app(db, acc, elem_ty)
            })
        }

        HirTyKind::Array(hir_elem_ty, len) => {
            let elem_ty = lower_opt_hir_ty(db, *hir_elem_ty, scope, assumptions);
            let len_ty = ConstTyId::from_opt_body(db, *len);
            let len_ty = TyId::const_ty(db, len_ty);
            let array = TyId::array(db, elem_ty);
            TyId::app(db, array, len_ty)
        }

        HirTyKind::Never => TyId::never(db),
    }
}

pub fn lower_opt_hir_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: Partial<HirTyId<'db>>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    ty.to_opt()
        .map(|hir_ty| lower_hir_ty(db, hir_ty, scope, assumptions))
        .unwrap_or_else(|| TyId::invalid(db, InvalidCause::ParseError))
}

fn lower_path<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    path: Partial<PathId<'db>>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let Some(path) = path.to_opt() else {
        return TyId::invalid(db, InvalidCause::ParseError);
    };

    match resolve_path(db, path, scope, assumptions, false) {
        Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty) | PathRes::Func(ty)) => ty,
        Ok(res) => TyId::invalid(db, InvalidCause::NotAType(res)),
        Err(err) => {
            // Try to resolve as a value, to find a matching `const` definition
            if matches!(err.kind, PathResErrorKind::NotFound { .. })
                && let Ok(resolved) = resolve_path(db, path, scope, assumptions, true)
            {
                return match resolved {
                    PathRes::Const(const_def, ty) => {
                        if let Some(body) = const_def.body(db).to_opt() {
                            let const_ty =
                                ConstTyId::from_body(db, body, Some(ty), Some(const_def));
                            TyId::const_ty(db, const_ty)
                        } else {
                            TyId::invalid(db, InvalidCause::ParseError)
                        }
                    }
                    PathRes::TraitConst(recv_ty, inst, name) => {
                        let mut args = inst.args(db).clone();
                        if let Some(self_arg) = args.first_mut() {
                            *self_arg = recv_ty;
                        }
                        let inst = TraitInstId::new(
                            db,
                            inst.def(db),
                            args,
                            inst.assoc_type_bindings(db).clone(),
                        );

                        if let Some(const_ty) =
                            super::const_ty::const_ty_from_trait_const(db, inst, name)
                        {
                            TyId::const_ty(db, const_ty)
                        } else if let Some(expected_ty) = inst
                            .def(db)
                            .const_(db, name)
                            .and_then(|v| v.ty_binder(db))
                            .map(|b| b.instantiate(db, inst.args(db)))
                        {
                            let expr = ConstExprId::new(db, ConstExpr::TraitConst { inst, name });
                            let const_ty =
                                ConstTyId::new(db, ConstTyData::Abstract(expr, expected_ty));
                            TyId::const_ty(db, const_ty)
                        } else {
                            TyId::invalid(db, InvalidCause::Other)
                        }
                    }
                    other => TyId::invalid(db, InvalidCause::NotAType(other)),
                };
            }

            TyId::invalid(db, InvalidCause::PathResolutionFailed { path })
        }
    }
}

fn lower_hir_ty_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    _ty: HirTyId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    // On cycles during type lowering, treat the type as invalid so that
    // callers can emit a suitable diagnostic without recursing further.
    TyId::invalid(db, InvalidCause::Other)
}

fn lower_hir_ty_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &TyId<'db>,
    _count: u32,
    _ty: HirTyId<'db>,
    _scope: ScopeId<'db>,
    _assumptions: PredicateListId<'db>,
) -> salsa::CycleRecoveryAction<TyId<'db>> {
    // Keep iterating until we reach a fixpoint; the initial value is
    // already marked invalid, so subsequent iterations will converge
    // quickly without panicking.
    salsa::CycleRecoveryAction::Iterate
}

fn lower_const_ty_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    scope: ScopeId<'db>,
    ty: HirTyId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let HirTyKind::Path(path) = ty.data(db) else {
        return TyId::invalid(db, InvalidCause::InvalidConstParamTy);
    };

    if !path
        .to_opt()
        .map(|p| p.generic_args(db).is_empty(db))
        .unwrap_or(true)
    {
        return TyId::invalid(db, InvalidCause::InvalidConstParamTy);
    }
    let ty = lower_path(db, scope, *path, assumptions);

    if ty.has_invalid(db) || ty.is_integral(db) || ty.is_bool(db) {
        ty
    } else {
        TyId::invalid(db, InvalidCause::InvalidConstParamTy)
    }
}

/// Collects the generic parameters of the given generic parameter owner.
#[salsa::tracked]
pub(crate) fn collect_generic_params<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
) -> GenericParamTypeSet<'db> {
    GenericParamCollector::new(db, owner).finalize()
}

/// Lowers the given type alias to [`TyAlias`].
#[salsa::tracked(return_ref, cycle_fn=lower_type_alias_cycle_recover, cycle_initial=lower_type_alias_cycle_initial)]
pub(crate) fn lower_type_alias<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
) -> TyAlias<'db> {
    crate::core::semantic::lower_type_alias_body(db, alias)
}

pub(crate) fn lower_type_alias_from_hir<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
    alias_type_ref: Option<HirTyId<'db>>,
) -> TyAlias<'db> {
    let param_set = collect_generic_params(db, alias.into());

    let Some(hir_ty) = alias_type_ref else {
        return TyAlias {
            alias,
            alias_to: Binder::bind(TyId::invalid(db, InvalidCause::ParseError)),
            param_set,
        };
    };

    let assumptions = collect_constraints(db, alias.into()).instantiate_identity();
    let alias_to = lower_hir_ty(db, hir_ty, alias.scope(), assumptions);
    let alias_to = if let TyData::Invalid(InvalidCause::AliasCycle(cycle)) = alias_to.data(db) {
        if cycle.contains(&alias) {
            alias_to
        } else {
            let mut cycle = cycle.clone();
            cycle.push(alias);
            TyId::invalid(db, InvalidCause::AliasCycle(cycle))
        }
    } else if alias_to.has_invalid(db) {
        // Should be reported by TypeAliasAnalysisPass
        TyId::invalid(db, InvalidCause::Other)
    } else {
        alias_to
    };
    TyAlias {
        alias,
        alias_to: Binder::bind(alias_to),
        param_set,
    }
}

fn lower_type_alias_cycle_initial<'db>(
    db: &'db dyn HirAnalysisDb,
    alias: HirTypeAlias<'db>,
) -> TyAlias<'db> {
    TyAlias {
        alias,
        alias_to: Binder::bind(TyId::invalid(
            db,
            InvalidCause::AliasCycle(smallvec![alias]),
        )),
        param_set: GenericParamTypeSet::empty(db, alias.scope()),
    }
}

fn lower_type_alias_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &TyAlias<'db>,
    _count: u32,
    _alias: HirTypeAlias<'db>,
) -> salsa::CycleRecoveryAction<TyAlias<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

#[doc(hidden)]
#[salsa::tracked(return_ref)]
pub(crate) fn evaluate_params_precursor<'db>(
    db: &'db dyn HirAnalysisDb,
    set: GenericParamTypeSet<'db>,
) -> Vec<TyId<'db>> {
    set.params_precursor(db)
        .iter()
        .enumerate()
        .map(|(i, p)| p.evaluate(db, set.scope(db), i))
        .collect()
}

/// Represents a lowered type alias. `TyAlias` itself isn't a type, but
/// can be instantiated to a `TyId` by substituting its type
/// parameters with actual types.
///
/// NOTE: `TyAlias` can't become an alias to partial applied types, i.e., the
/// right hand side of the alias declaration must be a fully applied type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct TyAlias<'db> {
    pub alias: HirTypeAlias<'db>,
    pub alias_to: Binder<TyId<'db>>,
    pub param_set: GenericParamTypeSet<'db>,
}

impl<'db> TyAlias<'db> {
    pub fn params(&self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        self.param_set.params(db)
    }
}

pub(crate) fn lower_generic_arg_list<'db>(
    db: &'db dyn HirAnalysisDb,
    args: GenericArgListId<'db>,
    scope: ScopeId<'db>,
    assumptions: PredicateListId<'db>,
) -> Vec<TyId<'db>> {
    args.data(db)
        .iter()
        .map(|arg| match arg {
            GenericArg::Type(ty_arg) => {
                // Generic args are syntactically ambiguous: `String<N>` may parse `N` as a type
                // even when `String` expects a const generic arg. When a type-arg is a path that
                // resolves as a value const/trait-const, lower it as a const-ty argument so
                // downstream `TyId::app` sees a const generic.
                if let Some(hir_ty) = ty_arg.ty.to_opt()
                    && let HirTyKind::Path(path) = hir_ty.data(db)
                    && let Some(path) = path.to_opt()
                    && let Ok(resolved) = resolve_path(db, path, scope, assumptions, true)
                {
                    match resolved {
                        PathRes::Const(const_def, ty) => {
                            if let Some(body) = const_def.body(db).to_opt() {
                                let const_ty =
                                    ConstTyId::from_body(db, body, Some(ty), Some(const_def));
                                return TyId::const_ty(db, const_ty);
                            }
                            return TyId::invalid(db, InvalidCause::ParseError);
                        }
                        PathRes::TraitConst(recv_ty, inst, name) => {
                            let mut args = inst.args(db).clone();
                            if let Some(self_arg) = args.first_mut() {
                                *self_arg = recv_ty;
                            }
                            let inst = TraitInstId::new(
                                db,
                                inst.def(db),
                                args,
                                inst.assoc_type_bindings(db).clone(),
                            );

                            if let Some(const_ty) =
                                super::const_ty::const_ty_from_trait_const(db, inst, name)
                            {
                                return TyId::const_ty(db, const_ty);
                            }
                            if let Some(expected_ty) = inst
                                .def(db)
                                .const_(db, name)
                                .and_then(|v| v.ty_binder(db))
                                .map(|b| b.instantiate(db, inst.args(db)))
                            {
                                let expr =
                                    ConstExprId::new(db, ConstExpr::TraitConst { inst, name });
                                let const_ty =
                                    ConstTyId::new(db, ConstTyData::Abstract(expr, expected_ty));
                                return TyId::const_ty(db, const_ty);
                            }
                        }
                        PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                            if let TyData::ConstTy(const_ty) = ty.data(db) {
                                return TyId::const_ty(db, *const_ty);
                            }
                        }
                        _ => {}
                    }
                }
                lower_opt_hir_ty(db, ty_arg.ty, scope, assumptions)
            }
            GenericArg::Const(const_arg) => {
                let const_ty = ConstTyId::from_opt_body(db, const_arg.body);
                TyId::const_ty(db, const_ty)
            }

            GenericArg::AssocType(_assoc_type_arg) => {
                // TODO: ?
                TyId::invalid(db, InvalidCause::Other)
            }
        })
        .collect()
}

#[salsa::interned]
#[derive(Debug)]
pub struct GenericParamTypeSet<'db> {
    #[return_ref]
    pub(crate) params_precursor: Vec<TyParamPrecursor<'db>>,
    pub(crate) scope: ScopeId<'db>,
    offset_to_explicit: usize,
}

impl<'db> GenericParamTypeSet<'db> {
    pub(crate) fn params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        evaluate_params_precursor(db, self)
    }

    pub(crate) fn explicit_params(self, db: &'db dyn HirAnalysisDb) -> &'db [TyId<'db>] {
        let offset = self.offset_to_explicit(db);
        &self.params(db)[offset..]
    }

    pub(crate) fn empty(db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Self {
        Self::new(db, Vec::new(), scope, 0)
    }

    pub(crate) fn trait_self(&self, db: &'db dyn HirAnalysisDb) -> Option<TyId<'db>> {
        let params = self.params_precursor(db);
        let cand = params.first()?;

        if cand.is_trait_self() {
            Some(cand.evaluate(db, self.scope(db), 0))
        } else {
            None
        }
    }

    pub(crate) fn offset_to_explicit_params_position(&self, db: &dyn HirAnalysisDb) -> usize {
        self.offset_to_explicit(db)
    }

    pub(crate) fn param_by_original_idx(
        &self,
        db: &'db dyn HirAnalysisDb,
        original_idx: usize,
    ) -> Option<TyId<'db>> {
        let idx = self.offset_to_explicit(db) + original_idx;
        self.params_precursor(db)
            .get(idx)
            .map(|p| p.evaluate(db, self.scope(db), idx))
    }

    /// Given explicit generic args provided at the use site, append any trailing
    /// defaults from this param set and return the completed explicit arg list.
    ///
    /// - `provided_explicit`: args corresponding to the explicit params (i.e.,
    ///   skipping implicit ones like trait `Self`).
    /// - `implicit_bindings`: mapping of (lowered_idx -> TyId) for implicit
    ///   parameters that should be available when evaluating defaults (e.g.,
    ///   trait `Self` at index 0).
    pub(crate) fn complete_explicit_args_with_defaults(
        self,
        db: &'db dyn HirAnalysisDb,
        trait_self: Option<TyId<'db>>,
        provided_explicit: &[TyId<'db>],
        assumptions: PredicateListId<'db>,
    ) -> Vec<TyId<'db>> {
        let total = self.params_precursor(db).len();
        let offset = self.offset_to_explicit(db);

        // mapping from lowered param idx -> bound arg, used to substitute in defaults
        let mut mapping = vec![];
        if let Some(self_ty) = trait_self {
            mapping.push(Some(self_ty));
        }
        mapping.extend(provided_explicit.iter().map(|ty| Some(*ty)));
        mapping.resize(total, None);

        // Helper folder to substitute known params when lowering defaults
        struct ParamSubst<'a, 'db> {
            db: &'db dyn HirAnalysisDb,
            mapping: &'a [Option<TyId<'db>>],
        }
        impl<'a, 'db> TyFolder<'db> for ParamSubst<'a, 'db> {
            fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
                match ty.data(self.db) {
                    TyData::TyParam(param) => {
                        if let Some(Some(rep)) = self.mapping.get(param.idx) {
                            return *rep;
                        }
                        ty.super_fold_with(db, self)
                    }
                    TyData::ConstTy(const_ty) => {
                        if let super::const_ty::ConstTyData::TyParam(param, _) =
                            const_ty.data(self.db)
                            && let Some(Some(rep)) = self.mapping.get(param.idx)
                        {
                            return *rep;
                        }
                        ty.super_fold_with(db, self)
                    }
                    _ => ty.super_fold_with(db, self),
                }
            }
        }

        // Build the returned explicit arg list, appending defaults where available.
        let mut result: Vec<TyId<'db>> = provided_explicit.to_vec();
        let scope = self.scope(db);
        for i in (offset + provided_explicit.len())..total {
            let prec = &self.params_precursor(db)[i];

            if let Some(hir_ty) = prec.default_hir_ty {
                let lowered = lower_hir_ty(db, hir_ty, scope, assumptions);
                let lowered = {
                    let mut subst = ParamSubst {
                        db,
                        mapping: &mapping,
                    };
                    lowered.fold_with(db, &mut subst)
                };
                mapping[i] = Some(lowered);
                result.push(lowered);
                continue;
            }

            if let Some(default) = prec.default_hir_const {
                let expected = prec.evaluate(db, scope, i).const_ty_ty(db);
                let const_ty = ConstTyId::from_body(db, default, expected, None);
                let lowered = TyId::const_ty(db, const_ty);
                let lowered = lowered
                    .evaluate_const_ty(db, expected)
                    .unwrap_or_else(|cause| TyId::invalid(db, cause));

                let lowered = {
                    let mut subst = ParamSubst {
                        db,
                        mapping: &mapping,
                    };
                    lowered.fold_with(db, &mut subst)
                };

                mapping[i] = Some(lowered);
                result.push(lowered);
                continue;
            }

            break; // Missing non-default; stop filling further params
        }

        result
    }
}

struct GenericParamCollector<'db> {
    db: &'db dyn HirAnalysisDb,
    owner: GenericParamOwner<'db>,
    params: Vec<TyParamPrecursor<'db>>,
    offset_to_original: usize,
}

impl<'db> GenericParamCollector<'db> {
    fn new(db: &'db dyn HirAnalysisDb, owner: GenericParamOwner<'db>) -> Self {
        let mut params = match owner {
            GenericParamOwner::Trait(_) => {
                vec![TyParamPrecursor::trait_self(db, None)]
            }

            GenericParamOwner::Func(func) if func.is_associated_func(db) => {
                let parent = owner.parent(db).unwrap();
                collect_generic_params(db, parent)
                    .params_precursor(db)
                    .to_vec()
            }

            _ => vec![],
        };

        // For each effect parameter, insert an implicit generic parameter that carries the
        // concrete "provider type". This allows monomorphization to treat effects as ordinary
        // implicit generics and substitute a concrete provider at call sites.
        if let GenericParamOwner::Func(func) = owner {
            let mut provider_idx = 0usize;
            for effect in func.effect_params(db) {
                let Some(key_path) = effect.key_path(db) else {
                    continue;
                };
                if !matches!(
                    effect_key_kind(db, key_path, func.scope()),
                    EffectKeyKind::Type | EffectKeyKind::Trait
                ) {
                    continue;
                }

                let name = IdentId::new(db, format!("__effprov{provider_idx}"));
                provider_idx += 1;
                let prec_idx = params.len();
                params.push(TyParamPrecursor::effect_provider_param(
                    Partial::Present(name),
                    prec_idx,
                ));
            }
        }

        let offset_to_original = params.len();
        Self {
            db,
            owner,
            params,
            offset_to_original,
        }
    }

    fn collect_generic_params(&mut self) {
        let hir_db = self.db;
        let params = self.owner.params(hir_db);
        for (idx, param) in params
            .map(|GenericParamView { param, .. }| param)
            .enumerate()
        {
            let idx = idx + self.offset_to_original;

            match param {
                GenericParam::Type(param) => {
                    let name = param.name;

                    let kind = lower_kind_in_bounds(param.bounds.as_slice());
                    let default_hir_ty = param.default_ty;
                    self.params
                        .push(TyParamPrecursor::ty_param(name, idx, kind, default_hir_ty));
                }

                GenericParam::Const(param) => {
                    let name = param.name;
                    let hir_ty = param.ty.to_opt();
                    let default = param.default;

                    self.params
                        .push(TyParamPrecursor::const_ty_param(name, idx, hir_ty, default))
                }
            }
        }
    }

    fn collect_kind_in_where_clause(&mut self) {
        let Some(where_clause_owner) = self.owner.where_clause_owner() else {
            return;
        };

        let hir_db = self.db;
        let where_clause = where_clause_owner.clause(hir_db);
        for pred in where_clause.predicates(hir_db) {
            let Some(kind) = pred.kind(self.db) else {
                continue;
            };

            // Kind bound on a concrete type parameter in this owner.
            if let Some(orig_idx) = pred.param_original_index(hir_db) {
                let idx = orig_idx + self.offset_to_original;
                if let Some(param) = self.params.get_mut(idx)
                    && param.kind.is_none()
                    && !param.is_const_ty()
                {
                    param.kind = Some(kind.clone());
                }
                continue;
            }

            // Kind bound on `Self` in a trait owner.
            if pred.is_self_subject(hir_db)
                && matches!(self.owner, GenericParamOwner::Trait(_))
                && let Some(trait_self) = self.trait_self_ty_mut()
                && trait_self.kind.is_none()
            {
                trait_self.kind = Some(kind);
            }
        }
    }

    fn finalize(mut self) -> GenericParamTypeSet<'db> {
        self.collect_generic_params();
        self.collect_kind_in_where_clause();

        GenericParamTypeSet::new(
            self.db,
            self.params,
            self.owner.scope(),
            self.offset_to_original,
        )
    }

    fn trait_self_ty_mut(&mut self) -> Option<&mut TyParamPrecursor<'db>> {
        let cand = self.params.get_mut(0)?;
        cand.is_trait_self().then_some(cand)
    }
}

#[doc(hidden)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyParamPrecursor<'db> {
    name: Partial<IdentId<'db>>,
    original_idx: Option<usize>,
    kind: Option<Kind>,
    variant: Variant<'db>,
    default_hir_ty: Option<HirTyId<'db>>, // Only used for type params
    default_hir_const: Option<Body<'db>>, // Only used for const params
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Variant<'db> {
    TraitSelf,
    Normal,
    Const(Option<HirTyId<'db>>),
    EffectProvider,
}

impl<'db> TyParamPrecursor<'db> {
    fn evaluate(
        &self,
        db: &'db dyn HirAnalysisDb,
        scope: ScopeId<'db>,
        lowered_idx: usize,
    ) -> TyId<'db> {
        let Partial::Present(name) = self.name else {
            return TyId::invalid(db, InvalidCause::Other);
        };

        let kind = self.kind.clone().unwrap_or(Kind::Star);

        match self.variant {
            Variant::TraitSelf => {
                let param = TyParam::trait_self(db, kind, scope);
                TyId::new(db, TyData::TyParam(param))
            }
            Variant::Normal => {
                let param = TyParam::normal_param(name, lowered_idx, kind, scope);
                TyId::new(db, TyData::TyParam(param))
            }
            Variant::EffectProvider => {
                let param = TyParam::effect_provider_param(name, lowered_idx, scope);
                TyId::new(db, TyData::TyParam(param))
            }
            Variant::Const(Some(ty)) => {
                let param = TyParam::normal_param(name, lowered_idx, kind, scope);
                let ty = lower_const_ty_ty(db, scope, ty, PredicateListId::empty_list(db)); // xxx fixme
                let const_ty = ConstTyId::new(db, ConstTyData::TyParam(param, ty));
                TyId::new(db, TyData::ConstTy(const_ty))
            }
            Variant::Const(None) => TyId::invalid(db, InvalidCause::Other),
        }
    }

    fn ty_param(
        name: Partial<IdentId<'db>>,
        idx: usize,
        kind: Option<Kind>,
        default_hir_ty: Option<HirTyId<'db>>,
    ) -> Self {
        Self {
            name,
            original_idx: idx.into(),
            kind,
            variant: Variant::Normal,
            default_hir_ty,
            default_hir_const: None,
        }
    }

    fn const_ty_param(
        name: Partial<IdentId<'db>>,
        idx: usize,
        ty: Option<HirTyId<'db>>,
        default: Option<Body<'db>>,
    ) -> Self {
        Self {
            name,
            original_idx: idx.into(),
            kind: None,
            variant: Variant::Const(ty),
            default_hir_ty: None,
            default_hir_const: default,
        }
    }

    fn effect_provider_param(name: Partial<IdentId<'db>>, idx: usize) -> Self {
        Self {
            name,
            original_idx: idx.into(),
            kind: Some(Kind::Star),
            variant: Variant::EffectProvider,
            default_hir_ty: None,
            default_hir_const: None,
        }
    }

    fn trait_self(db: &'db dyn HirAnalysisDb, kind: Option<Kind>) -> Self {
        let name = Partial::Present(IdentId::make_self_ty(db));
        Self {
            name,
            original_idx: None,
            kind,
            variant: Variant::TraitSelf,
            default_hir_ty: None,
            default_hir_const: None,
        }
    }

    fn is_trait_self(&self) -> bool {
        self.original_idx.is_none()
    }

    fn is_const_ty(&self) -> bool {
        matches!(self.variant, Variant::Const(_))
    }
}

pub(super) fn lower_kind(kind: &HirKindBound) -> Kind {
    match kind {
        HirKindBound::Mono => Kind::Star,
        HirKindBound::Abs(lhs, rhs) => match (lhs, rhs) {
            (Partial::Present(lhs), Partial::Present(rhs)) => {
                Kind::Abs(Box::new((lower_kind(lhs), lower_kind(rhs))))
            }
            (Partial::Present(lhs), Partial::Absent) => {
                Kind::Abs(Box::new((lower_kind(lhs), Kind::Any)))
            }
            (Partial::Absent, Partial::Present(rhs)) => {
                Kind::Abs(Box::new((Kind::Any, lower_kind(rhs))))
            }
            (Partial::Absent, Partial::Absent) => Kind::Abs(Box::new((Kind::Any, Kind::Any))),
        },
    }
}

/// Helper for extracting a lowered kind from a slice of HIR `TypeBound`s.
/// Returns the first kind bound if present.
pub(super) fn lower_kind_in_bounds<'db>(bounds: &[TypeBound<'db>]) -> Option<Kind> {
    for bound in bounds {
        if let TypeBound::Kind(Partial::Present(k)) = bound {
            return Some(lower_kind(k));
        }
    }
    None
}
