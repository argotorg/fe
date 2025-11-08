mod callable;
mod env;
mod expr;
mod pat;
mod path;
mod stmt;

pub use self::path::RecordLike;
use crate::{
    hir_def::{Body, Expr, ExprId, Func, LitKind, Partial, Pat, PatId, PathId, TypeId as HirTyId},
    span::{
        DynLazySpan, expr::LazyExprSpan, pat::LazyPatSpan, path::LazyPathSpan, types::LazyTySpan,
    },
    visitor::{Visitor, VisitorCtxt, walk_expr, walk_pat},
};
pub use callable::Callable;
pub use env::ExprProp;
use env::TyCheckEnv;
pub(super) use expr::TraitOps;

use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Update;

use super::{
    diagnostics::{BodyDiag, FuncBodyDiag, TyDiagCollection, TyLowerDiag},
    fold::TyFoldable,
    trait_def::{TraitInstId, TraitMethod},
    trait_resolution::PredicateListId,
    ty_def::{InvalidCause, Kind, TyId, TyVarSort},
    ty_lower::lower_hir_ty,
    unify::{InferenceKey, UnificationError, UnificationTable},
};
use crate::analysis::ty::{normalize::normalize_ty, ty_error::collect_ty_lower_errors};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{
        PathRes, PathResError, diagnostics::PathResDiag, resolve_path_with_observer,
    },
    ty::ty_def::{TyFlags, inference_keys},
};

#[salsa::tracked(return_ref)]
pub fn check_func_body<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
    let Ok(mut checker) = TyChecker::new_with_func(db, func) else {
        return (Vec::new(), TypedBody::empty());
    };

    checker.run();
    checker.finish()
}

pub struct TyChecker<'db> {
    db: &'db dyn HirAnalysisDb,
    env: TyCheckEnv<'db>,
    table: UnificationTable<'db>,
    expected: TyId<'db>,
    diags: Vec<FuncBodyDiag<'db>>,
}

impl<'db> TyChecker<'db> {
    fn new_with_func(db: &'db dyn HirAnalysisDb, func: Func<'db>) -> Result<Self, ()> {
        let env = TyCheckEnv::new_with_func(db, func)?;
        let rt = func.return_ty(db);
        // If the return type is explicitly annotated and of invalid kind,
        // reflect that as an invalid expected type.
        let expected_ty = if func.has_explicit_return_ty(db) {
            if rt.is_star_kind(db) { rt } else { TyId::invalid(db, InvalidCause::Other) }
        } else {
            rt
        };

        Ok(Self::new(db, env, expected_ty))
    }

    fn run(&mut self) {
        let root_expr = self.env.body().expr(self.db);
        self.check_expr(root_expr, self.expected);
    }

    fn finish(self) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
        TyCheckerFinalizer::new(self).finish()
    }

    fn new(db: &'db dyn HirAnalysisDb, env: TyCheckEnv<'db>, expected: TyId<'db>) -> Self {
        let table = UnificationTable::new(db);
        Self {
            db,
            env,
            table,
            expected,
            diags: Vec::new(),
        }
    }

    fn push_diag(&mut self, diag: impl Into<FuncBodyDiag<'db>>) {
        self.diags.push(diag.into())
    }

    fn body(&self) -> Body<'db> {
        self.env.body()
    }

    fn parent_expr(&self) -> Option<&'db Expr<'db>> {
        let id = self.env.parent_expr()?;
        match &self.body().exprs(self.db)[id] {
            Partial::Present(expr) => Some(expr),
            Partial::Absent => None,
        }
    }

    fn lit_ty(&mut self, lit: &LitKind<'db>) -> TyId<'db> {
        match lit {
            LitKind::Bool(_) => TyId::bool(self.db),
            LitKind::Int(_) => self.table.new_var(TyVarSort::Integral, &Kind::Star),
            LitKind::String(s) => {
                let len_bytes = s.len_bytes(self.db);
                self.table
                    .new_var(TyVarSort::String(len_bytes), &Kind::Star)
            }
        }
    }

    fn lower_ty(
        &mut self,
        hir_ty: HirTyId<'db>,
        span: LazyTySpan<'db>,
        star_kind_required: bool,
    ) -> TyId<'db> {
        let ty = lower_hir_ty(self.db, hir_ty, self.env.scope(), self.env.assumptions());

        // If lowering failed, try to produce precise diagnostics (e.g., path resolution errors)
        if ty.has_invalid(self.db) {
            let diags = collect_ty_lower_errors(
                self.db,
                self.env.scope(),
                hir_ty,
                span.clone(),
                self.env.assumptions(),
            );
            if !diags.is_empty() {
                for d in diags {
                    self.push_diag(d);
                }
                // Avoid cascading kind errors for already-invalid types
                return TyId::invalid(self.db, InvalidCause::Other);
            }
        }

        if let Some(diag) = ty.emit_diag(self.db, span.clone().into()) {
            self.push_diag(diag)
        }

        if star_kind_required && ty.is_star_kind(self.db) {
            ty
        } else {
            let diag: TyDiagCollection = TyLowerDiag::ExpectedStarKind(span.into()).into();
            self.push_diag(diag);
            TyId::invalid(self.db, InvalidCause::Other)
        }
    }

    /// Returns the fresh type variable for pattern and expr type checking. The
    /// kind of the type variable is `*`, and the sort is `General`.
    fn fresh_ty(&mut self) -> TyId<'db> {
        self.table.new_var(TyVarSort::General, &Kind::Star)
    }

    fn fresh_tys_n(&mut self, n: usize) -> Vec<TyId<'db>> {
        (0..n).map(|_| self.fresh_ty()).collect()
    }

    fn unify_ty<T>(&mut self, t: T, actual: TyId<'db>, expected: TyId<'db>) -> TyId<'db>
    where
        T: Into<Typeable<'db>>,
    {
        let t = t.into();
        let actual = self.equate_ty(actual, expected, t.span(self.env.body()));

        match t {
            Typeable::Expr(expr, mut typed_expr) => {
                typed_expr.ty = actual;
                self.env.type_expr(expr, typed_expr)
            }
            Typeable::Pat(pat) => self.env.type_pat(pat, actual),
        }

        actual
    }

    fn equate_ty(
        &mut self,
        actual: TyId<'db>,
        expected: TyId<'db>,
        span: DynLazySpan<'db>,
    ) -> TyId<'db> {
        // FIXME: This is a temporary workaround, this should be removed when we
        // implement subtyping.
        if expected.is_never(self.db) && !actual.is_never(self.db) {
            let diag = BodyDiag::TypeMismatch {
                span,
                expected,
                given: actual,
            };
            self.push_diag(diag);
            return TyId::invalid(self.db, InvalidCause::Other);
        };

        // Resolve associated types before unification
        let actual = actual.fold_with(self.db, &mut self.table);
        let expected = expected.fold_with(self.db, &mut self.table);
        let actual = self.normalize_ty(actual);
        let expected = self.normalize_ty(expected);

        match self.table.unify(actual, expected) {
            Ok(()) => {
                // FIXME: This is a temporary workaround, this should be removed when we
                // implement subtyping.
                let actual = actual.fold_with(self.db, &mut self.table);
                if actual.is_never(self.db) {
                    expected
                } else {
                    actual
                }
            }

            Err(UnificationError::TypeMismatch) => {
                let actual = actual.fold_with(self.db, &mut self.table);
                let expected = expected.fold_with(self.db, &mut self.table);
                self.push_diag(BodyDiag::TypeMismatch {
                    span,
                    expected,
                    given: actual,
                });
                TyId::invalid(self.db, InvalidCause::Other)
            }

            Err(UnificationError::OccursCheckFailed) => {
                self.push_diag(BodyDiag::InfiniteOccurrence(span));

                TyId::invalid(self.db, InvalidCause::Other)
            }
        }
    }

    fn resolve_path(
        &mut self,
        path: PathId<'db>,
        resolve_tail_as_value: bool,
        span: LazyPathSpan<'db>,
    ) -> Result<PathRes<'db>, PathResError<'db>> {
        let scope = self.env.scope();
        let mut invisible = None;
        let mut check_visibility = |path: PathId<'db>, reso: &PathRes<'db>| {
            if invisible.is_some() {
                return;
            }
            if !reso.is_visible_from(self.db, scope) {
                invisible = Some((path, reso.name_span(self.db)));
            }
        };

        let res = match resolve_path_with_observer(
            self.db,
            path,
            scope,
            self.env.assumptions(),
            resolve_tail_as_value,
            &mut check_visibility,
        ) {
            Ok(r) => Ok(r.map_over_ty(|ty| self.table.instantiate_to_term(ty))),
            Err(err) => Err(err),
        };

        if let Some((path, deriv_span)) = invisible {
            let span = span.clone().segment(path.segment_index(self.db)).ident();
            let ident = path.ident(self.db);
            let diag = PathResDiag::Invisible(span.into(), ident.unwrap(), deriv_span);
            self.diags.push(diag.into());
        }

        res
    }

    /// Resolve associated type to concrete type if possible
    fn normalize_ty(&mut self, ty: TyId<'db>) -> TyId<'db> {
        normalize_ty(
            self.db,
            ty.fold_with(self.db, &mut self.table),
            self.env.scope(),
            self.env.assumptions(),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct TypedBody<'db> {
    body: Option<Body<'db>>,
    pat_ty: FxHashMap<PatId, TyId<'db>>,
    expr_ty: FxHashMap<ExprId, ExprProp<'db>>,
    callables: FxHashMap<ExprId, Callable<'db>>,
}

impl<'db> TypedBody<'db> {
    pub fn expr_ty(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> TyId<'db> {
        self.expr_prop(db, expr).ty
    }

    pub fn expr_prop(&self, db: &'db dyn HirAnalysisDb, expr: ExprId) -> ExprProp<'db> {
        self.expr_ty
            .get(&expr)
            .copied()
            .unwrap_or_else(|| ExprProp::invalid(db))
    }

    pub fn pat_ty(&self, db: &'db dyn HirAnalysisDb, pat: PatId) -> TyId<'db> {
        self.pat_ty
            .get(&pat)
            .copied()
            .unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
    }

    pub fn callable_expr(&self, expr: ExprId) -> Option<&Callable<'db>> {
        self.callables.get(&expr)
    }

    fn empty() -> Self {
        Self {
            body: None,
            pat_ty: FxHashMap::default(),
            expr_ty: FxHashMap::default(),
            callables: FxHashMap::default(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, derive_more::From)]
enum Typeable<'db> {
    Expr(ExprId, ExprProp<'db>),
    Pat(PatId),
}

impl Typeable<'_> {
    fn span(self, body: Body) -> DynLazySpan {
        match self {
            Self::Expr(expr, ..) => expr.span(body).into(),
            Self::Pat(pat) => pat.span(body).into(),
        }
    }
}

impl<'db> TraitMethod<'db> {
    pub fn instantiate_with_inst(
        self,
        table: &mut UnificationTable<'db>,
        receiver_ty: TyId<'db>,
        inst: TraitInstId<'db>,
    ) -> TyId<'db> {
        let db = table.db;
        let ty = TyId::foldl(db, TyId::func(db, self.0), inst.args(db));

        let inst_self = table.instantiate_to_term(inst.self_ty(db));
        table.unify(inst_self, receiver_ty).unwrap();

        let instantiated = table.instantiate_to_term(ty);

        // Apply associated type substitutions from the trait instance
        use crate::analysis::ty::fold::{AssocTySubst, TyFoldable};
        let mut subst = AssocTySubst::new(inst);
        instantiated.fold_with(db, &mut subst)
    }
}

struct TyCheckerFinalizer<'db> {
    db: &'db dyn HirAnalysisDb,
    body: TypedBody<'db>,
    assumptions: PredicateListId<'db>,
    ty_vars: FxHashSet<InferenceKey<'db>>,
    diags: Vec<FuncBodyDiag<'db>>,
}

impl<'db> Visitor<'db> for TyCheckerFinalizer<'db> {
    fn visit_pat(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>,
        pat: PatId,
        _: &Pat<'db>,
    ) {
        let ty = self.body.pat_ty(self.db, pat);
        let span = ctxt.span().unwrap();
        self.check_unknown(ty, span.clone().into());

        walk_pat(self, ctxt, pat)
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        // Skip the check if the expr is block.
        if !matches!(expr_data, Expr::Block(..)) {
            let prop = self.body.expr_prop(self.db, expr);
            let span = ctxt.span().unwrap();
            self.check_unknown(prop.ty, span.clone().into());
            if prop.binding.is_none() {
                self.check_wf(prop.ty, span.into());
            }
        }

        // We need this additional check for method call because the callable type is
        // not tied to the expression type.
        if let Expr::MethodCall(..) = expr_data
            && let Some(callable) = self.body.callable_expr(expr)
        {
            let callable_ty = callable.ty(self.db);
            let span = ctxt.span().unwrap().into_method_call_expr().method_name();
            self.check_unknown(callable_ty, span.clone().into());
            self.check_wf(callable_ty, span.into())
        }

        walk_expr(self, ctxt, expr);
    }

    fn visit_item(
        &mut self,
        _: &mut VisitorCtxt<'db, crate::core::visitor::prelude::LazyItemSpan<'db>>,
        _: crate::core::hir_def::ItemKind<'db>,
    ) {
    }
}

impl<'db> TyCheckerFinalizer<'db> {
    fn new(mut checker: TyChecker<'db>) -> Self {
        let assumptions = checker.env.assumptions();
        let body = checker.env.finish(&mut checker.table, &mut checker.diags);

        Self {
            db: checker.db,
            body,
            assumptions,
            ty_vars: FxHashSet::default(),
            diags: checker.diags,
        }
    }

    fn finish(mut self) -> (Vec<FuncBodyDiag<'db>>, TypedBody<'db>) {
        self.check_unknown_types();
        (self.diags, self.body)
    }

    fn check_unknown_types(&mut self) {
        if let Some(body) = self.body.body {
            let mut ctxt = VisitorCtxt::with_body(self.db, body);
            self.visit_body(&mut ctxt, body);
        }
    }

    fn check_unknown(&mut self, ty: TyId<'db>, span: DynLazySpan<'db>) {
        let flags = ty.flags(self.db);
        if flags.contains(TyFlags::HAS_INVALID) || !flags.contains(TyFlags::HAS_VAR) {
            return;
        }

        let mut skip_diag = false;
        for key in inference_keys(self.db, &ty) {
            // If at least one of the inference keys are already seen, we will skip emitting
            // diagnostics.
            skip_diag |= !self.ty_vars.insert(key);
        }

        if !skip_diag {
            let diag = BodyDiag::TypeAnnotationNeeded { span, ty };
            self.diags.push(diag.into())
        }
    }

    fn check_wf(&mut self, ty: TyId<'db>, span: DynLazySpan<'db>) {
        let flags = ty.flags(self.db);
        if flags.contains(TyFlags::HAS_INVALID) || flags.contains(TyFlags::HAS_VAR) {
            return;
        }

        let hir_db = self.db;
        let ingot = self.body.body.unwrap().top_mod(hir_db).ingot(hir_db);
        if let Some(diag) = ty.emit_wf_diag(self.db, ingot, self.assumptions, span) {
            self.diags.push(diag.into());
        }
    }
}
