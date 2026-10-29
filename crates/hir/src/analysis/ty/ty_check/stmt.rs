use common::indexmap::IndexMap;
use salsa::SalsaValue;

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::method_selection::{MethodCandidate, select_method_candidate},
};
use crate::core::hir_def::{
    BindingMarker, Expr, ExprId, IdentId, ModeMarker, Partial, Pat, PatId, Stmt, StmtId, Trait,
};
use crate::span::DynLazySpan;

use super::{
    AccessBinding, Callable, LocalBinding, TyChecker,
    env::{ExprProp, ParamSite, TraitObligation, TraitObligationOrigin, hir_param},
};
use crate::analysis::ty::{
    canonical::Canonicalized,
    corelib::resolve_core_trait,
    diagnostics::{BodyDiag, MarkerPosition},
    fold::{TyFoldable, TyFolder},
    shape::Shape,
    trait_def::{AssocBinding, TraitInstId},
    trait_resolution::{GoalSatisfiability, TraitSolveCx, is_goal_satisfiable},
    ty_def::{BorrowKind, InvalidCause, TyId},
    visitor::TyVisitable,
};

/// A step of the protocol a `for` loop runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, SalsaValue)]
pub enum ForLoopStep {
    /// `start`: the first cursor or driver state.
    Start,
    /// `has`: whether a state names an element.
    Has,
    /// `next`: the state after one.
    Next,
    /// `at`, or a producer's `produce`: the step's element.
    At,
}

impl ForLoopStep {
    pub const ALL: [Self; 4] = [Self::Start, Self::Has, Self::Next, Self::At];
}

/// A call of the loop protocol, resolved.
#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub struct ForLoopCall<'db> {
    pub callable: Callable<'db>,
    pub effect_args: Vec<super::ResolvedEffectArg<'db>>,
}

/// How a `for` loop reaches its elements: the base it holds, the driver its
/// calls take first if a method chain drives the base, and the calls it
/// selects.
#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub struct ForLoopPlan<'db> {
    /// The base the loop holds.
    pub base: ExprId,
    /// The method chain that drives the base: all of `xs.reversed()` in
    /// `for x in xs.reversed()`, whose base is `xs`.
    pub driver: Option<ExprId>,
    /// Whether the driver is a producer, whose `produce` advances it.
    pub produces: bool,
    pub state_ty: TyId<'db>,
    /// What an `at` or `produce` call yields, which the pattern binds as a
    /// `let` binds it: a step's accesses, with what a driver adds beside
    /// them, or a producer's value.
    pub shape: Shape<'db>,
    /// The calls, in `ForLoopStep` order.
    pub calls: [ForLoopCall<'db>; 4],
}

impl<'db> ForLoopPlan<'db> {
    pub fn call(&self, step: ForLoopStep) -> &ForLoopCall<'db> {
        &self.calls[step as usize]
    }
}

impl<'db> TyVisitable<'db> for ForLoopPlan<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        self.state_ty.visit_with(visitor);
        self.shape.visit_with(visitor);
        for call in &self.calls {
            call.callable.visit_with(visitor);
        }
    }
}

impl<'db> TyFoldable<'db> for ForLoopPlan<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        ForLoopPlan {
            state_ty: self.state_ty.fold_with(db, folder),
            shape: self.shape.fold_with(db, folder),
            calls: self.calls.map(|call| ForLoopCall {
                callable: call.callable.fold_with(db, folder),
                effect_args: call.effect_args,
            }),
            ..self
        }
    }
}

impl<'db> TyChecker<'db> {
    pub(super) fn check_stmt(&mut self, stmt: StmtId, expected: TyId<'db>) -> TyId<'db> {
        let Partial::Present(stmt_data) = self.env.stmt_data(stmt) else {
            return TyId::invalid(self.db, InvalidCause::ParseError);
        };

        match stmt_data {
            Stmt::Let(..) => self.check_let(stmt, stmt_data),
            Stmt::For(..) => self.check_for(stmt, stmt_data),
            Stmt::While(..) => self.check_while(stmt, stmt_data),
            Stmt::Continue => self.check_continue(stmt, stmt_data),
            Stmt::Break => self.check_break(stmt, stmt_data),
            Stmt::Return(..) => self.check_return(stmt, stmt_data),
            Stmt::Yield(expr) => self.check_yield(stmt, *expr),
            Stmt::Expr(expr) => self.check_expr(*expr, expected).ty,
        }
    }

    fn check_let(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::Let(pat, ascription, expr, else_) = stmt_data else {
            unreachable!()
        };

        let span = stmt.span(self.env.body()).into_let_stmt();

        let ascription = ascription.map(|ty| self.lower_ty(ty, span.clone().ty(), true));

        if let Some(expr) = expr {
            let prop = if let Some(ascription) = ascription {
                self.check_expr(*expr, ascription)
            } else {
                self.check_expr_unknown(*expr)
            };
            // The `let` a destructured parameter's body begins with.
            let param = self.destructured_param(prop.binding);
            // An access marker on the initializer binds a single name only.
            if !matches!(
                pat.data(self.db, self.body()),
                Partial::Present(Pat::Path(..) | Pat::WildCard)
            ) {
                self.take_initializer_marker(*expr, &[*pat], MarkerPosition::Initializer);
            }
            self.check_pat(*pat, prop.ty);
            if let Some((site, idx)) = param {
                self.check_param_bindings(*pat, site, idx);
            }
            if let Some(LocalBinding::Local { pat, .. }) = self.env.pat_binding(*pat) {
                self.env
                    .set_local_borrow_provider(pat, prop.borrow_provider);
            }

            let prop = if prop.shape.is_none() && self.env.is_place_expr(*expr) {
                self.open_let_place(*pat, *expr, prop)
            } else {
                prop
            };
            if prop.shape.is_some() {
                self.bind_pattern_source(*pat, *expr, &prop, true);
            }
            if let Some(else_) = else_ {
                let else_ty = self
                    .check_expr_unknown(*else_)
                    .ty
                    .fold_with(self.db, &mut self.table);
                if !else_ty.is_never(self.db) && !else_ty.has_invalid(self.db) {
                    self.push_diag(BodyDiag::LetElseMustDiverge {
                        primary: else_.span(self.body()).into(),
                    });
                }
            } else if let Some(root) = self.env.pattern_store().root(*pat)
                && !self.env.pattern_store().is_irrefutable(self.db, root)
            {
                let primary = pat.span(self.body()).into();
                self.push_diag(if param.is_some() {
                    BodyDiag::RefutableParamPattern { primary }
                } else {
                    BodyDiag::RefutableLetPattern { primary }
                });
            }
        } else {
            let ascription = ascription.unwrap_or_else(|| self.fresh_ty());
            self.check_pat(*pat, ascription);
        }
        self.env.flush_pending_bindings();
        TyId::unit(self.db)
    }

    /// The parameter `binding` is, if its body destructures it: the `let` the
    /// body begins with reads it.
    pub(super) fn destructured_param(
        &self,
        binding: Option<LocalBinding<'db>>,
    ) -> Option<(ParamSite<'db>, usize)> {
        let LocalBinding::Param { site, idx, .. } = binding? else {
            return None;
        };
        hir_param(self.db, site, idx)?
            .is_destructured()
            .then_some((site, idx))
    }

    /// Reports each name `pat`, the pattern of parameter `idx` of `site`,
    /// binds that another parameter or an effect of `site` binds too, the
    /// one written later as the second binding. Patterns are bound after the
    /// named parameters and the effects, so the parameters' positions give
    /// the order, and an effect, written after them, is the second. A
    /// closure's parameter may shadow its enclosing body's effects.
    fn check_param_bindings(&mut self, pat: PatId, site: ParamSite<'db>, idx: usize) {
        let mut bound = Vec::new();
        self.bound_names(pat, &mut bound);
        for (binding_pat, name) in bound {
            let here: DynLazySpan<'db> = binding_pat.span(self.body()).into();
            let spans = match self.env.visible_binding(name) {
                Some(
                    binding @ LocalBinding::Param {
                        site: other,
                        idx: other_idx,
                        ..
                    },
                ) if other == site => {
                    let other = binding.def_span(&self.env);
                    Some(if other_idx < idx {
                        (here, other)
                    } else {
                        (other, here)
                    })
                }
                Some(LocalBinding::Local { pat: other, .. })
                    if self.param_pattern_sites.get(&other) == Some(&site) =>
                {
                    Some((here, other.span(self.body()).into()))
                }
                _ => None,
            };
            if let Some((primary, first)) = spans {
                self.push_diag(BodyDiag::ParamBindsTwice {
                    primary,
                    first,
                    name,
                    effect: false,
                });
            } else if matches!(site, ParamSite::Func(_) | ParamSite::ContractInit(_)) {
                self.check_effect_name(name, binding_pat.span(self.body()).into());
            }
            self.param_pattern_sites.insert(binding_pat, site);
        }
    }

    /// Reports each named parameter whose name an effect of the same
    /// signature binds too, which would hide it: the effects are bound after
    /// the parameters. A pattern's names are checked where the body binds
    /// them (`check_param_bindings`).
    pub(super) fn check_param_effect_names(&mut self) {
        let params: Vec<_> = self
            .env
            .param_bindings()
            .iter()
            .filter_map(|&param| {
                let LocalBinding::Param { site, idx, .. } = param else {
                    return None;
                };
                Some((
                    hir_param(self.db, site, idx)?.name()?,
                    param.def_span(&self.env),
                ))
            })
            .collect();
        for (name, span) in params {
            self.check_effect_name(name, span);
        }
    }

    /// Reports each name the recv arm pattern `pat` binds that an effect of
    /// the arm binds too, which the pattern, bound after the effects, would
    /// hide.
    pub(super) fn check_arm_effect_names(&mut self, pat: PatId) {
        let mut bound = Vec::new();
        self.bound_names(pat, &mut bound);
        for (binding_pat, name) in bound {
            self.check_effect_name(name, binding_pat.span(self.body()).into());
        }
    }

    /// Reports `name`, which `first` binds, if an effect binds it too.
    fn check_effect_name(&mut self, name: IdentId<'db>, first: DynLazySpan<'db>) {
        if let Some(effect @ LocalBinding::EffectParam { .. }) = self.env.visible_binding(name) {
            self.push_diag(BodyDiag::ParamBindsTwice {
                primary: effect.def_span(&self.env),
                first,
                name,
                effect: true,
            });
        }
    }

    /// The names `pat` binds, with the patterns binding them.
    fn bound_names(&self, pat: PatId, out: &mut Vec<(PatId, IdentId<'db>)>) {
        let Partial::Present(data) = pat.data(self.db, self.body()) else {
            return;
        };
        match data {
            Pat::Path(Partial::Present(path), _) => {
                if matches!(self.env.pat_binding(pat), Some(LocalBinding::Local { .. }))
                    && let Some(name) = path.as_ident(self.db)
                {
                    out.push((pat, name));
                }
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                for &pat in pats {
                    self.bound_names(pat, out);
                }
            }
            Pat::Record(_, fields) => {
                for field in fields {
                    self.bound_names(field.pat, out);
                }
            }
            Pat::Or(lhs, rhs) => {
                self.bound_names(*lhs, out);
                self.bound_names(*rhs, out);
            }
            Pat::Path(Partial::Absent, _) | Pat::WildCard | Pat::Rest | Pat::Lit(_) => {}
        }
    }

    /// What a `let` of the place `expr` binds through: a lone name takes the
    /// place's value, or a read access to a type with no value reading
    /// (`#[view]` or state-only); a pattern binds the place's components.
    fn open_let_place(&mut self, pat: PatId, expr: ExprId, prop: ExprProp<'db>) -> ExprProp<'db> {
        let Partial::Present(Pat::Path(_, marker)) = pat.data(self.db, self.body()) else {
            return self.open_matched_place(expr, prop, [pat]);
        };
        let lone_name = matches!(marker, BindingMarker::Plain | BindingMarker::Var)
            && matches!(self.env.pat_binding(pat), Some(LocalBinding::Local { .. }));
        let reads_value = prop
            .ty
            .fold_with(self.db, &mut self.table)
            .has_value_reading(self.db);
        if lone_name && reads_value {
            return prop;
        }
        // A `var` of a type with no value reading is reported at its binding.
        if !lone_name || *marker == BindingMarker::Var {
            return self.open_matched_place(expr, prop, [pat]);
        }
        let prop = ExprProp {
            borrow_provider: self.access_provider(expr),
            shape: Some(Shape::Access(BorrowKind::Ref, prop.ty)),
            ..prop
        };
        self.env.type_expr(expr, prop.clone());
        prop
    }

    fn check_for(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::For(pat, base, body, _unroll) = stmt_data else {
            unreachable!()
        };

        let expected = self.fresh_ty();
        let prop = self
            .check_expr(*base, expected)
            .fold_with(self.db, &mut self.table);
        if prop.shape.is_some() {
            self.consume_access(*base);
        }
        let base = (*base, prop.ty);
        // The pattern chooses the traversal: a read one, unless it binds an
        // access of the step `mut`, which opens the base mutably.
        let mut plan = self.plan_for_loop(base, false);
        if let Some(read) = &plan {
            let items: Vec<_> = self
                .access_pats(*pat, &read.shape)
                .into_iter()
                .map(|(item, _)| item)
                .collect();
            // A method chain's plan holds the receiver it drives.
            let held = read.base;
            self.take_initializer_marker(held, &items, MarkerPosition::LoopBase);
            // Over a mutable base, a `mut` item would write the elements.
            if self.env.typed_expr(held).is_some_and(|prop| {
                prop.is_mut || matches!(prop.shape, Some(Shape::Access(BorrowKind::Mut, _)))
            }) {
                self.record_mutable_sources(&items);
            }
            if let Some(mut_pat) = items
                .iter()
                .find_map(|&item| self.first_marked_binding(item, BindingMarker::Mut))
                && self.open_loop_base(held, mut_pat)
            {
                plan = self.plan_for_loop(base, true);
            }
        }
        match plan {
            Some(plan) => {
                // The pattern binds the step as a `let` binds a projection's
                // result.
                self.check_pat(*pat, plan.shape.erased_ty(self.db));
                if let Some(root) = self.env.pattern_store().root(*pat)
                    && !self.env.pattern_store().is_irrefutable(self.db, root)
                {
                    self.push_diag(BodyDiag::RefutableLoopPattern {
                        primary: pat.span(self.body()).into(),
                    });
                }
                let authority = self.expr_has_authority(base.0);
                self.bind_pattern_accesses(*pat, &plan.shape, authority, AccessBinding::Result);
                self.env.register_for_loop_plan(stmt, plan);
            }
            None => {
                self.check_pat(*pat, TyId::invalid(self.db, InvalidCause::Other));
            }
        }

        self.env.enter_loop(stmt);
        self.env.enter_scope(*body);
        self.env.flush_pending_bindings();

        let body_ty = self.fresh_ty();
        self.check_expr_with_discarded_result(*body, body_ty);

        self.env.leave_scope();
        self.env.leave_loop();

        TyId::unit(self.db)
    }

    /// Opens the loop base `base` mutably for the `mut` item `mut_pat`, as a
    /// `mut` component opens a matched place, returning whether it could: a
    /// `mut` result already is open, a `mut?` call's read result is asked
    /// for `mut`, and a place must be mutable. Any other read result or an
    /// owned value has no mutable place, which the item's binding reports.
    fn open_loop_base(&mut self, base: ExprId, mut_pat: PatId) -> bool {
        let Some(prop) = self.env.typed_expr(base) else {
            return false;
        };
        match prop.shape {
            Some(Shape::Access(BorrowKind::Mut, _)) => return true,
            Some(Shape::Access(BorrowKind::Ref, _)) => {
                return self.select_mut_place(base)
                    && matches!(
                        self.env.typed_expr(base).and_then(|prop| prop.shape),
                        Some(Shape::Access(BorrowKind::Mut, _))
                    );
            }
            Some(_) => return false,
            None => {}
        }
        let prop = if !prop.is_mut && self.select_mut_place(base) {
            self.env.typed_expr(base).expect("selected place is typed")
        } else {
            prop
        };
        if prop.shape.is_some() {
            return true;
        }
        if !self.env.is_place_expr(base) {
            return false;
        }
        if !prop.is_mut {
            self.report_cannot_borrow_mut(base, mut_pat.span(self.body()).into());
        }
        self.consume_access(base);
        let prop = ExprProp {
            is_mut: true,
            borrow_provider: self.access_provider(base),
            shape: Some(Shape::Access(BorrowKind::Mut, prop.ty)),
            ..prop
        };
        self.env.type_expr(base, prop);
        true
    }

    /// The protocol a loop over `expr`, of type `ty`, runs: the collection's
    /// own, or, for a method chain that drives a collection it passes
    /// through (`for x in xs.reversed()`, `for x in xs.by(d)`), the chain's
    /// driver over it.
    fn plan_for_loop(
        &mut self,
        base: (ExprId, TyId<'db>),
        mutates: bool,
    ) -> Option<ForLoopPlan<'db>> {
        let (expr, ty) = base;
        if ty.has_invalid(self.db) {
            return None;
        }
        if ty.is_never(self.db) || ty.base_ty(self.db).is_ty_var(self.db) {
            self.push_diag(BodyDiag::TypeMustBeKnown(expr.span(self.body()).into()));
            return None;
        }
        let plan = match self.chain_base(expr, ty) {
            Some(chain_base) => self.plan_protocol(chain_base, Some(base), mutates),
            None => self.plan_protocol(base, None, mutates),
        };
        if plan.is_none() {
            let primary = expr.span(self.body()).into();
            // A driver over another base, or over none of the chain's.
            let diag = match self.driver_base(ty) {
                Some(base) => {
                    let mut root = expr;
                    while let Partial::Present(Expr::MethodCall(receiver, ..)) =
                        root.data(self.db, self.body())
                    {
                        root = *receiver;
                    }
                    let receiver = self
                        .env
                        .typed_expr(root)
                        .filter(|_| root != expr)
                        .map(|prop| prop.ty.fold_with(self.db, &mut self.table))
                        .filter(|&root_ty| self.driver_base(root_ty).is_none());
                    BodyDiag::LoopDriverBase {
                        primary,
                        driver: ty,
                        base,
                        receiver,
                    }
                }
                None => BodyDiag::TraitNotImplemented {
                    primary,
                    ty: ty.pretty_print(self.db).to_string(),
                    trait_name: IdentId::new(self.db, "Collection".to_string()),
                },
            };
            self.push_diag(diag);
        }
        plan
    }

    /// The base `ty` drives if it is a driver or a producer.
    fn driver_base(&mut self, ty: TyId<'db>) -> Option<TyId<'db>> {
        let solve_cx =
            TraitSolveCx::new(self.db, self.env.scope()).with_assumptions(self.env.assumptions());
        let base = IdentId::new(self.db, "Base".to_string());
        let projection = ["Driver", "Producer"].into_iter().find_map(|name| {
            let trait_def = resolve_core_trait(self.db, self.env.scope(), &["iter", name])?;
            let inst = TraitInstId::new(
                self.db,
                trait_def,
                vec![ty],
                IndexMap::new(),
                ModeMarker::None,
            );
            matches!(
                is_goal_satisfiable(self.db, solve_cx, inst),
                GoalSatisfiability::Satisfied(_)
            )
            .then(|| inst.project_assoc_ty(self.db, base))
            .flatten()
        })?;
        Some(self.normalize_ty(projection))
    }

    /// The receiver a method chain of type `ty` drives, the nearest first:
    /// `xs` in `xs.reversed().take(3)`, and the projected `buf.span()` in
    /// `buf.span().reversed()`. A driver or producer names its base's type,
    /// `Base`, which the receiver has.
    fn chain_base(&mut self, expr: ExprId, ty: TyId<'db>) -> Option<(ExprId, TyId<'db>)> {
        let traits = ["Driver", "Producer"]
            .map(|name| resolve_core_trait(self.db, self.env.scope(), &["iter", name]));
        let solve_cx =
            TraitSolveCx::new(self.db, self.env.scope()).with_assumptions(self.env.assumptions());
        let base = AssocBinding::of(IdentId::new(self.db, "Base".to_string()));
        let mut receiver = expr;
        while let Partial::Present(Expr::MethodCall(next, ..)) = receiver.data(self.db, self.body())
        {
            receiver = *next;
            let receiver_ty = self
                .env
                .typed_expr(receiver)?
                .ty
                .fold_with(self.db, &mut self.table);
            let drives = traits.iter().flatten().any(|&trait_def| {
                let goal = TraitInstId::new(
                    self.db,
                    trait_def,
                    vec![ty],
                    IndexMap::from_iter([(base, receiver_ty)]),
                    ModeMarker::None,
                );
                !matches!(
                    is_goal_satisfiable(self.db, solve_cx, goal),
                    GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid
                )
            });
            if drives {
                return Some((receiver, receiver_ty));
            }
        }
        None
    }

    /// The protocol of a loop over `base`, of type `base_ty`, run by the
    /// method chain `driver` or by the collection itself. A loop that mutates
    /// its elements selects the `mut` form of `at`; otherwise a producer's
    /// owned values come before a driver's steps.
    fn plan_protocol(
        &mut self,
        base: (ExprId, TyId<'db>),
        driver: Option<(ExprId, TyId<'db>)>,
        mutates: bool,
    ) -> Option<ForLoopPlan<'db>> {
        let (base, base_ty) = base;
        let span: DynLazySpan<'db> = base.span(self.body()).into();
        let core_trait = |this: &Self, name: &str| {
            resolve_core_trait(this.db, this.env.scope(), &["iter", name])
        };
        // The calls take the driver first, then the base.
        let (receiver_ty, inputs) = match driver {
            Some((_, driver_ty)) => (driver_ty, vec![driver_ty, base_ty]),
            None => (base_ty, vec![base_ty]),
        };
        let select = |this: &mut Self, trait_def, method| {
            let driven = driver.map(|_| base_ty);
            this.select_loop_trait(span.clone(), receiver_ty, trait_def, method, driven)
        };
        let (protocol, inst, at_name) = match driver {
            None => {
                let collection = core_trait(self, "Collection")?;
                (collection, select(self, collection, "start")?, "at")
            }
            Some(_) => {
                let driver_trait = core_trait(self, "Driver")?;
                let producer = core_trait(self, "Producer")?;
                match select(self, producer, "produce").filter(|_| !mutates) {
                    Some(inst) => (producer, inst, "produce"),
                    None => (driver_trait, select(self, driver_trait, "start")?, "at"),
                }
            }
        };
        // A loop that mutates its elements takes them at `mut`, from a
        // `mut Collection` or a `mut Driver`.
        if mutates {
            let span = driver.map_or_else(
                || span.clone(),
                |(driver, _)| driver.span(self.body()).into(),
            );
            self.env.register_trait_obligation(TraitObligation {
                goal: inst.mut_goal(self.db),
                origin: TraitObligationOrigin::GenericConfirmation,
                span,
            });
        }
        let call = |this: &mut Self, trait_def: Trait<'db>, inst, name: &str, inputs| {
            let method = *trait_def
                .method_defs(this.db)
                .get(&IdentId::new(this.db, name.to_string()))?;
            let func_ty = this.instantiate_trait_method_to_term(method, receiver_ty, inst);
            let mut callable = Callable::new(this.db, func_ty, span.clone(), Some(inst)).ok()?;
            if mutates && method.is_maybe_mut(this.db) {
                callable.set_mode(Some(BorrowKind::Mut));
            }
            callable.set_checked_input_tys(inputs);
            let effect_args = this.resolve_callable_effects(span.clone(), &mut callable, None);
            Some(ForLoopCall {
                callable,
                effect_args,
            })
        };
        let start = call(self, protocol, inst, "start", inputs.clone())?;
        let state_ty = self.normalize_ty(start.callable.ret_ty(self.db));
        let with_state = || inputs.iter().copied().chain([state_ty]).collect::<Vec<_>>();
        let has = call(self, protocol, inst, "has", with_state())?;
        let next = call(self, protocol, inst, "next", with_state())?;
        let at = call(self, protocol, inst, at_name, with_state())?;
        let shape = match at.callable.ret_shape(self.db) {
            Some(shape) => shape.map_tys(self.db, &mut |ty| self.normalize_ty(ty)),
            None => Shape::Owned(self.normalize_ty(at.callable.ret_ty(self.db))),
        };
        Some(ForLoopPlan {
            base,
            driver: driver.map(|(driver, _)| driver),
            produces: at_name == "produce",
            state_ty,
            shape,
            calls: [start, has, next, at],
        })
    }

    /// The instance of `trait_def`, selected through its method `method`, that
    /// a loop's receiver of type `receiver_ty` implements: for a driver, one
    /// whose `Base` is `base_ty`.
    fn select_loop_trait(
        &mut self,
        span: DynLazySpan<'db>,
        receiver_ty: TyId<'db>,
        trait_def: Trait<'db>,
        method: &str,
        base_ty: Option<TyId<'db>>,
    ) -> Option<TraitInstId<'db>> {
        let canonical = Canonicalized::new(self.db, receiver_ty);
        let (cand, confirm) = match select_method_candidate(
            self.db,
            &canonical,
            IdentId::new(self.db, method.to_string()),
            self.env.scope(),
            self.env.assumptions(),
            Some(trait_def),
        ) {
            Ok(MethodCandidate::TraitMethod(cand)) => (cand, false),
            Ok(MethodCandidate::NeedsConfirmation(cand)) => (cand, true),
            _ => return None,
        };
        let inst = canonical.extract_solution(&mut self.table, cand.inst);
        // A driver's base is the loop's. Its unique solution infers a driver
        // built without naming the base, `let d = Identity {}`.
        let bindings: IndexMap<_, _> = base_ty
            .map(|base_ty| {
                let base = IdentId::new(self.db, "Base".to_string());
                (AssocBinding::of(base), base_ty)
            })
            .into_iter()
            .collect();
        let goal = TraitInstId::new(
            self.db,
            inst.def(self.db),
            inst.args(self.db).clone(),
            bindings,
            inst.mode(self.db),
        );
        if base_ty.is_some() {
            match self.trait_effect_goal_satisfiability(goal) {
                GoalSatisfiability::Satisfied(solution) => {
                    self.commit_trait_goal_solution(goal, solution);
                }
                GoalSatisfiability::UnSat(_) | GoalSatisfiability::ContainsInvalid => return None,
                GoalSatisfiability::NeedsConfirmation { .. } => {}
            }
        }
        if confirm || base_ty.is_some() {
            self.env.register_trait_obligation(TraitObligation {
                goal: goal.fold_with(self.db, &mut self.table),
                origin: TraitObligationOrigin::GenericConfirmation,
                span,
            });
        }
        Some(inst.fold_with(self.db, &mut self.table))
    }

    fn check_while(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::While(cond, body) = stmt_data else {
            unreachable!()
        };

        // Keep let-chain bindings local to the loop condition/body.
        self.env.enter_lexical_scope();
        self.check_cond(*cond);

        self.env.enter_loop(stmt);
        self.env.enter_scope(*body);
        self.env.flush_pending_bindings();
        let body_ty = self.fresh_ty();
        self.check_expr_with_discarded_result(*body, body_ty);
        self.env.leave_scope();
        self.env.clear_pending_bindings();
        self.env.leave_loop();
        self.env.leave_scope();

        TyId::unit(self.db)
    }

    fn check_continue(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        assert!(matches!(stmt_data, Stmt::Continue));

        if self.env.current_loop().is_none() {
            let span = stmt.span(self.env.body());
            let diag = BodyDiag::LoopControlOutsideOfLoop {
                primary: span.into(),
                is_break: false,
            };
            self.push_diag(diag);
        }

        TyId::never(self.db)
    }

    fn check_break(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        assert!(matches!(stmt_data, Stmt::Break));

        if self.env.current_loop().is_none() {
            let span = stmt.span(self.env.body());
            let diag = BodyDiag::LoopControlOutsideOfLoop {
                primary: span.into(),
                is_break: true,
            };
            self.push_diag(diag);
        }

        TyId::never(self.db)
    }

    fn check_return(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::Return(expr) = stmt_data else {
            unreachable!()
        };
        // In a body with `yield` statements, a bare `return` ends the slide.
        if expr.is_some() || !self.explicit_yields || self.projection_shape.is_none() {
            self.check_exit_value(stmt, *expr);
        }
        TyId::never(self.db)
    }

    fn check_yield(&mut self, stmt: StmtId, expr: ExprId) -> TyId<'db> {
        if self.projection_shape.is_some() {
            self.check_exit_value(stmt, Some(expr));
        } else {
            self.check_expr(expr, self.expected);
            self.push_diag(BodyDiag::YieldOutsideProjection {
                primary: stmt.span(self.env.body()).into(),
            });
        }
        TyId::unit(self.db)
    }

    /// Checks the value a `return` or `yield` ends its path with.
    fn check_exit_value(&mut self, stmt: StmtId, expr: Option<ExprId>) {
        let (returned_expr, returned_prop, returned_ty, had_child_err) = if let Some(expr) = expr {
            let before = self.diags.len();
            let expected = self.fresh_ty();
            let prop = self.check_expr(expr, expected);
            let ty = expected.fold_with(self.db, &mut self.table);
            (Some(expr), Some(prop), ty, self.diags.len() > before)
        } else {
            (None, None, TyId::unit(self.db), false)
        };

        let ret_ty_ok = !had_child_err
            && !returned_ty.has_invalid(self.db)
            && self.table.unify(returned_ty, self.expected).is_ok();

        if !had_child_err && !returned_ty.has_invalid(self.db) && !ret_ty_ok {
            let func = self.env.func();
            let span = stmt.span(self.env.body());
            let diag = BodyDiag::ReturnedTypeMismatch {
                primary: span.into(),
                actual: returned_ty,
                expected: self.expected,
                func,
            };

            self.push_diag(diag);
        } else if ret_ty_ok
            && let Some(expr) = returned_expr
            && let Some(shape) = self.projection_shape.clone()
        {
            self.check_yield_site(expr, &shape);
        }

        if ret_ty_ok
            && returned_expr.is_some()
            && let Some(prop) = returned_prop
            && let Some(provider) = prop.borrow_provider
        {
            self.first_return_borrow_provider.get_or_insert(provider);
        }
    }
}
