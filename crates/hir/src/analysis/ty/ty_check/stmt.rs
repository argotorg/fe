use salsa::Update;

use crate::analysis::{
    HirAnalysisDb,
    name_resolution::method_selection::{MethodCandidate, select_method_candidate},
};
use crate::core::hir_def::{
    BindingMarker, Expr, ExprId, IdentId, Partial, Pat, PatId, Stmt, StmtId, Trait,
};
use crate::span::DynLazySpan;

use super::{
    AccessBinding, Callable, LocalBinding, TyChecker,
    env::{ExprProp, TraitObligation, TraitObligationOrigin},
};
use crate::analysis::ty::{
    canonical::Canonicalized,
    corelib::resolve_core_trait,
    diagnostics::{BodyDiag, MarkerPosition},
    fold::{TyFoldable, TyFolder},
    shape::Shape,
    trait_def::TraitInstId,
    trait_resolution::{GoalSatisfiability, TraitSolveCx, is_goal_satisfiable},
    ty_def::{BorrowKind, InvalidCause, TyId},
    visitor::TyVisitable,
};

/// A step of the protocol a `for` loop runs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ForLoopStep {
    /// `start`: the first cursor or driver state.
    Start,
    /// `next`: the state after one.
    Next,
    /// `at`, or a producer's `produce`: the step's element.
    At,
}

impl ForLoopStep {
    pub const ALL: [Self; 3] = [Self::Start, Self::Next, Self::At];
}

/// What a loop's body receives from each step.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Update)]
pub enum ForLoopItem {
    /// A copy of the element and of what a driver yields beside it, taken
    /// before the body runs, so the element's session ends first.
    Copy,
    /// The element's access, open while the body uses it.
    Access(BorrowKind),
    /// A producer's owned value.
    Produced,
}

/// A call of the loop protocol, resolved.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ForLoopCall<'db> {
    pub callable: Callable<'db>,
    pub effect_args: Vec<super::ResolvedEffectArg<'db>>,
}

/// How a `for` loop reaches its elements: the base it holds, the driver its
/// calls take first if it has one, the calls it selects and what its body
/// receives.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ForLoopPlan<'db> {
    /// The base the loop holds.
    pub base: ExprId,
    /// The driver of `for .. by d`, or the method chain `for x in xs.reversed()`
    /// stands for `for x in xs by xs.reversed()`.
    pub driver: Option<ExprId>,
    pub state_ty: TyId<'db>,
    /// What an `at` or `produce` call yields: the element's access, beside
    /// what a driver yields with it, or a producer's value.
    pub shape: Shape<'db>,
    pub item: ForLoopItem,
    /// The pattern binds the element alone: a driver yields nothing beside it.
    pub binds_element: bool,
    /// The calls, in `ForLoopStep` order.
    pub calls: [ForLoopCall<'db>; 3],
}

impl<'db> ForLoopPlan<'db> {
    pub fn call(&self, step: ForLoopStep) -> &ForLoopCall<'db> {
        &self.calls[step as usize]
    }

    /// The shape of what the pattern binds.
    pub fn pattern_shape(&self) -> &Shape<'db> {
        match &self.shape {
            Shape::Tuple(parts) if self.binds_element => &parts[1],
            shape => shape,
        }
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
            // An access marker on the initializer binds a single name only.
            if !matches!(
                pat.data(self.db, self.body()),
                Partial::Present(Pat::Path(..) | Pat::WildCard)
            ) {
                self.take_initializer_marker(*expr, &[*pat], MarkerPosition::Initializer);
            }
            self.check_pat(*pat, prop.ty);
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
                self.push_diag(BodyDiag::RefutableLetPattern {
                    primary: pat.span(self.body()).into(),
                });
            }
        } else {
            let ascription = ascription.unwrap_or_else(|| self.fresh_ty());
            if let Some(diag) = ascription.emit_wf_diag(
                self.db,
                TraitSolveCx::new(self.db, self.env.scope()),
                self.env.assumptions(),
                span.ty().into(),
            ) {
                self.push_diag(diag);
            }
            self.check_pat(*pat, ascription);
        }
        self.env.flush_pending_bindings();
        TyId::unit(self.db)
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
        let Stmt::For(pat, base, driver, body, _unroll) = stmt_data else {
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
        let driver = driver.map(|driver| {
            let driver_ty = self
                .check_expr_unknown(driver)
                .ty
                .fold_with(self.db, &mut self.table);
            (driver, driver_ty)
        });
        // The item pattern chooses the traversal: a read one, unless the item
        // is bound `mut`, which opens the base mutably.
        let mut plan = self.plan_for_loop(base, driver, false);
        if let Some(read) = &plan {
            let item = self.loop_item_pat(*pat, read);
            // A method chain's plan holds the receiver it drives.
            let held = read.base;
            self.take_initializer_marker(held, item.as_slice(), MarkerPosition::LoopBase);
            // Over a mutable base, a `mut` item would write the elements.
            if self.env.typed_expr(held).is_some_and(|prop| {
                prop.is_mut || matches!(prop.shape, Some(Shape::Access(BorrowKind::Mut, _)))
            }) {
                self.record_mutable_sources(item.as_slice());
            }
            if let Some(mut_pat) =
                item.and_then(|item| self.first_marked_binding(item, BindingMarker::Mut))
                && self.open_loop_base(held, mut_pat)
            {
                plan = self.plan_for_loop(base, driver, true);
            }
        }
        match plan {
            Some(mut plan) => {
                // `for ref x in c` holds each element's access, `Copy` or not.
                if plan.item == ForLoopItem::Copy
                    && self
                        .first_marked_binding(*pat, BindingMarker::Ref)
                        .is_some()
                {
                    plan.item = ForLoopItem::Access(BorrowKind::Ref);
                }
                let pattern_shape = plan.pattern_shape().clone();
                self.check_pat(*pat, pattern_shape.erased_ty(self.db));
                if let ForLoopItem::Access(_) = plan.item {
                    let authority = self.expr_has_authority(base.0);
                    self.bind_pattern_accesses(
                        *pat,
                        &pattern_shape,
                        authority,
                        AccessBinding::Result,
                    );
                }
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

    /// The part of the loop pattern `pat` binding the element `plan` yields:
    /// the item beside a driver's `Extra`, or the whole pattern. A
    /// producer's values are no items.
    fn loop_item_pat(&self, pat: PatId, plan: &ForLoopPlan<'db>) -> Option<PatId> {
        if plan.item == ForLoopItem::Produced {
            return None;
        }
        match (pat.data(self.db, self.body()), &plan.shape) {
            (Partial::Present(Pat::Tuple(parts)), Shape::Tuple(_))
                if parts.len() == 2 && !plan.binds_element =>
            {
                Some(parts[1])
            }
            _ => Some(pat),
        }
    }

    /// Opens the loop base `base` mutably for the `mut` item `mut_pat`, as a
    /// `mut` component opens a matched place, returning whether it could: a
    /// `mut` result already is open, and a place must be mutable. A read
    /// result or an owned value has no mutable place, which the item's
    /// binding reports.
    fn open_loop_base(&mut self, base: ExprId, mut_pat: PatId) -> bool {
        let Some(prop) = self.env.typed_expr(base) else {
            return false;
        };
        match prop.shape {
            Some(Shape::Access(BorrowKind::Mut, _)) => return true,
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

    /// The protocol a loop over `expr`, of type `ty`, runs: its driver's, or
    /// the collection's own. Without a driver, a method chain that drives a
    /// collection it passes through, `for x in xs.reversed()`, is a driver
    /// over it.
    fn plan_for_loop(
        &mut self,
        base: (ExprId, TyId<'db>),
        driver: Option<(ExprId, TyId<'db>)>,
        mutates: bool,
    ) -> Option<ForLoopPlan<'db>> {
        let (expr, ty) = base;
        for (expr, ty) in [base].into_iter().chain(driver) {
            if ty.has_invalid(self.db) {
                return None;
            }
            if ty.is_never(self.db) || ty.base_ty(self.db).is_ty_var(self.db) {
                self.push_diag(BodyDiag::TypeMustBeKnown(expr.span(self.body()).into()));
                return None;
            }
        }
        let plan = match (driver, self.chain_base(expr, ty)) {
            (None, Some(chain_base)) => self.plan_protocol(chain_base, Some(base), mutates),
            _ => self.plan_protocol(base, driver, mutates),
        };
        if plan.is_none() {
            let (expr, ty, name) = match driver {
                Some((driver, driver_ty)) => (driver, driver_ty, "Driver"),
                None => (expr, ty, "Collection"),
            };
            let name = if mutates {
                format!("{name}Mut")
            } else {
                name.to_string()
            };
            self.push_diag(BodyDiag::TraitNotImplemented {
                primary: expr.span(self.body()).into(),
                ty: ty.pretty_print(self.db).to_string(),
                trait_name: IdentId::new(self.db, name),
            });
        }
        plan
    }

    /// The receiver a method chain of type `ty` drives, the nearest first:
    /// `xs` in `xs.reversed().take(3)`, and the projected `buf.span()` in
    /// `buf.span().reversed()`.
    fn chain_base(&mut self, expr: ExprId, ty: TyId<'db>) -> Option<(ExprId, TyId<'db>)> {
        let traits = ["Driver", "Producer"]
            .map(|name| resolve_core_trait(self.db, self.env.scope(), &["iter", name]));
        let solve_cx =
            TraitSolveCx::new(self.db, self.env.scope()).with_assumptions(self.env.assumptions());
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
                let goal = TraitInstId::new_simple(self.db, trait_def, vec![ty, receiver_ty]);
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

    /// The protocol of a loop over `base`, of type `base_ty`, run by `driver`
    /// or by the collection itself. A loop that mutates its elements selects
    /// the `mut` form of `at`; otherwise a producer's owned values come
    /// before a driver's elements, which the body sees as copies when they
    /// are `Copy` and as `ref` accesses otherwise.
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
        let (protocol, inst, at_trait, at_inst, at_name) = match driver {
            None => {
                let collection = core_trait(self, "Collection")?;
                let inst = select(self, collection, "start")?;
                if mutates {
                    let collection_mut = core_trait(self, "CollectionMut")?;
                    let at_inst = select(self, collection_mut, "at_mut")?;
                    (collection, inst, collection_mut, at_inst, "at_mut")
                } else {
                    (collection, inst, collection, inst, "at")
                }
            }
            Some(_) => {
                let driver_trait = core_trait(self, "Driver")?;
                let producer = core_trait(self, "Producer")?;
                if mutates {
                    let driver_mut = core_trait(self, "DriverMut")?;
                    let at_inst = select(self, driver_mut, "at_mut")?;
                    let inst = select(self, driver_trait, "start")?;
                    (driver_trait, inst, driver_mut, at_inst, "at_mut")
                } else if let Some(inst) = select(self, producer, "produce") {
                    (producer, inst, producer, inst, "produce")
                } else {
                    let inst = select(self, driver_trait, "start")?;
                    (driver_trait, inst, driver_trait, inst, "at")
                }
            }
        };
        let call = |this: &mut Self, trait_def: Trait<'db>, inst, name: &str, inputs| {
            let method = *trait_def
                .method_defs(this.db)
                .get(&IdentId::new(this.db, name.to_string()))?;
            let func_ty = this.instantiate_trait_method_to_term(method, receiver_ty, inst);
            let mut callable = Callable::new(this.db, func_ty, span.clone(), Some(inst)).ok()?;
            callable.set_checked_input_tys(inputs);
            let effect_args = this.resolve_callable_effects(span.clone(), &mut callable);
            Some(ForLoopCall {
                callable,
                effect_args,
            })
        };
        let start = call(self, protocol, inst, "start", inputs.clone())?;
        let state_ty = *self
            .normalize_ty(start.callable.ret_ty(self.db))
            .generic_args(self.db)
            .first()?;
        let with_state = || inputs.iter().copied().chain([state_ty]).collect::<Vec<_>>();
        let next = call(self, protocol, inst, "next", with_state())?;
        let at = call(self, at_trait, at_inst, at_name, with_state())?;
        let shape = match at.callable.ret_shape(self.db) {
            Some(shape) => shape.map_tys(&mut |ty| self.normalize_ty(ty)),
            None => Shape::Owned(self.normalize_ty(at.callable.ret_ty(self.db))),
        };
        // The element, beside what a driver yields with it.
        let element = match &shape {
            Shape::Tuple(parts) => parts.get(1).map(|part| part.erased_ty(self.db)),
            Shape::Access(_, element) => Some(*element),
            _ => None,
        };
        let item = match element {
            None => ForLoopItem::Produced,
            Some(_) if mutates => ForLoopItem::Access(BorrowKind::Mut),
            Some(element) if self.ty_is_copy(element) => ForLoopItem::Copy,
            Some(_) => ForLoopItem::Access(BorrowKind::Ref),
        };
        // A driver that yields nothing beside its elements binds them alone.
        let binds_element = match &shape {
            Shape::Tuple(parts) => parts[0] == Shape::Owned(TyId::unit(self.db)),
            _ => true,
        };
        Some(ForLoopPlan {
            base,
            driver: driver.map(|(driver, _)| driver),
            state_ty,
            shape,
            item,
            binds_element,
            calls: [start, next, at],
        })
    }

    /// The instance of `trait_def`, selected through its method `method`, that
    /// a loop's receiver of type `receiver_ty` implements: for a driver, over
    /// the base of type `base_ty`.
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
        let snapshot = self.snapshot_state();
        let inst = canonical.extract_solution(&mut self.table, cand.inst);
        if let Some(base_ty) = base_ty
            && inst
                .args(self.db)
                .get(1)
                .is_none_or(|&arg| self.table.unify(arg, base_ty).is_err())
        {
            self.rollback_state(snapshot);
            return None;
        }
        self.commit_state(snapshot);
        let inst = inst.fold_with(self.db, &mut self.table);
        if confirm || base_ty.is_some() {
            self.env.register_trait_obligation(TraitObligation {
                goal: inst,
                origin: TraitObligationOrigin::GenericConfirmation,
                span,
            });
        }
        Some(inst)
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
