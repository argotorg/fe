use salsa::Update;

use crate::analysis::HirAnalysisDb;
use crate::core::hir_def::{ExprId, IdentId, Partial, Stmt, StmtId};

use super::{Callable, TyChecker, instantiate_trait_method};
use crate::analysis::ty::{
    canonical::Canonical,
    corelib::resolve_core_trait,
    diagnostics::BodyDiag,
    fold::{TyFoldable, TyFolder},
    trait_def::{TraitInstId, impls_for_ty},
    ty_def::{InvalidCause, TyId},
    visitor::TyVisitable,
};

/// Resolved Seq trait methods for a for-loop.
///
/// This stores the pre-resolved `Callable` for `Seq::len` and `Seq::get`
/// so that MIR lowering can emit direct method calls without re-resolving.
#[derive(Debug, Clone, PartialEq, Eq, Update)]
pub struct ForLoopSeq<'db> {
    /// The type being iterated over
    pub iterable_ty: TyId<'db>,
    /// The element type (T in Seq<T>)
    pub elem_ty: TyId<'db>,
    /// The trait instance (Seq<T> for the iterable type)
    pub trait_inst: TraitInstId<'db>,
    /// Resolved callable for Seq::len(self) -> usize
    pub len_callable: Callable<'db>,
    /// Resolved callable for Seq::get(self, i: usize) -> T
    pub get_callable: Callable<'db>,
    /// Resolved effect arguments for Seq::len, in callee effect-param order.
    pub len_effect_args: Vec<super::ResolvedEffectArg<'db>>,
    /// Resolved effect arguments for Seq::get, in callee effect-param order.
    pub get_effect_args: Vec<super::ResolvedEffectArg<'db>>,
}

impl<'db> TyVisitable<'db> for ForLoopSeq<'db> {
    fn visit_with<V>(&self, visitor: &mut V)
    where
        V: crate::analysis::ty::visitor::TyVisitor<'db> + ?Sized,
    {
        self.iterable_ty.visit_with(visitor);
        self.elem_ty.visit_with(visitor);
        self.trait_inst.visit_with(visitor);
        self.len_callable.visit_with(visitor);
        self.get_callable.visit_with(visitor);
    }
}

impl<'db> TyFoldable<'db> for ForLoopSeq<'db> {
    fn super_fold_with<F>(self, db: &'db dyn HirAnalysisDb, folder: &mut F) -> Self
    where
        F: TyFolder<'db>,
    {
        ForLoopSeq {
            iterable_ty: self.iterable_ty.fold_with(db, folder),
            elem_ty: self.elem_ty.fold_with(db, folder),
            trait_inst: self.trait_inst.fold_with(db, folder),
            len_callable: self.len_callable.fold_with(db, folder),
            get_callable: self.get_callable.fold_with(db, folder),
            len_effect_args: self.len_effect_args,
            get_effect_args: self.get_effect_args,
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
            Stmt::Expr(expr) => self.check_expr(*expr, expected).ty,
        }
    }

    fn check_let(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::Let(pat, ascription, expr) = stmt_data else {
            unreachable!()
        };

        let span = stmt.span(self.env.body()).into_let_stmt();

        let ascription = match ascription {
            Some(ty) => self.lower_ty(*ty, span.ty(), true),
            None => self.fresh_ty(),
        };

        if let Some(expr) = expr {
            self.check_expr(*expr, ascription);
        }

        self.check_pat(*pat, ascription);
        self.env.flush_pending_bindings();
        TyId::unit(self.db)
    }

    fn check_for(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::For(pat, expr, body) = stmt_data else {
            unreachable!()
        };

        let expr_ty = self.fresh_ty();
        let typed_expr = self
            .check_expr(*expr, expr_ty)
            .fold_with(self.db, &mut self.table);
        let expr_ty = typed_expr.ty;

        // Resolve Seq implementation and get element type
        let (elem_ty, for_loop_seq) = self.resolve_seq_info(expr_ty, *expr, stmt);

        // Store the resolved Seq info for MIR lowering
        if let Some(seq_info) = for_loop_seq {
            self.env.register_for_loop_seq(stmt, seq_info);
        }

        self.check_pat(*pat, elem_ty);

        self.env.enter_loop(stmt);
        self.env.enter_scope(*body);
        self.env.flush_pending_bindings();

        let body_ty = self.fresh_ty();
        self.check_expr(*body, body_ty);

        self.env.leave_scope();
        self.env.leave_loop();

        TyId::unit(self.db)
    }

    /// Resolve the Seq implementation for an iterable type.
    ///
    /// Returns the element type and optionally the resolved Seq methods.
    /// The ForLoopSeq is None only when there's an error (type doesn't implement Seq).
    fn resolve_seq_info(
        &mut self,
        iterable_ty: TyId<'db>,
        expr: ExprId,
        _stmt: StmtId,
    ) -> (TyId<'db>, Option<ForLoopSeq<'db>>) {
        let (base, _args) = iterable_ty.decompose_ty_app(self.db);

        // Handle invalid and unknown types
        if base.has_invalid(self.db) {
            return (TyId::invalid(self.db, InvalidCause::Other), None);
        }
        if base.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(expr.span(self.body()).into());
            self.push_diag(diag);
            return (TyId::invalid(self.db, InvalidCause::Other), None);
        }

        // Look up Seq trait (if missing, treat as invalid).
        let Some(seq_trait) = resolve_core_trait(self.db, self.env.scope(), &["seq", "Seq"]) else {
            return (TyId::invalid(self.db, InvalidCause::Other), None);
        };

        let scope_ingot = self.env.scope().ingot(self.db);
        let canonical_ty = Canonical::new(self.db, iterable_ty);

        let search_ingots = [
            Some(scope_ingot),
            iterable_ty
                .ingot(self.db)
                .filter(|&ingot| ingot != scope_ingot),
        ];

        for ingot in search_ingots.into_iter().flatten() {
            for impl_ in impls_for_ty(self.db, ingot, canonical_ty) {
                let snapshot = self.table.snapshot();
                let impl_id = impl_.skip_binder();
                if impl_id.trait_def(self.db) != seq_trait {
                    self.table.commit(snapshot);
                    continue;
                }

                // Instantiate the impl's trait instance with fresh type variables
                // and unify to get the concrete types
                let raw_trait_inst = impl_id.trait_(self.db);
                let trait_inst = self.table.instantiate_with_fresh_vars(
                    crate::analysis::ty::binder::Binder::bind(raw_trait_inst),
                );

                // Unify the trait's Self type with the iterable type
                let self_ty = trait_inst.self_ty(self.db);
                if self.table.unify(self_ty, iterable_ty).is_err() {
                    self.table.rollback_to(snapshot);
                    continue;
                }

                // Fold to resolve type variables
                use crate::analysis::ty::fold::TyFoldable;
                let trait_inst = trait_inst.fold_with(self.db, &mut self.table);

                // For Seq<T>, the trait args are [Self, T]
                let trait_args = trait_inst.args(self.db);
                if trait_args.len() < 2 {
                    self.table.rollback_to(snapshot);
                    continue;
                }
                let elem_ty = trait_args[1].fold_with(self.db, &mut self.table);

                // Resolve len and get methods from the trait
                let len_ident = IdentId::new(self.db, "len".to_string());
                let get_ident = IdentId::new(self.db, "get".to_string());

                let method_defs = seq_trait.method_defs(self.db);
                let Some(&len_method) = method_defs.get(&len_ident) else {
                    self.table.rollback_to(snapshot);
                    continue;
                };
                let Some(&get_method) = method_defs.get(&get_ident) else {
                    self.table.rollback_to(snapshot);
                    continue;
                };

                // Create Callable objects for the methods
                let span: crate::span::DynLazySpan<'db> = expr.span(self.body()).into();

                let len_func_ty = instantiate_trait_method(
                    self.db,
                    len_method,
                    &mut self.table,
                    iterable_ty,
                    trait_inst,
                );
                let Ok(len_callable) =
                    Callable::new(self.db, len_func_ty, span.clone(), Some(trait_inst))
                else {
                    self.table.rollback_to(snapshot);
                    continue;
                };

                let get_func_ty = instantiate_trait_method(
                    self.db,
                    get_method,
                    &mut self.table,
                    iterable_ty,
                    trait_inst,
                );
                let Ok(get_callable) = Callable::new(self.db, get_func_ty, span, Some(trait_inst))
                else {
                    self.table.rollback_to(snapshot);
                    continue;
                };

                let call_span: crate::span::DynLazySpan<'db> = expr.span(self.body()).into();
                let len_effect_args =
                    self.resolve_callable_effects(call_span.clone(), &len_callable);
                let get_effect_args = self.resolve_callable_effects(call_span, &get_callable);

                let for_loop_seq = ForLoopSeq {
                    iterable_ty,
                    elem_ty,
                    trait_inst,
                    len_callable,
                    get_callable,
                    len_effect_args,
                    get_effect_args,
                };

                self.table.commit(snapshot);
                return (elem_ty, Some(for_loop_seq));
            }
        }

        // Type doesn't implement Seq
        let diag = BodyDiag::TraitNotImplemented {
            primary: expr.span(self.body()).into(),
            ty: iterable_ty.pretty_print(self.db).to_string(),
            trait_name: IdentId::new(self.db, "Seq".to_string()),
        };
        self.push_diag(diag);
        (TyId::invalid(self.db, InvalidCause::Other), None)
    }

    fn check_while(&mut self, stmt: StmtId, stmt_data: &Stmt<'db>) -> TyId<'db> {
        let Stmt::While(cond, body) = stmt_data else {
            unreachable!()
        };

        self.check_expr(*cond, TyId::bool(self.db));

        self.env.enter_loop(stmt);
        self.check_expr(*body, TyId::unit(self.db));
        self.env.leave_loop();

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

        let (returned_ty, had_child_err) = if let Some(expr) = expr {
            let before = self.diags.len();
            let expected = self.fresh_ty();
            self.check_expr(*expr, expected);
            let ty = expected.fold_with(self.db, &mut self.table);
            (ty, self.diags.len() > before)
        } else {
            (TyId::unit(self.db), false)
        };

        if !had_child_err
            && !returned_ty.has_invalid(self.db)
            && self.table.unify(returned_ty, self.expected).is_err()
        {
            let func = self.env.func();
            let span = stmt.span(self.env.body());
            let diag = BodyDiag::ReturnedTypeMismatch {
                primary: span.into(),
                actual: returned_ty,
                expected: self.expected,
                func,
            };

            self.push_diag(diag);
        }

        TyId::never(self.db)
    }
}
