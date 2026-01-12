use crate::core::hir_def::{IdentId, Partial, Stmt, StmtId};

use super::TyChecker;
use crate::analysis::ty::{
    canonical::Canonical,
    corelib::resolve_core_trait,
    diagnostics::BodyDiag,
    fold::TyFoldable,
    trait_def::impls_for_ty,
    ty_def::{InvalidCause, TyId},
};

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

        let elem_ty = self.resolve_seq_element_ty(expr_ty, *expr);

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

    /// Resolve the element type for a type that can be iterated over.
    ///
    /// Handles:
    /// 1. Arrays `[T; N]` - element type is T (special case since const generics
    ///    in array type positions aren't fully supported yet)
    /// 2. Types implementing `Seq<T>` - element type is T from the trait impl
    fn resolve_seq_element_ty(
        &mut self,
        iterable_ty: TyId<'db>,
        expr: crate::core::hir_def::ExprId,
    ) -> TyId<'db> {
        let (base, args) = iterable_ty.decompose_ty_app(self.db);

        // Handle invalid and unknown types
        if base.has_invalid(self.db) {
            return TyId::invalid(self.db, InvalidCause::Other);
        }
        if base.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(expr.span(self.body()).into());
            self.push_diag(diag);
            return TyId::invalid(self.db, InvalidCause::Other);
        }

        // Special case: arrays have element type as first type argument
        // This is needed because const generics in array type positions (e.g. `[T; N]`)
        // aren't fully supported yet, so we can't have `impl<T, const N> Seq<T> for [T; N]`
        if base.is_array(self.db) {
            return args[0];
        }

        // Look up Seq trait and check if iterable_ty implements it
        let seq_trait = resolve_core_trait(self.db, self.env.scope(), &["seq", "Seq"]);
        let ingot = self.env.body().top_mod(self.db).ingot(self.db);
        let canonical_ty = Canonical::new(self.db, iterable_ty);

        for impl_ in impls_for_ty(self.db, ingot, canonical_ty) {
            let impl_id = impl_.skip_binder();
            if impl_id.trait_def(self.db) == seq_trait {
                // For Seq<T>, the trait args are [Self, T]
                // args[0] is Self, args[1] is the element type T
                let trait_inst = impl_id.trait_(self.db);
                let trait_args = trait_inst.args(self.db);
                if trait_args.len() >= 2 {
                    return trait_args[1];
                }
            }
        }

        // Type doesn't implement Seq and is not an array
        let diag = BodyDiag::TraitNotImplemented {
            primary: expr.span(self.body()).into(),
            ty: iterable_ty.pretty_print(self.db).to_string(),
            trait_name: IdentId::new(self.db, "Seq".to_string()),
        };
        self.push_diag(diag);
        TyId::invalid(self.db, InvalidCause::Other)
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
