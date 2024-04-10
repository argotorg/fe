use hir::hir_def::{IdentId, Partial, Stmt, StmtId};

use super::TyChecker;
use crate::ty::{
    diagnostics::{BodyDiag, FuncBodyDiagAccumulator},
    fold::TypeFoldable,
    ty_def::{InvalidCause, TyId},
};

impl<'db> TyChecker<'db> {
    pub(super) fn check_stmt(&mut self, stmt: StmtId, expected: TyId) -> TyId {
        let Partial::Present(stmt_data) = self.env.stmt_data(stmt) else {
            return TyId::invalid(self.db, InvalidCause::Other);
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

    fn check_let(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        let Stmt::Let(pat, ascription, expr) = stmt_data else {
            unreachable!()
        };

        let span = stmt.lazy_span(self.env.body()).into_let_stmt();

        let ascription = match ascription {
            Some(ty) => self.lower_ty(*ty, span.ty_moved().into(), true),
            None => self.fresh_ty(),
        };

        if let Some(expr) = expr {
            self.check_expr(*expr, ascription);
        }

        self.check_pat(*pat, ascription);
        self.env.flush_pending_bindings();
        TyId::unit(self.db)
    }

    fn check_for(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        let Stmt::For(pat, expr, body) = stmt_data else {
            unreachable!()
        };

        let expr_ty = self.fresh_ty();
        let typed_expr = self.check_expr(*expr, expr_ty).fold_with(&mut self.table);
        let expr_ty = typed_expr.ty;

        let (base, arg) = expr_ty.decompose_ty_app(self.db);
        // TODO: We can generalize this by just checking the `expr_ty` implements
        // `Iterator` trait when `std::iter::Iterator` is implemented.
        let elem_ty = if base.is_array(self.db) {
            arg[0]
        } else if base.is_invalid(self.db) {
            TyId::invalid(self.db, InvalidCause::Other)
        } else if base.is_ty_var(self.db) {
            let diag = BodyDiag::TypeMustBeKnown(expr.lazy_span(self.body()).into());
            FuncBodyDiagAccumulator::push(self.db, diag.into());

            TyId::invalid(self.db, InvalidCause::Other)
        } else {
            let diag = BodyDiag::TraitNotImplemented {
                primary: expr.lazy_span(self.body()).into(),
                ty: expr_ty.pretty_print(self.db).to_string(),
                trait_name: IdentId::new(self.db.as_hir_db(), "Iterator".to_string()),
            };
            FuncBodyDiagAccumulator::push(self.db, diag.into());

            TyId::invalid(self.db, InvalidCause::Other)
        };

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

    fn check_while(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        let Stmt::While(cond, body) = stmt_data else {
            unreachable!()
        };

        self.check_expr(*cond, TyId::bool(self.db));

        self.env.enter_loop(stmt);
        self.check_expr(*body, TyId::unit(self.db));
        self.env.leave_loop();

        TyId::unit(self.db)
    }

    fn check_continue(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        assert!(matches!(stmt_data, Stmt::Continue));

        if self.env.current_loop().is_none() {
            let span = stmt.lazy_span(self.env.body());
            let diag = BodyDiag::LoopControlOutsideOfLoop {
                primary: span.into(),
                is_break: false,
            };
            FuncBodyDiagAccumulator::push(self.db, diag.into());
        }

        TyId::bot(self.db)
    }

    fn check_break(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        assert!(matches!(stmt_data, Stmt::Break));

        if self.env.current_loop().is_none() {
            let span = stmt.lazy_span(self.env.body());
            let diag = BodyDiag::LoopControlOutsideOfLoop {
                primary: span.into(),
                is_break: true,
            };
            FuncBodyDiagAccumulator::push(self.db, diag.into());
        }

        TyId::bot(self.db)
    }

    fn check_return(&mut self, stmt: StmtId, stmt_data: &Stmt) -> TyId {
        let Stmt::Return(expr) = stmt_data else {
            unreachable!()
        };

        let returned_ty = if let Some(expr) = expr {
            let returned_ty = self.fresh_ty();
            self.check_expr(*expr, returned_ty);
            returned_ty.fold_with(&mut self.table)
        } else {
            TyId::unit(self.db)
        };

        if self.table.unify(returned_ty, self.expected).is_err() {
            let func = self.env.func();
            let span = stmt.lazy_span(self.env.body());
            let diag = BodyDiag::returned_type_mismatch(
                self.db,
                span.into(),
                returned_ty,
                self.expected,
                func,
            );

            FuncBodyDiagAccumulator::push(self.db, diag.into());
        }

        TyId::bot(self.db)
    }
}
