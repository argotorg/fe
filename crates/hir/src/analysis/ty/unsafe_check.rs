use crate::analysis::HirAnalysisDb;
use crate::analysis::place::resolve_place_field;
use crate::analysis::semantic::effect_param_site;
use crate::analysis::ty::adt_def::AdtRef;
use crate::analysis::ty::closure::{closure_effect_requirements, closure_regions};
use crate::analysis::ty::corelib::effect_key_state_access;
use crate::analysis::ty::diagnostics::{BodyDiag, FuncBodyDiag};
use crate::analysis::ty::effects::rows::expand_rows;
use crate::analysis::ty::provider::AddressSpace;
use crate::analysis::ty::shape::Shape;
use crate::analysis::ty::ty_check::{BodyOwner, TypedBody};
use crate::analysis::ty::ty_check::{RecordInitLowering, transparent_newtype_field_ty};
use crate::analysis::ty::ty_def::TyId;
use crate::analysis::ty::ty_def::{BorrowKind, ClosureDef};
use crate::core::semantic::EffectEnvView;
use crate::hir_def::{
    BinOp, BindingMarker, BlockKind, Body, CallableDef, Cond, CondId, EnumVariant, Expr, ExprId,
    Field, FieldIndex, FieldParent, FuncParamMode, IdentId, Partial, Pat, PatId, Stmt, StmtId,
    UnOp,
};
use common::ingot::IngotKind;
use rustc_hash::FxHashSet;
use std::iter;

/// Reports the unsafe operations of a body that are outside an unsafe
/// context: an `unsafe { .. }` block, or the body of an `unsafe fn`. These are
/// dereferencing a raw pointer, explicitly or by selecting a field through
/// it, calling an `unsafe fn`, and initializing or writing an `unsafe` field.
///
/// Also reports, inside unsafe contexts too, an access through a storage or
/// transient-storage pointer that the body's own effects do not cover: state
/// interference never disappears from a signature.
///
/// Nested items and anonymous const bodies are checked as their own owners,
/// and never inherit an enclosing unsafe context.
pub(crate) fn check_unsafe_ops<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    typed_body: &TypedBody<'db>,
) -> Vec<FuncBodyDiag<'db>> {
    let Some(body) = owner.body(db) else {
        return Vec::new();
    };

    let mut checker = UnsafeChecker {
        db,
        owner,
        body,
        typed_body,
        in_unsafe: matches!(owner, BodyOwner::Func(func, _) if func.is_unsafe(db)),
        written_fields: FxHashSet::default(),
        state_accesses: FxHashSet::default(),
        diags: Vec::new(),
    };
    checker.check_expr(body.expr(db));
    checker.diags
}

struct UnsafeChecker<'db, 'a> {
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    body: Body<'db>,
    typed_body: &'a TypedBody<'db>,
    /// Inside an `unsafe` block or `unsafe fn`, where unsafe operations are
    /// allowed.
    in_unsafe: bool,
    /// The `unsafe` field selections already reported as written, so a
    /// place both assigned and opened `mut` is reported once.
    written_fields: FxHashSet<ExprId>,
    /// The state-pointer dereferences already checked for authority. A write
    /// is checked before the read of the same place.
    state_accesses: FxHashSet<ExprId>,
    diags: Vec<FuncBodyDiag<'db>>,
}

impl<'db> UnsafeChecker<'db, '_> {
    /// Reports an unsafe operation, unless an unsafe context allows it.
    fn push_unsafe(&mut self, diag: BodyDiag<'db>) {
        if !self.in_unsafe {
            self.diags.push(diag.into());
        }
    }

    /// Whether `ty`, or the target of a capability of it, is a pointer into
    /// storage or transient storage.
    fn is_state_pointer(&self, ty: TyId<'db>) -> bool {
        matches!(
            ty.as_capability(self.db)
                .map_or(ty, |(_, inner)| inner)
                .as_ptr(self.db),
            Some((_, AddressSpace::Storage | AddressSpace::Transient))
        )
    }

    /// The dereference of a state pointer the place `expr` lies under: an
    /// explicit `*p`, or a field selected through `p`.
    fn state_deref_of_place(&self, expr: ExprId) -> Option<ExprId> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Un(inner, UnOp::Deref)) => self
                .is_state_pointer(self.typed_body.expr_ty(self.db, *inner))
                .then_some(expr),
            Partial::Present(Expr::Field(base, Partial::Present(field))) => {
                let base_ty = self.typed_body.expr_ty(self.db, *base);
                if self.is_state_pointer(base_ty)
                    && resolve_place_field(self.db, base_ty, *field)
                        .is_some_and(|resolved| resolved.implicit_deref_ty.is_some())
                {
                    Some(expr)
                } else {
                    self.state_deref_of_place(*base)
                }
            }
            Partial::Present(Expr::Bin(base, _, BinOp::Index)) => self.state_deref_of_place(*base),
            _ => None,
        }
    }

    /// Reports an access through the state-pointer dereference `deref` that
    /// the body's own effects do not cover, `unsafe` or not. Inside a
    /// closure those are the closure's row: a closure takes none of the
    /// enclosing body's authority, since its caller provides its row.
    fn check_state_access(&mut self, deref: ExprId, is_mut: bool) {
        if !self.state_accesses.insert(deref) {
            return;
        }
        let scope = self.body.scope();
        let assumptions = self.typed_body.assumptions();
        let closure = closure_regions(self.db, self.body).expr(deref);
        let requirements = match closure {
            Some(expr) => closure_effect_requirements(
                self.db,
                ClosureDef {
                    body: self.body,
                    expr,
                    mode: self.owner.mode(),
                },
            ),
            None => effect_param_site(self.owner)
                .map(|site| EffectEnvView::new(site).requirements(self.db))
                .unwrap_or_default(),
        };
        let rows = expand_rows(self.db, &requirements, scope, assumptions);
        let covered = requirements
            .iter()
            .chain(
                rows.components
                    .iter()
                    .map(|component| &component.requirement),
            )
            .filter(|requirement| requirement.is_mut || !is_mut)
            .any(|requirement| {
                effect_key_state_access(
                    self.db,
                    scope,
                    assumptions,
                    requirement.key.clone(),
                    requirement.is_mut,
                )
                .is_some()
            });
        if !covered {
            self.diags.push(
                BodyDiag::UncoveredStateAccess {
                    primary: deref.span(self.body).into(),
                    is_mut,
                    in_closure: closure.is_some(),
                }
                .into(),
            );
        }
    }

    /// Whether `name` is an `unsafe` field of `parent`.
    fn is_unsafe_field(&self, parent: FieldParent<'db>, name: IdentId<'db>) -> bool {
        parent
            .fields(self.db)
            .find(|field| field.name(self.db) == Some(name))
            .is_some_and(|field| field.is_unsafe(self.db))
    }

    /// The outermost `unsafe` field selection the place `expr` goes through:
    /// `self.len` in `self.len`, and `self.buf` in `self.buf[i].x`.
    fn unsafe_field_of_place(&self, expr: ExprId) -> Option<(ExprId, IdentId<'db>)> {
        match expr.data(self.db, self.body) {
            Partial::Present(Expr::Field(base, Partial::Present(field))) => {
                if let FieldIndex::Ident(name) = field
                    && let Some(resolved) = resolve_place_field(
                        self.db,
                        self.typed_body.expr_ty(self.db, *base),
                        *field,
                    )
                    && let Some(AdtRef::Struct(struct_)) = resolved.base_ty.adt_ref(self.db)
                    && self.is_unsafe_field(FieldParent::Struct(struct_), *name)
                {
                    return Some((expr, *name));
                }
                self.unsafe_field_of_place(*base)
            }
            Partial::Present(Expr::Bin(base, _, BinOp::Index)) => self.unsafe_field_of_place(*base),
            _ => None,
        }
    }

    /// Reports a write to the place `expr` if it goes through an `unsafe`
    /// field.
    fn check_place_write(&mut self, expr: ExprId) {
        if let Some(deref) = self.state_deref_of_place(expr) {
            self.check_state_access(deref, true);
        }
        if let Some((field, name)) = self.unsafe_field_of_place(expr)
            && self.written_fields.insert(field)
        {
            self.push_unsafe(BodyDiag::UnsafeFieldRequiresUnsafe {
                primary: field.span(self.body).into(),
                name,
                write: true,
            });
        }
    }

    /// Reports each `unsafe` field a record literal initializes.
    fn check_record_init(&mut self, expr: ExprId, fields: &[Field<'db>]) {
        let parent = match self.typed_body.record_init_lowering(expr) {
            Some(RecordInitLowering::Struct) => {
                match self.typed_body.expr_ty(self.db, expr).adt_ref(self.db) {
                    Some(AdtRef::Struct(struct_)) => FieldParent::Struct(struct_),
                    _ => return,
                }
            }
            Some(RecordInitLowering::EnumVariant(variant)) => FieldParent::Variant(variant.variant),
            None => return,
        };
        let span = expr.span(self.body).into_record_init_expr().fields();
        for (idx, field) in fields.iter().enumerate() {
            if let Some(name) = field.label_eagerly(self.db, self.body)
                && self.is_unsafe_field(parent, name)
            {
                self.push_unsafe(BodyDiag::UnsafeFieldRequiresUnsafe {
                    primary: span.clone().field(idx).into(),
                    name,
                    write: false,
                });
            }
        }
    }

    /// Reports the `unsafe` field a newtype cast `expr` of `inner`
    /// initializes: the only field of a layer the cast builds, one of the
    /// target's newtype layers above the first type `inner`'s type peels to.
    fn check_cast_init(&mut self, expr: ExprId, inner: ExprId) {
        let scope = self.owner.scope();
        let peel = |ty| transparent_newtype_field_ty(self.db, scope, ty);
        let from = self.typed_body.expr_ty(self.db, inner);
        let from_layers: Vec<_> = iter::successors(Some(from), |ty| peel(*ty)).collect();
        let mut ty = self.typed_body.expr_ty(self.db, expr);
        while !from_layers.contains(&ty)
            && let Some(field_ty) = peel(ty)
        {
            if let Some(AdtRef::Struct(struct_)) = ty.adt_ref(self.db)
                && let Some(field) = FieldParent::Struct(struct_).fields(self.db).next()
                && field.is_unsafe(self.db)
                && let Some(name) = field.name(self.db)
            {
                self.push_unsafe(BodyDiag::UnsafeFieldRequiresUnsafe {
                    primary: expr.span(self.body).into(),
                    name,
                    write: false,
                });
                return;
            }
            ty = field_ty;
        }
    }

    /// Reports `mut` bindings of `unsafe` fields in a record pattern, which
    /// write the field through the binding.
    fn check_pat(&mut self, pat: PatId) {
        let Partial::Present(pat_data) = pat.data(self.db, self.body) else {
            return;
        };
        match pat_data {
            Pat::Record(path, fields) => {
                let ty = self.typed_body.pat_ty(self.db, pat);
                let parent = match ty.adt_ref(self.db) {
                    Some(AdtRef::Struct(struct_)) => Some(FieldParent::Struct(struct_)),
                    Some(AdtRef::Enum(enum_)) => path
                        .to_opt()
                        .and_then(|path| path.ident(self.db).to_opt())
                        .and_then(|name| {
                            enum_
                                .variants(self.db)
                                .find(|variant| variant.name(self.db) == Some(name))
                        })
                        .map(|variant| {
                            FieldParent::Variant(EnumVariant::new(variant.owner, variant.idx))
                        }),
                    _ => None,
                };
                for field in fields {
                    if let Some(parent) = parent
                        && let Some(name) = field.label(self.db, self.body)
                        && self.is_unsafe_field(parent, name)
                        && let Some(binding) = self.mut_binding(field.pat)
                    {
                        self.push_unsafe(BodyDiag::UnsafeFieldRequiresUnsafe {
                            primary: binding.span(self.body).into(),
                            name,
                            write: true,
                        });
                    }
                    self.check_pat(field.pat);
                }
            }
            Pat::Tuple(pats) | Pat::PathTuple(_, pats) => {
                pats.iter().for_each(|pat| self.check_pat(*pat));
            }
            Pat::Or(lhs, rhs) => {
                self.check_pat(*lhs);
                self.check_pat(*rhs);
            }
            Pat::WildCard | Pat::Rest | Pat::Lit(_) | Pat::Path(..) => {}
        }
    }

    /// The first `mut` binding in `pat`.
    fn mut_binding(&self, pat: PatId) -> Option<PatId> {
        match pat.data(self.db, self.body) {
            Partial::Present(Pat::Path(_, BindingMarker::Mut)) => Some(pat),
            Partial::Present(Pat::Path(_, BindingMarker::MaybeMut))
                if self.owner.mode() == Some(BorrowKind::Mut) =>
            {
                Some(pat)
            }
            Partial::Present(Pat::Tuple(pats) | Pat::PathTuple(_, pats)) => {
                pats.iter().find_map(|pat| self.mut_binding(*pat))
            }
            Partial::Present(Pat::Record(_, fields)) => {
                fields.iter().find_map(|field| self.mut_binding(field.pat))
            }
            Partial::Present(Pat::Or(lhs, rhs)) => {
                self.mut_binding(*lhs).or_else(|| self.mut_binding(*rhs))
            }
            _ => None,
        }
    }

    /// The receiver of the call `expr`, a method call or a call of a callable
    /// value, if the call takes it `mut`.
    fn mut_receiver(&self, expr: ExprId) -> Option<ExprId> {
        let receiver = self.typed_body.call_receiver(self.db, expr)?;
        let callable = self.typed_body.callable_expr(expr)?;
        (callable
            .callable_def()
            .param_mode(self.db, 0, callable.mode())
            == FuncParamMode::Mut)
            .then_some(receiver)
    }
    fn report_deref(&mut self, expr: ExprId) {
        self.push_unsafe(BodyDiag::UnsafeDerefRequiresUnsafe {
            primary: expr.span(self.body).into(),
        });
    }

    /// Reports a call, method call or operator whose callee is an
    /// `unsafe fn` or a foreign function, and a `with` provider through an
    /// `unsafe` field that one of its `mut` effects writes. A trait method
    /// call resolves to the trait's declaration, whose unsafety every
    /// implementation shares. A runtime function of an `extern` block
    /// outside core and std is foreign: nothing checks what it does.
    fn check_call(&mut self, expr: ExprId) {
        if let Some(callable) = self.typed_body.callable_expr(expr)
            && let callee @ CallableDef::Func(func) = callable.callable_def()
            && (func.is_unsafe(self.db)
                || func.is_extern(self.db)
                    && !func.is_const(self.db)
                    && !matches!(
                        func.top_mod(self.db).ingot(self.db).kind(self.db),
                        IngotKind::Core | IngotKind::Std
                    ))
        {
            self.push_unsafe(BodyDiag::UnsafeCallRequiresUnsafe {
                primary: expr.span(self.body).into(),
                callee,
            });
        }
        let typed_body = self.typed_body;
        for arg in typed_body.call_effect_args(expr).into_iter().flatten() {
            if arg.required_mut
                && let Some(source) = arg.with_source
            {
                self.check_place_write(source);
            }
        }
    }

    fn check_stmt(&mut self, stmt: StmtId) {
        let Partial::Present(stmt_data) = stmt.data(self.db, self.body) else {
            return;
        };

        match stmt_data {
            Stmt::Let(pat, _, init, else_) => {
                self.check_pat(*pat);
                for expr in init.iter().chain(else_) {
                    self.check_expr(*expr);
                }
            }
            Stmt::For(pat, iter, body, _) => {
                self.check_pat(*pat);
                self.check_expr(*iter);
                self.check_expr(*body);
            }
            Stmt::While(cond, body) => {
                self.check_cond(*cond);
                self.check_expr(*body);
            }
            Stmt::Continue | Stmt::Break => {}
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.check_expr(*expr);
                }
            }
            Stmt::Yield(expr) | Stmt::Expr(expr) => self.check_expr(*expr),
        }
    }

    fn check_expr(&mut self, expr: ExprId) {
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            return;
        };
        // A place opened `mut` by its pattern or loop writes through it.
        if let Some(Shape::Access(BorrowKind::Mut, _)) =
            self.typed_body.expr_prop(self.db, expr).shape
        {
            self.check_place_write(expr);
        }

        match expr_data {
            Expr::Lit(_) | Expr::Path(_) | Expr::UnsupportedMacroCall => {}
            Expr::Block(stmts, BlockKind::Unsafe) => {
                let outer = std::mem::replace(&mut self.in_unsafe, true);
                stmts.iter().for_each(|stmt| self.check_stmt(*stmt));
                self.in_unsafe = outer;
            }
            Expr::Block(stmts, BlockKind::Normal) => {
                stmts.iter().for_each(|stmt| self.check_stmt(*stmt));
            }
            Expr::Closure { body, .. } => self.check_expr(*body),

            Expr::Un(inner, op) => {
                if *op == UnOp::Mut
                    || *op == UnOp::MaybeMut && self.owner.mode() == Some(BorrowKind::Mut)
                {
                    self.check_place_write(*inner);
                }
                self.check_expr(*inner);
                if *op == UnOp::Deref {
                    let ty = self.typed_body.expr_ty(self.db, *inner);
                    if ty
                        .as_capability(self.db)
                        .map_or(ty, |(_, inner)| inner)
                        .as_ptr(self.db)
                        .is_some()
                    {
                        self.report_deref(expr);
                    }
                    if self.is_state_pointer(ty) {
                        self.check_state_access(expr, false);
                    }
                }
                self.check_call(expr);
            }

            Expr::Field(inner, field) => {
                self.check_expr(*inner);
                let inner_ty = self.typed_body.expr_ty(self.db, *inner);
                if let Some(field) = field.to_opt()
                    && resolve_place_field(self.db, inner_ty, field)
                        .is_some_and(|resolved| resolved.implicit_deref_ty.is_some())
                {
                    self.report_deref(expr);
                    if self.is_state_pointer(inner_ty) {
                        self.check_state_access(expr, false);
                    }
                }
            }

            Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
                self.check_place_write(*lhs);
                self.check_expr(*lhs);
                self.check_expr(*rhs);
                self.check_call(expr);
            }
            Expr::Bin(lhs, rhs, _) => {
                self.check_expr(*lhs);
                self.check_expr(*rhs);
                self.check_call(expr);
            }

            Expr::Cast(inner, _) => {
                self.check_cast_init(expr, *inner);
                self.check_expr(*inner)
            }

            Expr::ArrayRep(inner, _) | Expr::Try(inner) => self.check_expr(*inner),

            Expr::Call(callee, args) => {
                if let Some(receiver) = self.mut_receiver(expr) {
                    self.check_place_write(receiver);
                }
                self.check_expr(*callee);
                args.iter().for_each(|arg| self.check_expr(arg.expr));
                self.check_call(expr);
            }
            Expr::MethodCall(receiver, _, _, args) => {
                if let Some(receiver) = self.mut_receiver(expr) {
                    self.check_place_write(receiver);
                }
                self.check_expr(*receiver);
                args.iter().for_each(|arg| self.check_expr(arg.expr));
                self.check_call(expr);
            }
            Expr::Assert(args) => args.iter().for_each(|arg| self.check_expr(arg.expr)),

            Expr::If(cond, then, else_) => {
                self.check_cond(*cond);
                self.check_expr(*then);
                if let Some(else_) = else_ {
                    self.check_expr(*else_);
                }
            }
            Expr::Match(scrutinee, arms) => {
                self.check_expr(*scrutinee);
                if let Partial::Present(arms) = arms {
                    for arm in arms {
                        self.check_pat(arm.pat);
                        self.check_expr(arm.body);
                    }
                }
            }
            Expr::With(bindings, body) => {
                for binding in bindings {
                    self.check_expr(binding.value);
                }
                self.check_expr(*body);
            }
            Expr::RecordInit(_, fields) => {
                self.check_record_init(expr, fields);
                fields.iter().for_each(|field| self.check_expr(field.expr));
            }
            Expr::Tuple(elems) | Expr::Array(elems) => {
                elems.iter().for_each(|elem| self.check_expr(*elem));
            }
        }
    }

    fn check_cond(&mut self, cond: CondId) {
        let Partial::Present(cond_data) = cond.data(self.db, self.body) else {
            return;
        };

        match cond_data {
            Cond::Expr(expr) => self.check_expr(*expr),
            Cond::Let(pat, expr) => {
                self.check_pat(*pat);
                self.check_expr(*expr);
            }
            Cond::Bin(lhs, rhs, _) => {
                self.check_cond(*lhs);
                self.check_cond(*rhs);
            }
        }
    }
}
