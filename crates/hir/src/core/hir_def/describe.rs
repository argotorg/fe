use common::ir_describe::{DescribeCtx, Dim, IrConsumer, IrDescribe};

use super::{
    Body, Partial,
    expr::{Expr, ExprId},
    item::{
        Contract, Enum, Func, Impl, ImplTrait, Struct, TopLevelMod, Trait, TypeAlias, Const,
        ItemKind,
    },
    stmt::Stmt,
};
use crate::HirDb;

impl<'db> IrDescribe for TopLevelMod<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("TopLevelMod");
        c.field_str(Dim::Names, self.name(db).data(db));
        for item in self.all_items(db) {
            describe_item(cx, c, &item);
        }
        c.exit_node();
    }
}

fn describe_item<'db, C: IrConsumer>(cx: &DescribeCtx<'_>, c: &mut C, item: &ItemKind<'db>) {
    let db: &dyn HirDb = cx.db();
    match item {
        ItemKind::Func(f) => f.describe(cx, c),
        ItemKind::Struct(s) => s.describe(cx, c),
        ItemKind::Contract(ct) => ct.describe(cx, c),
        ItemKind::Enum(e) => e.describe(cx, c),
        ItemKind::TypeAlias(ta) => ta.describe(cx, c),
        ItemKind::Impl(i) => i.describe(cx, c),
        ItemKind::Trait(t) => t.describe(cx, c),
        ItemKind::ImplTrait(it) => it.describe(cx, c),
        ItemKind::Const(co) => co.describe(cx, c),
        ItemKind::Mod(m) => {
            c.enter_node("Mod");
            if let Partial::Present(name) = &m.name(db) {
                c.field_str(Dim::Names, name.data(db));
            }
            c.exit_node();
        }
        _ => {
            c.enter_node("OtherItem");
            c.exit_node();
        }
    }
}

impl<'db> IrDescribe for Func<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("Func");
        if let Partial::Present(name) = &self.name(db) {
            c.field_str(Dim::Names, name.data(db));
        }
        if let Some(body) = self.body(db) {
            c.child(cx, &BodyDescriptor(body));
        }
        c.exit_node();
    }
}

impl<'db> IrDescribe for Struct<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("Struct");
        if let Partial::Present(name) = &self.name(db) {
            c.field_str(Dim::Names, name.data(db));
        }
        c.field_u64(Dim::Structure, self.vis(db) as u64);
        let fields = self.fields(db).data(db);
        c.field_u64(Dim::Structure, fields.len() as u64);
        for field in fields {
            if let Partial::Present(name) = &field.name {
                c.field_str(Dim::Names, name.data(db));
            }
            c.field_u64(Dim::Structure, field.vis as u64);
        }
        c.exit_node();
    }
}

impl<'db> IrDescribe for Contract<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("Contract");
        if let Partial::Present(name) = &self.name(db) {
            c.field_str(Dim::Names, name.data(db));
        }
        let fields = self.hir_fields(db).data(db);
        c.field_u64(Dim::Structure, fields.len() as u64);
        for field in fields {
            if let Partial::Present(name) = &field.name {
                c.field_str(Dim::Names, name.data(db));
            }
        }
        if self.init(db).is_some() {
            c.field_bool(Dim::Structure, true);
        }
        c.field_u64(Dim::Structure, self.recvs(db).data(db).len() as u64);
        c.exit_node();
    }
}

impl<'db> IrDescribe for Enum<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("Enum");
        if let Partial::Present(name) = &self.name(db) {
            c.field_str(Dim::Names, name.data(db));
        }
        let variants = self.variants_list(db).data(db);
        c.field_u64(Dim::Structure, variants.len() as u64);
        for variant in variants {
            if let Partial::Present(name) = &variant.name {
                c.field_str(Dim::Names, name.data(db));
            }
        }
        c.exit_node();
    }
}

impl<'db> IrDescribe for Trait<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("Trait");
        if let Partial::Present(name) = &self.name(db) {
            c.field_str(Dim::Names, name.data(db));
        }
        c.field_u64(Dim::Structure, self.generic_params(db).data(db).len() as u64);
        c.exit_node();
    }
}

impl<'db> IrDescribe for Impl<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("Impl");
        c.field_u64(Dim::Structure, self.generic_params(db).data(db).len() as u64);
        c.exit_node();
    }
}

impl<'db> IrDescribe for ImplTrait<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("ImplTrait");
        c.field_u64(Dim::Structure, self.generic_params(db).data(db).len() as u64);
        c.exit_node();
    }
}

impl<'db> IrDescribe for TypeAlias<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("TypeAlias");
        if let Partial::Present(name) = &self.name(db) {
            c.field_str(Dim::Names, name.data(db));
        }
        c.exit_node();
    }
}

impl<'db> IrDescribe for Const<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("Const");
        if let Partial::Present(name) = &self.name(db) {
            c.field_str(Dim::Names, name.data(db));
        }
        c.exit_node();
    }
}

struct BodyDescriptor<'db>(Body<'db>);

impl<'db> IrDescribe for BodyDescriptor<'db> {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        let db: &dyn HirDb = cx.db();
        c.enter_node("Body");

        // Walk the root expression (body.expr is the top-level block)
        let root_expr = self.0.expr(db);
        let root_data = &self.0.exprs(db)[root_expr];
        if let Partial::Present(expr) = root_data {
            describe_expr(cx, c, db, self.0, expr);
        }

        c.exit_node();
    }
}

fn describe_expr<'db, C: IrConsumer>(
    cx: &DescribeCtx<'_>, c: &mut C, db: &'db dyn HirDb,
    body: Body<'db>, expr: &Expr<'db>,
) {
    use super::expr::*;
    match expr {
        Expr::Lit(lit) => {
            c.enter_node("Lit");
            match lit {
                super::LitKind::Bool(b) => c.field_bool(Dim::Structure, *b),
                super::LitKind::Int(_) => c.field_u64(Dim::Structure, 0),
                super::LitKind::String(_) => c.field_u64(Dim::Structure, 1),
            }
            c.exit_node();
        }
        Expr::Block(stmts) => {
            c.enter_node("Block");
            c.field_u64(Dim::Structure, stmts.len() as u64);
            for &stmt_id in stmts {
                if let Partial::Present(stmt) = &body.stmts(db)[stmt_id] {
                    describe_stmt(cx, c, db, body, stmt);
                }
            }
            c.exit_node();
        }
        Expr::Bin(lhs, rhs, op) => {
            c.enter_node("Bin");
            c.field_u64(Dim::Structure, binop_tag_hir(op));
            describe_expr_id(cx, c, db, body, *lhs);
            describe_expr_id(cx, c, db, body, *rhs);
            c.exit_node();
        }
        Expr::Un(expr, op) => {
            c.enter_node("Un");
            c.field_u64(Dim::Structure, *op as u64);
            describe_expr_id(cx, c, db, body, *expr);
            c.exit_node();
        }
        Expr::Call(callee, args) => {
            c.enter_node("Call");
            describe_expr_id(cx, c, db, body, *callee);
            c.field_u64(Dim::Structure, args.len() as u64);
            for arg in args {
                if let Some(label) = arg.label {
                    c.field_str(Dim::Names, label.data(db));
                }
                describe_expr_id(cx, c, db, body, arg.expr);
            }
            c.exit_node();
        }
        Expr::Path(path) => {
            c.enter_node("Path");
            if let Partial::Present(path_id) = path {
                describe_path(cx, c, db, *path_id);
            }
            c.exit_node();
        }
        Expr::If(cond, then_expr, else_expr) => {
            c.enter_node("If");
            describe_cond(cx, c, db, body, *cond);
            describe_expr_id(cx, c, db, body, *then_expr);
            if let Some(else_id) = else_expr {
                describe_expr_id(cx, c, db, body, *else_id);
            }
            c.exit_node();
        }
        Expr::Match(scrutinee, arms) => {
            c.enter_node("Match");
            describe_expr_id(cx, c, db, body, *scrutinee);
            if let Partial::Present(arms) = arms {
                c.field_u64(Dim::Structure, arms.len() as u64);
                for arm in arms {
                    describe_pat(cx, c, db, body, arm.pat);
                    describe_expr_id(cx, c, db, body, arm.body);
                }
            }
            c.exit_node();
        }
        Expr::Assign(dst, src) => {
            c.enter_node("Assign");
            describe_expr_id(cx, c, db, body, *dst);
            describe_expr_id(cx, c, db, body, *src);
            c.exit_node();
        }
        Expr::AugAssign(dst, src, op) => {
            c.enter_node("AugAssign");
            c.field_u64(Dim::Structure, *op as u64);
            describe_expr_id(cx, c, db, body, *dst);
            describe_expr_id(cx, c, db, body, *src);
            c.exit_node();
        }
        Expr::MethodCall(recv, name, _generics, args) => {
            c.enter_node("MethodCall");
            describe_expr_id(cx, c, db, body, *recv);
            if let Partial::Present(n) = name {
                c.field_str(Dim::Names, n.data(db));
            }
            c.field_u64(Dim::Structure, args.len() as u64);
            for arg in args {
                if let Some(label) = arg.label {
                    c.field_str(Dim::Names, label.data(db));
                }
                describe_expr_id(cx, c, db, body, arg.expr);
            }
            c.exit_node();
        }
        Expr::RecordInit(path, fields) => {
            c.enter_node("RecordInit");
            if let Partial::Present(path_id) = path {
                describe_path(cx, c, db, *path_id);
            }
            c.field_u64(Dim::Structure, fields.len() as u64);
            for field in fields {
                if let Some(label) = field.label {
                    c.field_str(Dim::Names, label.data(db));
                }
                describe_expr_id(cx, c, db, body, field.expr);
            }
            c.exit_node();
        }
        Expr::Field(expr, idx) => {
            c.enter_node("Field");
            describe_expr_id(cx, c, db, body, *expr);
            match idx {
                Partial::Present(super::expr::FieldIndex::Ident(ident)) => {
                    c.field_str(Dim::Names, ident.data(db));
                }
                Partial::Present(super::expr::FieldIndex::Index(int_id)) => {
                    let idx_str = int_id.data(db).to_string();
                    c.field_str(Dim::Structure, &idx_str);
                }
                Partial::Absent => {}
            }
            c.exit_node();
        }
        Expr::Tuple(exprs) => {
            c.enter_node("Tuple");
            c.field_u64(Dim::Structure, exprs.len() as u64);
            for &e in exprs { describe_expr_id(cx, c, db, body, e); }
            c.exit_node();
        }
        Expr::Array(exprs) => {
            c.enter_node("Array");
            c.field_u64(Dim::Structure, exprs.len() as u64);
            for &e in exprs { describe_expr_id(cx, c, db, body, e); }
            c.exit_node();
        }
        Expr::ArrayRep(expr, size) => {
            c.enter_node("ArrayRep");
            describe_expr_id(cx, c, db, body, *expr);
            if let Partial::Present(size_body) = size {
                c.child(cx, &BodyDescriptor(*size_body));
            }
            c.exit_node();
        }
        Expr::Cast(expr, ty) => {
            c.enter_node("Cast");
            describe_expr_id(cx, c, db, body, *expr);
            if let Partial::Present(type_id) = ty {
                describe_type(cx, c, db, *type_id);
            }
            c.exit_node();
        }
        Expr::With(bindings, body_expr) => {
            c.enter_node("With");
            c.field_u64(Dim::Structure, bindings.len() as u64);
            for binding in bindings {
                if let Some(Partial::Present(key_path)) = &binding.key_path {
                    describe_path(cx, c, db, *key_path);
                }
                describe_expr_id(cx, c, db, body, binding.value);
            }
            describe_expr_id(cx, c, db, body, *body_expr);
            c.exit_node();
        }
    }
}

fn describe_expr_id<'db, C: IrConsumer>(
    cx: &DescribeCtx<'_>, c: &mut C, db: &'db dyn HirDb,
    body: Body<'db>, expr_id: ExprId,
) {
    if let Partial::Present(expr) = &body.exprs(db)[expr_id] {
        describe_expr(cx, c, db, body, expr);
    }
}

fn describe_stmt<'db, C: IrConsumer>(
    cx: &DescribeCtx<'_>, c: &mut C, db: &'db dyn HirDb,
    body: Body<'db>, stmt: &Stmt<'db>,
) {
    use super::stmt::*;
    match stmt {
        Stmt::Let(pat, ty, init) => {
            c.enter_node("Let");
            describe_pat(cx, c, db, body, *pat);
            if let Some(type_id) = ty {
                describe_type(cx, c, db, *type_id);
            }
            if let Some(init) = init {
                describe_expr_id(cx, c, db, body, *init);
            }
            c.exit_node();
        }
        Stmt::For(pat, iter, body_expr, unroll) => {
            c.enter_node("For");
            describe_pat(cx, c, db, body, *pat);
            if let Some(u) = unroll {
                c.field_bool(Dim::Structure, *u);
            }
            describe_expr_id(cx, c, db, body, *iter);
            describe_expr_id(cx, c, db, body, *body_expr);
            c.exit_node();
        }
        Stmt::While(cond, body_expr) => {
            c.enter_node("While");
            describe_cond(cx, c, db, body, *cond);
            describe_expr_id(cx, c, db, body, *body_expr);
            c.exit_node();
        }
        Stmt::Continue => { c.enter_node("Continue"); c.exit_node(); }
        Stmt::Break => { c.enter_node("Break"); c.exit_node(); }
        Stmt::Return(expr) => {
            c.enter_node("Return");
            if let Some(e) = expr {
                describe_expr_id(cx, c, db, body, *e);
            }
            c.exit_node();
        }
        Stmt::Expr(expr) => {
            c.enter_node("ExprStmt");
            describe_expr_id(cx, c, db, body, *expr);
            c.exit_node();
        }
    }
}

fn describe_pat<'db, C: IrConsumer>(
    cx: &DescribeCtx<'_>, c: &mut C, db: &'db dyn HirDb,
    body: Body<'db>, pat_id: super::PatId,
) {
    use super::pat::Pat;
    let Partial::Present(pat) = &body.pats(db)[pat_id] else { return };
    match pat {
        Pat::WildCard => {
            c.enter_node("WildCard");
            c.exit_node();
        }
        Pat::Rest => {
            c.enter_node("Rest");
            c.exit_node();
        }
        Pat::Lit(lit) => {
            c.enter_node("PatLit");
            if let Partial::Present(lit) = lit {
                match lit {
                    super::LitKind::Bool(b) => c.field_bool(Dim::Structure, *b),
                    super::LitKind::Int(_) => c.field_u64(Dim::Structure, 0),
                    super::LitKind::String(_) => c.field_u64(Dim::Structure, 1),
                }
            }
            c.exit_node();
        }
        Pat::Tuple(pats) => {
            c.enter_node("PatTuple");
            c.field_u64(Dim::Structure, pats.len() as u64);
            for &p in pats { describe_pat(cx, c, db, body, p); }
            c.exit_node();
        }
        Pat::Path(path, is_mut) => {
            c.enter_node("PatPath");
            c.field_bool(Dim::Structure, *is_mut);
            if let Partial::Present(path_id) = path {
                describe_path(cx, c, db, *path_id);
            }
            c.exit_node();
        }
        Pat::PathTuple(path, pats) => {
            c.enter_node("PatPathTuple");
            if let Partial::Present(path_id) = path {
                describe_path(cx, c, db, *path_id);
            }
            for &p in pats { describe_pat(cx, c, db, body, p); }
            c.exit_node();
        }
        Pat::Record(path, fields) => {
            c.enter_node("PatRecord");
            if let Partial::Present(path_id) = path {
                describe_path(cx, c, db, *path_id);
            }
            for field in fields {
                if let Partial::Present(label) = &field.label {
                    c.field_str(Dim::Names, label.data(db));
                }
                describe_pat(cx, c, db, body, field.pat);
            }
            c.exit_node();
        }
        Pat::Or(l, r) => {
            c.enter_node("PatOr");
            describe_pat(cx, c, db, body, *l);
            describe_pat(cx, c, db, body, *r);
            c.exit_node();
        }
    }
}

fn describe_cond<'db, C: IrConsumer>(
    cx: &DescribeCtx<'_>, c: &mut C, db: &'db dyn HirDb,
    body: Body<'db>, cond_id: super::expr::CondId,
) {
    let Partial::Present(cond) = &body.conds(db)[cond_id] else { return };
    match cond {
        super::expr::Cond::Expr(e) => describe_expr_id(cx, c, db, body, *e),
        super::expr::Cond::Let(pat, e) => {
            c.field_u64(Dim::Structure, 1);
            describe_pat(cx, c, db, body, *pat);
            describe_expr_id(cx, c, db, body, *e);
        }
        super::expr::Cond::Bin(l, r, op) => {
            c.field_u64(Dim::Structure, 2);
            c.field_u64(Dim::Structure, *op as u64);
            describe_cond(cx, c, db, body, *l);
            describe_cond(cx, c, db, body, *r);
        }
    }
}

fn describe_path<'db, C: IrConsumer>(
    cx: &DescribeCtx<'_>, c: &mut C, db: &'db dyn HirDb,
    path: super::PathId<'db>,
) {
    use super::path::PathKind;
    if let Some(parent) = path.parent(db) {
        describe_path(cx, c, db, parent);
    }
    match path.kind(db) {
        PathKind::Ident { ident, .. } => {
            if let Partial::Present(id) = ident {
                c.field_str(Dim::Names, id.data(db));
            }
        }
        PathKind::QualifiedType { type_, .. } => {
            describe_type(cx, c, db, type_);
        }
    }
}

fn describe_type<'db, C: IrConsumer>(
    cx: &DescribeCtx<'_>, c: &mut C, db: &'db dyn HirDb,
    type_id: super::TypeId<'db>,
) {
    use super::types::TypeKind;
    c.enter_node("Type");
    match type_id.data(db) {
        TypeKind::Path(path) => {
            if let Partial::Present(path_id) = path {
                describe_path(cx, c, db, *path_id);
            }
        }
        TypeKind::Ptr(inner) => {
            c.field_str(Dim::Structure, "ptr");
            if let Partial::Present(inner_ty) = inner {
                describe_type(cx, c, db, *inner_ty);
            }
        }
        TypeKind::Mode(mode, inner) => {
            c.field_str(Dim::Structure, mode.keyword());
            if let Partial::Present(inner_ty) = inner {
                describe_type(cx, c, db, *inner_ty);
            }
        }
        TypeKind::Tuple(tup) => {
            c.field_str(Dim::Structure, "tuple");
            c.field_u64(Dim::Structure, tup.data(db).len() as u64);
        }
        TypeKind::Array(elem, _len) => {
            c.field_str(Dim::Structure, "array");
            if let Partial::Present(elem_ty) = elem {
                describe_type(cx, c, db, *elem_ty);
            }
        }
        TypeKind::Never => {
            c.field_str(Dim::Structure, "never");
        }
    }
    c.exit_node();
}

fn binop_tag_hir(op: &super::expr::BinOp) -> u64 {
    use super::expr::BinOp;
    match op {
        BinOp::Arith(a) => *a as u64,
        BinOp::Comp(c) => 100 + *c as u64,
        BinOp::Logical(l) => 200 + *l as u64,
        BinOp::Index => 300,
    }
}
