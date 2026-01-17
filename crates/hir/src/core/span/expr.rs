use parser::ast;
use salsa::Update;

use super::{
    body_source_map, define_lazy_span_node,
    transition::{ChainInitiator, ResolvedOrigin, SpanTransitionChain},
};
use crate::{
    SpannedHirDb,
    hir_def::{Body, ExprId},
    span::{
        LazyLitSpan, LazySpanAtom, params::LazyGenericArgListSpan, path::LazyPathSpan,
        types::LazyTySpan,
    },
};

define_lazy_span_node!(LazyExprSpan, ast::Expr,);
impl<'db> LazyExprSpan<'db> {
    pub fn new(body: Body<'db>, expr: ExprId) -> Self {
        let root = ExprRoot { expr, body };
        Self(SpanTransitionChain::new(root))
    }

    pub fn into_lit_expr(self) -> LazyLitExprSpan<'db> {
        LazyLitExprSpan(self.0)
    }

    pub fn into_bin_expr(self) -> LazyBinExprSpan<'db> {
        LazyBinExprSpan(self.0)
    }

    pub fn into_un_expr(self) -> LazyUnExprSpan<'db> {
        LazyUnExprSpan(self.0)
    }

    pub fn into_cast_expr(self) -> LazyCastExprSpan<'db> {
        LazyCastExprSpan(self.0)
    }

    pub fn into_call_expr(self) -> LazyCallExprSpan<'db> {
        LazyCallExprSpan(self.0)
    }

    pub fn into_method_call_expr(self) -> LazyMethodCallExprSpan<'db> {
        LazyMethodCallExprSpan(self.0)
    }

    pub fn into_path_expr(self) -> LazyPathExprSpan<'db> {
        LazyPathExprSpan(self.0)
    }

    pub fn into_record_init_expr(self) -> LazyRecordInitExprSpan<'db> {
        LazyRecordInitExprSpan(self.0)
    }

    pub fn into_field_expr(self) -> LazyFieldExprSpan<'db> {
        LazyFieldExprSpan(self.0)
    }

    pub fn into_match_expr(self) -> LazyMatchExprSpan<'db> {
        LazyMatchExprSpan(self.0)
    }

    pub fn into_aug_assign_expr(self) -> LazyAugAssignExprSpan<'db> {
        LazyAugAssignExprSpan(self.0)
    }

    pub fn into_assign_expr(self) -> LazyAssignExprSpan<'db> {
        LazyAssignExprSpan(self.0)
    }
}

define_lazy_span_node! {
    LazyLitExprSpan,
    ast::LitExpr,
    @node {
        (lit, lit, LazyLitSpan),
    }
}

define_lazy_span_node!(
    LazyBinExprSpan,
    ast::BinExpr,
    @node {
        (op, op, LazySpanAtom),
    }
);

define_lazy_span_node!(
    LazyAssignExprSpan,
    ast::AssignExpr,
    @token {
        (eq, eq),
    }
);

define_lazy_span_node!(
    LazyAugAssignExprSpan,
    ast::AugAssignExpr,
    @node {
        (op, op, LazySpanAtom),
    }
);

define_lazy_span_node!(
    LazyUnExprSpan,
    ast::UnExpr,
    @node {
        (op, op, LazySpanAtom),
    }
);

define_lazy_span_node!(
    LazyCastExprSpan,
    ast::CastExpr,
    @token {
        (as_kw, as_kw),
    }
    @node {
        (ty, ty, LazyTySpan),
    }
);

define_lazy_span_node!(
    LazyCallExprSpan,
    ast::CallExpr,
    @node {
        (callee, callee, LazyExprSpan),
        (args, args, LazyCallArgListSpan),
    }
);

define_lazy_span_node!(
    LazyMethodCallExprSpan,
    ast::MethodCallExpr,
    @token {
        (method_name, method_name),
    }
    @node {
        (receiver, receiver, LazyExprSpan),
        (generic_args, generic_args, LazyGenericArgListSpan),
        (args, args, LazyCallArgListSpan),
    }
);

define_lazy_span_node! {
    LazyPathExprSpan,
    ast::PathExpr,
    @node {
        (path, path, LazyPathSpan),
    }
}

define_lazy_span_node!(
    LazyRecordInitExprSpan,
    ast::RecordInitExpr,
    @node {
        (path, path, LazyPathSpan),
        (fields, fields, LazyFieldListSpan),
    }
);

define_lazy_span_node!(
    LazyFieldExprSpan,
    ast::FieldExpr,
    @token {
        (accessor, name_or_index),
    }
);

define_lazy_span_node!(
    LazyMatchExprSpan,
    ast::MatchExpr,
    @node {
        (arms, arms, LazyMatchArmListSpan),
    }
);

define_lazy_span_node!(
    LazyCallArgListSpan,
    ast::CallArgList,
    @idx {
        (arg, LazyCallArgSpan),
    }
);

define_lazy_span_node!(
    LazyCallArgSpan,
    ast::CallArg,
    @token {
        (label, label),
    }
    @node {
        (expr, expr, LazyExprSpan),
    }
);

define_lazy_span_node!(
    LazyFieldListSpan,
    ast::FieldList,
    @idx {
        (field, LazyFieldSpan),
    }
);

define_lazy_span_node!(
    LazyFieldSpan,
    ast::RecordField,
    @token {
        (label, label),
    }
);

define_lazy_span_node!(
    LazyMatchArmListSpan,
    ast::MatchArmList,
    @idx {
        (arm, LazyMatchArmSpan),
    }
);

define_lazy_span_node!(LazyMatchArmSpan);

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, Update)]
pub(crate) struct ExprRoot<'db> {
    expr: ExprId,
    pub(crate) body: Body<'db>,
}

impl ChainInitiator for ExprRoot<'_> {
    fn init(&self, db: &dyn SpannedHirDb) -> ResolvedOrigin {
        let source_map = body_source_map(db, self.body);
        let origin = source_map.expr_map.node_to_source(self.expr);
        let top_mod = self.body.top_mod(db);
        ResolvedOrigin::resolve(db, top_mod, origin)
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        hir_def::{ArithBinOp, Body, Expr},
        test_db::TestDb,
    };

    #[test]
    fn aug_assign() {
        let mut db = TestDb::default();

        let text = r#"
            fn foo(mut x: i32) {
                x += 1
            }
        }"#;

        let file = db.standalone_file(text);
        let body: Body = db.expect_item::<Body>(file);
        let bin_expr = match body.exprs(&db).values().nth(2).cloned().unwrap().unwrap() {
            Expr::AugAssign(lhs, rhs, bin_op) => (lhs, rhs, bin_op),
            _ => unreachable!(),
        };
        let top_mod = body.top_mod(&db);
        assert_eq!("x", db.text_at(top_mod, &bin_expr.0.span(body)));
        assert_eq!("1", db.text_at(top_mod, &bin_expr.1.span(body)));
        assert_eq!(ArithBinOp::Add, bin_expr.2);
    }
}
