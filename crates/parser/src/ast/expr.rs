use rowan::ast::{AstNode, support};

use super::{GenericArgsOwner, LitInt, ast_node};
use crate::{SyntaxKind as SK, SyntaxNode, SyntaxToken};

ast_node! {
    /// An expression.
    /// Use [`Self::kind`] to determine the type of expression.
    pub struct Expr,
    SK::BlockExpr
    | SK::BinExpr
    | SK::UnExpr
    | SK::CastExpr
    | SK::CallExpr
    | SK::MethodCallExpr
    | SK::PathExpr
    | SK::RecordInitExpr
    | SK::FieldExpr
    | SK::IndexExpr
    | SK::TupleExpr
    | SK::ArrayExpr
    | SK::ArrayRepExpr
    | SK::LitExpr
    | SK::IfExpr
    | SK::MatchExpr
    | SK::WithExpr
    | SK::ParenExpr
    | SK::AssignExpr
    | SK::AugAssignExpr,
}

impl Expr {
    /// Returns the kind of expression.
    pub fn kind(&self) -> ExprKind {
        match self.syntax().kind() {
            SK::BlockExpr => ExprKind::Block(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::BinExpr => ExprKind::Bin(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::UnExpr => ExprKind::Un(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::CastExpr => ExprKind::Cast(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::CallExpr => ExprKind::Call(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::MethodCallExpr => {
                ExprKind::MethodCall(AstNode::cast(self.syntax().clone()).unwrap())
            }
            SK::PathExpr => ExprKind::Path(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::RecordInitExpr => {
                ExprKind::RecordInit(AstNode::cast(self.syntax().clone()).unwrap())
            }
            SK::FieldExpr => ExprKind::Field(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::IndexExpr => ExprKind::Index(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::TupleExpr => ExprKind::Tuple(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::ArrayExpr => ExprKind::Array(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::ArrayRepExpr => ExprKind::ArrayRep(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::LitExpr => ExprKind::Lit(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::IfExpr => ExprKind::If(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::MatchExpr => ExprKind::Match(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::WithExpr => ExprKind::With(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::ParenExpr => ExprKind::Paren(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::AssignExpr => ExprKind::Assign(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::AugAssignExpr => ExprKind::AugAssign(AstNode::cast(self.syntax().clone()).unwrap()),
            _ => unreachable!(),
        }
    }
}

ast_node! {
    /// `{ stmt1\n stmt2\n ..}`
    pub struct BlockExpr,
    SK::BlockExpr,
    IntoIterator<Item=super::Stmt>,
}
impl BlockExpr {
    /// Returns the statements in the block.
    pub fn stmts(&self) -> impl Iterator<Item = super::Stmt> {
        self.iter()
    }

    /// Returns items declared in the block.
    pub fn items(&self) -> impl Iterator<Item = super::Item> {
        support::children(self.syntax())
    }
}

ast_node! {
    /// `lhs op rhs`
    pub struct BinExpr,
    SK::BinExpr
}
impl BinExpr {
    /// Returns the left-hand side of the binary operation.
    pub fn lhs(&self) -> Option<Expr> {
        support::children(self.syntax()).next()
    }

    /// Returns the right-hand side of the binary operation.
    pub fn rhs(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(1)
    }

    /// Returns the operator of the binary operation.
    pub fn op(&self) -> Option<BinOp> {
        self.syntax()
            .children_with_tokens()
            .find_map(BinOp::from_node_or_token)
    }
}

ast_node! {
    /// `op expr`
    pub struct UnExpr,
    SK::UnExpr
}
impl UnExpr {
    /// Returns the operand of the unary operation.
    pub fn expr(&self) -> Option<Expr> {
        support::children(self.syntax()).next()
    }

    /// Returns the operator of the unary operation.
    pub fn op(&self) -> Option<UnOp> {
        self.syntax().children_with_tokens().find_map(|c| match c {
            rowan::NodeOrToken::Token(token) => UnOp::from_token(token),
            rowan::NodeOrToken::Node(_) => None,
        })
    }
}

ast_node! {
    /// `expr as Type`
    pub struct CastExpr,
    SK::CastExpr
}
impl CastExpr {
    /// Returns the cast operand.
    pub fn expr(&self) -> Option<Expr> {
        support::child(self.syntax())
    }

    /// Returns the cast target type.
    pub fn ty(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    /// Returns the `as` token.
    pub fn as_kw(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::AsKw)
    }
}

ast_node! {
    /// `func<Arg, ..>(arg1, arg2, ..)`
    pub struct CallExpr,
    SK::CallExpr,
}
impl CallExpr {
    /// Returns the callee of the call expression.
    pub fn callee(&self) -> Option<Expr> {
        support::child(self.syntax())
    }

    /// Returns the arguments of the call expression.
    pub fn args(&self) -> Option<super::CallArgList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `obj.method<Arg, ...>(arg1, arg2, ..)`
    pub struct MethodCallExpr,
    SK::MethodCallExpr
}
impl GenericArgsOwner for MethodCallExpr {}
impl MethodCallExpr {
    /// Returns the receiver of the method call expression.
    pub fn receiver(&self) -> Option<Expr> {
        support::child(self.syntax())
    }

    /// Returns the name of the method being called.
    pub fn method_name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the arguments of the method call expression.
    pub fn args(&self) -> Option<super::CallArgList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `path`
    pub struct PathExpr,
    SK::PathExpr
}
impl PathExpr {
    /// Returns the path of the path expression.
    pub fn path(&self) -> Option<super::Path> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `path { field1: expr1, field2: expr2, .. }`
    pub struct RecordInitExpr,
    SK::RecordInitExpr
}
impl RecordInitExpr {
    /// Returns the path of the record init expression.
    pub fn path(&self) -> Option<super::Path> {
        support::child(self.syntax())
    }

    /// Returns the fields of the record init expression.
    pub fn fields(&self) -> Option<FieldList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `expr.field` or `expr.0`
    pub struct FieldExpr,
    SK::FieldExpr
}
impl FieldExpr {
    /// Returns the expression being accessed.
    pub fn receiver(&self) -> Option<Expr> {
        support::child(self.syntax())
    }

    /// Returns the name of the field.
    pub fn field_name(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the index number of the field.
    pub fn field_index(&self) -> Option<LitInt> {
        support::token(self.syntax(), SK::Int).map(|it| LitInt { token: it })
    }

    pub fn name_or_index(&self) -> Option<SyntaxToken> {
        self.field_name()
            .or_else(|| self.field_index().map(|i| i.token().clone()))
    }
}

ast_node! {
    /// `expr[index]`
    pub struct IndexExpr,
    SK::IndexExpr
}
impl IndexExpr {
    /// Returns the expression being indexed.
    pub fn expr(&self) -> Option<Expr> {
        support::child(self.syntax())
    }

    /// Returns the index of the index expression.
    pub fn index(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(1)
    }
}

ast_node! {
    /// `(expr1, expr2, ..)`
    pub struct TupleExpr,
    SK::TupleExpr,
}
impl TupleExpr {
    /// Returns the expressions in the tuple.
    pub fn elems(&self) -> impl Iterator<Item = Option<Expr>> {
        self.syntax().children().map(Expr::cast)
    }
}

ast_node! {
    /// `[expr1, expr2, ..]`
    pub struct ArrayExpr,
    SK::ArrayExpr,
}
impl ArrayExpr {
    /// Returns the expressions in the array.
    /// Returns the expressions in the tuple.
    pub fn elems(&self) -> impl Iterator<Item = Option<Expr>> {
        self.syntax().children().map(Expr::cast)
    }
}

ast_node! {
    /// `[expr; size]`
    pub struct ArrayRepExpr,
    SK::ArrayRepExpr,
}
impl ArrayRepExpr {
    /// Returns the expression being repeated.
    pub fn val(&self) -> Option<Expr> {
        support::child(self.syntax())
    }

    /// Returns the size of the array.
    pub fn len(&self) -> Option<Expr> {
        support::children(self.syntax()).nth(1)
    }
}

ast_node! {
    pub struct LitExpr,
    SK::LitExpr
}
impl LitExpr {
    /// Returns the literal of the literal expression.
    pub fn lit(&self) -> Option<super::Lit> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `if cond { then } else { else_ }`
    pub struct IfExpr,
    SK::IfExpr
}
impl IfExpr {
    /// Returns the condition of the if expression.
    pub fn cond(&self) -> Option<Expr> {
        support::child(self.syntax())
    }

    /// Returns the then block of the if expression.
    pub fn then(&self) -> Option<BlockExpr> {
        self.syntax().children().skip(1).find_map(BlockExpr::cast)
    }

    /// Returns the else block of the if expression.
    pub fn else_(&self) -> Option<Expr> {
        self.syntax().children().filter_map(Expr::cast).nth(2)
    }
}

ast_node! {
    /// `match expr { arm1, arm2, .. }`
    pub struct MatchExpr,
    SK::MatchExpr
}
impl MatchExpr {
    /// Returns the expression being matched.
    pub fn scrutinee(&self) -> Option<Expr> {
        support::child(self.syntax())
    }

    /// Returns the arms of the match expression.
    pub fn arms(&self) -> Option<MatchArmList> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `with (Effect = value, ..) { .. }`
    ///
    /// Also supports shorthand `with (value1, value2, ..) { .. }`, where
    /// effect keys are inferred from the bound values.
    pub struct WithExpr,
    SK::WithExpr
}
impl WithExpr {
    pub fn params(&self) -> Option<WithParamList> {
        support::child(self.syntax())
    }

    pub fn body(&self) -> Option<BlockExpr> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct WithParamList,
    SK::WithParamList,
    IntoIterator<Item=WithParam>,
}

ast_node! {
    pub struct WithParam,
    SK::WithParam,
}
impl WithParam {
    /// Returns the effect key path on the left-hand side.
    pub fn path(&self) -> Option<super::Path> {
        support::child(self.syntax())
    }

    pub fn eq(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Eq)
    }

    /// Returns the value expression if the value is an expression.
    pub fn value_expr(&self) -> Option<Expr> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `(expr)`
    pub struct ParenExpr,
    SK::ParenExpr
}
impl ParenExpr {
    /// Returns the expression in the parentheses.
    pub fn expr(&self) -> Option<Expr> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `x = 1`
    pub struct AssignExpr,
    SK::AssignExpr,
}
impl AssignExpr {
    /// Returns the expression of the lhs and rhs of the assignment.
    pub fn lhs_expr(&self) -> Option<super::Expr> {
        support::children(self.syntax()).next()
    }

    pub fn rhs_expr(&self) -> Option<super::Expr> {
        support::children(self.syntax()).nth(1)
    }

    pub fn eq(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Eq)
    }
}

ast_node! {
    /// `x += 1`
    pub struct AugAssignExpr,
    SK::AugAssignExpr,
}
impl AugAssignExpr {
    /// Returns the expression of the lhs of the aug assignment.
    pub fn lhs_expr(&self) -> Option<super::Expr> {
        support::children(self.syntax()).next()
    }

    pub fn op(&self) -> Option<super::ArithBinOp> {
        self.syntax()
            .children_with_tokens()
            .find_map(ArithBinOp::from_node_or_token)
    }

    /// Returns the expression of the rhs of the assignment.
    pub fn rhs_expr(&self) -> Option<super::Expr> {
        support::children(self.syntax()).nth(1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum ExprKind {
    Lit(LitExpr),
    Block(BlockExpr),
    Bin(BinExpr),
    Un(UnExpr),
    Cast(CastExpr),
    Call(CallExpr),
    MethodCall(MethodCallExpr),
    Path(PathExpr),
    RecordInit(RecordInitExpr),
    Field(FieldExpr),
    Index(IndexExpr),
    Tuple(TupleExpr),
    Array(ArrayExpr),
    ArrayRep(ArrayRepExpr),
    If(IfExpr),
    Match(MatchExpr),
    With(WithExpr),
    Paren(ParenExpr),
    Assign(AssignExpr),
    AugAssign(AugAssignExpr),
}

ast_node! {
    /// `{ label1: expr1, expr2 }`
    pub struct FieldList,
    SK::RecordFieldList,
    IntoIterator<Item=RecordField>
}
ast_node! {
    pub struct RecordField,
    SK::RecordField,
}
impl RecordField {
    /// Returns the name of the field.
    pub fn label(&self) -> Option<SyntaxToken> {
        support::token(self.syntax(), SK::Ident)
    }

    /// Returns the expression of the field.
    pub fn expr(&self) -> Option<Expr> {
        support::child(self.syntax())
    }
}

ast_node! {
    pub struct MatchArmList,
    SK::MatchArmList,
    IntoIterator<Item=MatchArm>
}
ast_node! {
    pub struct MatchArm,
    SK::MatchArm
}
impl MatchArm {
    /// Returns the pattern of the match arm.
    pub fn pat(&self) -> Option<super::Pat> {
        support::child(self.syntax())
    }

    /// Returns the body of the match arm.
    pub fn body(&self) -> Option<Expr> {
        support::child(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    Arith(ArithBinOp),
    Comp(CompBinOp),
    Logical(LogicalBinOp),
}

impl BinOp {
    pub fn syntax(&self) -> crate::NodeOrToken {
        match self {
            BinOp::Arith(op) => op.syntax(),
            BinOp::Comp(op) => op.syntax(),
            BinOp::Logical(op) => op.syntax(),
        }
    }

    pub(super) fn from_node_or_token(node_or_token: crate::NodeOrToken) -> Option<Self> {
        match node_or_token {
            rowan::NodeOrToken::Token(token) => Self::from_token(token),
            rowan::NodeOrToken::Node(node) => Self::from_node(node),
        }
    }
    pub(super) fn from_token(token: SyntaxToken) -> Option<Self> {
        ArithBinOp::from_token(token.clone())
            .map(Self::Arith)
            .or_else(|| CompBinOp::from_token(token.clone()).map(Self::Comp))
            .or_else(move || LogicalBinOp::from_token(token).map(Self::Logical))
    }

    pub(super) fn from_node(node: SyntaxNode) -> Option<Self> {
        ArithBinOp::from_node(node.clone())
            .map(Self::Arith)
            .or_else(|| CompBinOp::from_node(node).map(Self::Comp))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnOp {
    /// `+`
    Plus(SyntaxToken),
    /// `-`
    Minus(SyntaxToken),
    /// `!`
    Not(SyntaxToken),
    /// `~`
    BitNot(SyntaxToken),
}
impl UnOp {
    pub fn syntax(&self) -> SyntaxToken {
        match self {
            UnOp::Plus(token) => token.clone(),
            UnOp::Minus(token) => token.clone(),
            UnOp::Not(token) => token.clone(),
            UnOp::BitNot(token) => token.clone(),
        }
    }

    fn from_token(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            SK::Plus => Some(Self::Plus(token)),
            SK::Minus => Some(Self::Minus(token)),
            SK::Not => Some(Self::Not(token)),
            SK::Tilde => Some(Self::BitNot(token)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArithBinOp {
    /// `+`
    Add(SyntaxToken),
    /// `-`
    Sub(SyntaxToken),
    /// `*`
    Mul(SyntaxToken),
    /// `/`
    Div(SyntaxToken),
    /// `%`
    Mod(SyntaxToken),
    /// `**`
    Pow(SyntaxToken),
    /// `<<`
    LShift(SyntaxNode),
    /// `>>`
    RShift(SyntaxNode),
    /// `&`
    BitAnd(SyntaxToken),
    /// `|`
    BitOr(SyntaxToken),
    /// `^`
    BitXor(SyntaxToken),
}
impl ArithBinOp {
    pub fn syntax(&self) -> crate::NodeOrToken {
        match self {
            ArithBinOp::Add(token) => token.clone().into(),
            ArithBinOp::Sub(token) => token.clone().into(),
            ArithBinOp::Mul(token) => token.clone().into(),
            ArithBinOp::Div(token) => token.clone().into(),
            ArithBinOp::Mod(token) => token.clone().into(),
            ArithBinOp::Pow(token) => token.clone().into(),
            ArithBinOp::LShift(node) => node.clone().into(),
            ArithBinOp::RShift(node) => node.clone().into(),
            ArithBinOp::BitAnd(token) => token.clone().into(),
            ArithBinOp::BitOr(token) => token.clone().into(),
            ArithBinOp::BitXor(token) => token.clone().into(),
        }
    }

    pub(super) fn from_node_or_token(
        node_or_token: rowan::NodeOrToken<SyntaxNode, SyntaxToken>,
    ) -> Option<Self> {
        match node_or_token {
            rowan::NodeOrToken::Token(token) => Self::from_token(token),
            rowan::NodeOrToken::Node(node) => Self::from_node(node),
        }
    }

    // NOTE: We need to have `from_node` because `<<` and `>>` are not primitive
    // tokens in our lexer.
    pub(super) fn from_node(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SK::LShift => Some(Self::LShift(node)),
            SK::RShift => Some(Self::RShift(node)),
            _ => None,
        }
    }

    pub(super) fn from_token(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            SK::Plus => Some(Self::Add(token)),
            SK::Minus => Some(Self::Sub(token)),
            SK::Star => Some(Self::Mul(token)),
            SK::Slash => Some(Self::Div(token)),
            SK::Percent => Some(Self::Mod(token)),
            SK::Star2 => Some(Self::Pow(token)),
            SK::Amp => Some(Self::BitAnd(token)),
            SK::Pipe => Some(Self::BitOr(token)),
            SK::Hat => Some(Self::BitXor(token)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompBinOp {
    /// `==`
    Eq(SyntaxToken),
    /// `!=`
    NotEq(SyntaxToken),
    /// `<`
    Lt(SyntaxToken),
    /// `<=`
    LtEq(SyntaxNode),
    /// `>`
    Gt(SyntaxToken),
    /// `>=`
    GtEq(SyntaxNode),
}
impl CompBinOp {
    pub fn syntax(&self) -> crate::NodeOrToken {
        match self {
            CompBinOp::Eq(token) => token.clone().into(),
            CompBinOp::NotEq(token) => token.clone().into(),
            CompBinOp::Lt(token) => token.clone().into(),
            CompBinOp::LtEq(node) => node.clone().into(),
            CompBinOp::Gt(token) => token.clone().into(),
            CompBinOp::GtEq(node) => node.clone().into(),
        }
    }

    pub(super) fn from_token(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            SK::Eq2 => Some(Self::Eq(token)),
            SK::NotEq => Some(Self::NotEq(token)),
            SK::Lt => Some(Self::Lt(token)),
            SK::Gt => Some(Self::Gt(token)),
            _ => None,
        }
    }

    pub(super) fn from_node(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            SK::LtEq => Some(Self::LtEq(node)),
            SK::GtEq => Some(Self::GtEq(node)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LogicalBinOp {
    /// `&&`
    And(SyntaxToken),
    /// `||`
    Or(SyntaxToken),
}
impl LogicalBinOp {
    pub fn syntax(&self) -> crate::NodeOrToken {
        match self {
            LogicalBinOp::And(token) => token.clone().into(),
            LogicalBinOp::Or(token) => token.clone().into(),
        }
    }

    pub(super) fn from_token(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            SK::Amp2 => Some(Self::And(token)),
            SK::Pipe2 => Some(Self::Or(token)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::*, lexer::Lexer, parser::Parser};

    use derive_more::TryIntoError;
    use wasm_bindgen_test::wasm_bindgen_test;

    fn parse_expr<T>(source: &str) -> T
    where
        T: TryFrom<ExprKind, Error = TryIntoError<ExprKind>>,
    {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        crate::parser::expr::parse_expr(&mut parser).unwrap();
        Expr::cast(parser.finish_to_node().0)
            .unwrap()
            .kind()
            .try_into()
            .unwrap()
    }

    #[test]
    #[wasm_bindgen_test]
    fn block_expr() {
        let source = r#"{
            let a = 1
            let b = a + 2
            return b
        }"#;
        let block_expr: BlockExpr = parse_expr(source);
        assert_eq!(block_expr.stmts().count(), 3);
    }

    #[test]
    #[wasm_bindgen_test]
    fn bin_expr() {
        let bin_expr: BinExpr = parse_expr("1 + 2");
        assert!(matches!(bin_expr.lhs().unwrap().kind(), ExprKind::Lit(_)));
        assert!(matches!(
            bin_expr.op().unwrap(),
            BinOp::Arith(ArithBinOp::Add(_))
        ));
        assert!(matches!(bin_expr.rhs().unwrap().kind(), ExprKind::Lit(_)));

        let bin_expr: BinExpr = parse_expr("1 <= 2");
        assert!(matches!(
            bin_expr.op().unwrap(),
            BinOp::Comp(CompBinOp::LtEq(_))
        ));
    }

    #[test]
    #[wasm_bindgen_test]
    fn un_expr() {
        let un_expr: UnExpr = parse_expr("-1");
        assert!(matches!(un_expr.op().unwrap(), UnOp::Minus(_)));
        assert!(matches!(un_expr.expr().unwrap().kind(), ExprKind::Lit(_)));
    }

    #[test]
    #[wasm_bindgen_test]
    fn call_expr() {
        let call_expr: CallExpr = parse_expr("foo<i32, T>(1, label: 2, 3 + 4)");
        let ExprKind::Path(path) = call_expr.callee().unwrap().kind() else {
            panic!();
        };

        assert_eq!(
            path.path()
                .unwrap()
                .segments()
                .next()
                .unwrap()
                .generic_args()
                .unwrap()
                .into_iter()
                .count(),
            2
        );

        for (i, arg) in call_expr.args().unwrap().into_iter().enumerate() {
            match i {
                0 => {
                    assert!(arg.label().is_none());
                    assert!(matches!(arg.expr().unwrap().kind(), ExprKind::Lit(_)))
                }
                1 => {
                    assert_eq!(arg.label().unwrap().text(), "label");
                    assert!(matches!(arg.expr().unwrap().kind(), ExprKind::Lit(_)))
                }
                2 => {
                    assert!(arg.label().is_none());
                    assert!(matches!(arg.expr().unwrap().kind(), ExprKind::Bin(_)))
                }
                _ => panic!("unexpected arg"),
            }
        }
    }

    #[test]
    #[wasm_bindgen_test]
    fn method_call_expr() {
        let method_call_expr: MethodCallExpr = parse_expr("foo.bar<i32>(1, label: 2, 3 + 4)");

        assert!(matches!(
            method_call_expr.receiver().unwrap().kind(),
            ExprKind::Path(_)
        ));

        assert_eq!(method_call_expr.method_name().unwrap().text(), "bar");

        assert!(matches!(
            method_call_expr
                .generic_args()
                .unwrap()
                .into_iter()
                .collect::<Vec<_>>()
                .len(),
            1
        ));

        for (i, arg) in method_call_expr.args().unwrap().into_iter().enumerate() {
            match i {
                0 => {
                    assert!(arg.label().is_none());
                    assert!(matches!(arg.expr().unwrap().kind(), ExprKind::Lit(_)))
                }
                1 => {
                    assert_eq!(arg.label().unwrap().text(), "label");
                    assert!(matches!(arg.expr().unwrap().kind(), ExprKind::Lit(_)))
                }
                2 => {
                    assert!(arg.label().is_none());
                    assert!(matches!(arg.expr().unwrap().kind(), ExprKind::Bin(_)))
                }
                _ => panic!("unexpected arg"),
            }
        }
    }

    #[test]
    #[wasm_bindgen_test]
    fn record_init_expr() {
        let record_init_expr: RecordInitExpr = parse_expr("Foo { a: 1, b: 2, c: 3 }");

        assert!(record_init_expr.path().is_some());
        for (i, field) in record_init_expr.fields().unwrap().into_iter().enumerate() {
            match i {
                0 => {
                    assert_eq!(field.label().unwrap().text(), "a");
                    assert!(matches!(field.expr().unwrap().kind(), ExprKind::Lit(_)))
                }
                1 => {
                    assert_eq!(field.label().unwrap().text(), "b");
                    assert!(matches!(field.expr().unwrap().kind(), ExprKind::Lit(_)))
                }
                2 => {
                    assert_eq!(field.label().unwrap().text(), "c");
                    assert!(matches!(field.expr().unwrap().kind(), ExprKind::Lit(_)))
                }
                _ => panic!("unexpected field"),
            }
        }
    }

    #[test]
    #[wasm_bindgen_test]
    fn field_expr() {
        let field_expr: FieldExpr = parse_expr("foo(1, 2).bar");

        assert!(matches!(
            field_expr.receiver().unwrap().kind(),
            ExprKind::Call(_)
        ));
        assert_eq!(field_expr.field_name().unwrap().text(), "bar");

        let field_expr: FieldExpr = parse_expr("(1, 2).1");

        assert!(matches!(
            field_expr.receiver().unwrap().kind(),
            ExprKind::Tuple(_)
        ));
        assert_eq!(field_expr.field_index().unwrap().token().text(), "1");
    }

    #[test]
    #[wasm_bindgen_test]
    fn tuple_expr() {
        let tuple_expr: TupleExpr = parse_expr("(1, 2, 3)");

        for (i, expr) in tuple_expr.elems().flatten().enumerate() {
            match i {
                0 => assert!(matches!(expr.kind(), ExprKind::Lit(_))),
                1 => assert!(matches!(expr.kind(), ExprKind::Lit(_))),
                2 => assert!(matches!(expr.kind(), ExprKind::Lit(_))),
                _ => panic!("unexpected expr"),
            }
        }
    }

    #[test]
    #[wasm_bindgen_test]
    fn array_expr() {
        let array_expr: ArrayExpr = parse_expr("[1, 2, 3]");

        for (i, expr) in array_expr.elems().flatten().enumerate() {
            match i {
                0 => assert!(matches!(expr.kind(), ExprKind::Lit(_))),
                1 => assert!(matches!(expr.kind(), ExprKind::Lit(_))),
                2 => assert!(matches!(expr.kind(), ExprKind::Lit(_))),
                _ => panic!("unexpected expr"),
            }
        }
    }

    #[test]
    #[wasm_bindgen_test]
    fn index_expr() {
        let index_expr: IndexExpr = parse_expr("foo[1]");

        assert!(matches!(
            index_expr.expr().unwrap().kind(),
            ExprKind::Path(_)
        ));
        assert!(matches!(
            index_expr.index().unwrap().kind(),
            ExprKind::Lit(_)
        ));
    }

    #[test]
    #[wasm_bindgen_test]
    fn array_rep_expr() {
        let array_rep_expr: ArrayRepExpr = parse_expr("[1; 2]");

        assert!(matches!(
            array_rep_expr.val().unwrap().kind(),
            ExprKind::Lit(_)
        ));
        assert!(matches!(
            array_rep_expr.len().unwrap().kind(),
            ExprKind::Lit(_)
        ));
    }

    #[test]
    #[wasm_bindgen_test]
    fn if_expr() {
        let if_expr: IfExpr = parse_expr("if true { 1 } else { 2 }");
        assert!(matches!(if_expr.cond().unwrap().kind(), ExprKind::Lit(_)));
        assert!(if_expr.then().is_some());

        let if_expr: IfExpr = parse_expr("if { true } { return } else { continue }");
        if let ExprKind::Block(stmts) = if_expr.cond().unwrap().kind() {
            assert!(matches!(
                stmts.into_iter().next().unwrap().kind(),
                crate::ast::StmtKind::Expr(_)
            ))
        } else {
            panic!("expected block statement");
        };
        matches!(
            if_expr.then().unwrap().into_iter().next().unwrap().kind(),
            crate::ast::StmtKind::Return(_)
        );
        let ExprKind::Block(else_) = if_expr.else_().unwrap().kind() else {
            panic!("expected block statement");
        };
        matches!(
            else_.into_iter().next().unwrap().kind(),
            crate::ast::StmtKind::Return(_)
        );

        let if_expr: IfExpr = parse_expr("if false { return } else if true { continue }");
        assert!(matches!(if_expr.else_().unwrap().kind(), ExprKind::If(_)));
    }

    #[test]
    #[wasm_bindgen_test]
    fn match_expr() {
        let source = r#"
            match foo {
                Foo::Bar => { 2 },
                Bar::Baz(Int) => (4),
                _ => 5,
            }
        }"#;

        let match_expr: MatchExpr = parse_expr(source);

        assert!(matches!(
            match_expr.scrutinee().unwrap().kind(),
            ExprKind::Path(_)
        ));
        let mut count = 0;
        for arm in match_expr.arms().unwrap() {
            match count {
                0 => {
                    assert!(matches!(arm.pat().unwrap().kind(), PatKind::Path(_)));
                    assert!(matches!(arm.body().unwrap().kind(), ExprKind::Block(_)));
                }

                1 => {
                    assert!(matches!(arm.pat().unwrap().kind(), PatKind::PathTuple(_)));
                    assert!(matches!(arm.body().unwrap().kind(), ExprKind::Paren(_)));
                }

                2 => {
                    assert!(matches!(arm.pat().unwrap().kind(), PatKind::WildCard(_)));
                    assert!(matches!(arm.body().unwrap().kind(), ExprKind::Lit(_)));
                }
                _ => panic!("unexpected arm"),
            }
            count += 1;
        }
        assert_eq!(count, 3)
    }
    #[test]
    #[wasm_bindgen_test]
    fn assign() {
        let assign_expr: AssignExpr = parse_expr(r#"Foo{x, y} = foo"#);

        assert!(matches!(
            assign_expr.lhs_expr().unwrap().kind(),
            ExprKind::RecordInit(_)
        ));
        assert!(matches!(
            assign_expr.rhs_expr().unwrap().kind(),
            ExprKind::Path(_)
        ));
    }

    #[test]
    #[wasm_bindgen_test]
    fn aug_assign() {
        let aug_assign_expr: AugAssignExpr = parse_expr("x += 1");
        assert!(matches!(
            aug_assign_expr.lhs_expr().unwrap().kind(),
            ExprKind::Path(_)
        ));
        assert!(matches!(
            aug_assign_expr.op().unwrap(),
            crate::ast::ArithBinOp::Add(_)
        ));

        let aug_assign_expr: AugAssignExpr = parse_expr("x.y <<= 1");
        assert!(matches!(
            aug_assign_expr.lhs_expr().unwrap().kind(),
            ExprKind::Field(_)
        ));
        assert!(matches!(
            aug_assign_expr.op().unwrap(),
            crate::ast::ArithBinOp::LShift(_)
        ));
    }
}
