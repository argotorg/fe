use rowan::ast::{AstNode, support};

use super::ast_node;
use crate::SyntaxKind as SK;

ast_node! {
    /// A statement.
    /// Use [`Self::kind`] to get the specific kind of the statement.
    pub struct Stmt,
    SK::LetStmt
    | SK::ForStmt
    | SK::WhileStmt
    | SK::ContinueStmt
    | SK::BreakStmt
    | SK::ReturnStmt
    | SK::ExprStmt
}
impl Stmt {
    /// Returns the specific kind of the statement.
    pub fn kind(&self) -> StmtKind {
        match self.syntax().kind() {
            SK::LetStmt => StmtKind::Let(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::ForStmt => StmtKind::For(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::WhileStmt => StmtKind::While(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::ContinueStmt => StmtKind::Continue(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::BreakStmt => StmtKind::Break(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::ReturnStmt => StmtKind::Return(AstNode::cast(self.syntax().clone()).unwrap()),
            SK::ExprStmt => StmtKind::Expr(AstNode::cast(self.syntax().clone()).unwrap()),
            _ => unreachable!(),
        }
    }
}

ast_node! {
    /// `let x: i32 = 1`
    pub struct LetStmt,
    SK::LetStmt,
}
impl LetStmt {
    /// Returns the pattern of the binding.
    pub fn pat(&self) -> Option<super::Pat> {
        support::child(self.syntax())
    }

    /// Returns the type annotation.
    pub fn type_annotation(&self) -> Option<super::Type> {
        support::child(self.syntax())
    }

    /// Returns the initializer.
    pub fn initializer(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }
}

ast_node! {
    /// `for pat in expr {..}`
    pub struct ForStmt,
    SK::ForStmt
}
impl ForStmt {
    /// Returns the pattern of the binding in the for loop.
    pub fn pat(&self) -> Option<super::Pat> {
        support::child(self.syntax())
    }

    /// Returns the expression of the iterator in the for loop.
    pub fn iterable(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }

    pub fn body(&self) -> Option<super::BlockExpr> {
        let mut block_exprs = support::children(self.syntax());
        let first = block_exprs.next();
        match block_exprs.next() {
            Some(expr) => Some(expr),
            None => first,
        }
    }
}

ast_node! {
    /// `while cond {..}`
    pub struct WhileStmt,
    SK::WhileStmt
}
impl WhileStmt {
    /// Returns the condition of the while loop.
    pub fn cond(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }

    pub fn body(&self) -> Option<super::Expr> {
        let mut block_exprs = support::children(self.syntax());
        let first = block_exprs.next();
        match block_exprs.next() {
            Some(expr) => Some(expr),
            None => first,
        }
    }
}

ast_node! {
    /// `continue`
    pub struct ContinueStmt,
    SK::ContinueStmt
}

ast_node! {
    /// `break`
    pub struct BreakStmt,
    SK::BreakStmt
}

ast_node! {
    /// `return` or
    /// `return expr`
    pub struct ReturnStmt,
    SK::ReturnStmt
}
impl ReturnStmt {
    /// Returns the expression of the return statement.
    pub fn expr(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }

    /// Returns `true` if there is an expression or `Error` node after `return`
    /// keyword.
    pub fn has_value(&self) -> bool {
        self.syntax().children().count() >= 1
    }
}

ast_node! {
    pub struct ExprStmt,
    SK::ExprStmt
}
impl ExprStmt {
    /// Returns the expression of the expression statement.
    pub fn expr(&self) -> Option<super::Expr> {
        support::child(self.syntax())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, derive_more::From, derive_more::TryInto)]
pub enum StmtKind {
    Let(LetStmt),
    For(ForStmt),
    While(WhileStmt),
    Continue(ContinueStmt),
    Break(BreakStmt),
    Return(ReturnStmt),
    Expr(ExprStmt),
}

#[cfg(test)]
mod tests {
    use derive_more::TryIntoError;
    use wasm_bindgen_test::wasm_bindgen_test;

    use super::*;
    use crate::{
        ast::{PatKind, TypeKind},
        lexer::Lexer,
        parser::Parser,
    };

    fn parse_stmt<T>(source: &str) -> T
    where
        T: TryFrom<StmtKind, Error = TryIntoError<StmtKind>>,
    {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(lexer);
        crate::parser::stmt::parse_stmt(&mut parser).unwrap();
        Stmt::cast(parser.finish_to_node().0)
            .unwrap()
            .kind()
            .try_into()
            .unwrap()
    }

    #[test]
    #[wasm_bindgen_test]
    fn let_() {
        let let_stmt: LetStmt = parse_stmt("let x: i32 = 1");

        assert!(matches!(let_stmt.pat().unwrap().kind(), PatKind::Path(_)));
        assert!(matches!(
            let_stmt.type_annotation().unwrap().kind(),
            TypeKind::Path(_)
        ));
        assert!(let_stmt.initializer().is_some());

        let let_stmt: LetStmt = parse_stmt("let x");
        assert!(matches!(let_stmt.pat().unwrap().kind(), PatKind::Path(_)));
        assert!(let_stmt.type_annotation().is_none());
        assert!(let_stmt.initializer().is_none());
    }

    #[test]
    #[wasm_bindgen_test]
    fn for_() {
        let source = r#"
            for x in foo {
                bar
            }
        "#;

        let for_stmt: ForStmt = parse_stmt(source);
        assert!(matches!(for_stmt.pat().unwrap().kind(), PatKind::Path(_)));
        assert!(for_stmt.iterable().is_some());
        assert!(for_stmt.body().is_some());
    }

    #[test]
    #[wasm_bindgen_test]
    fn while_() {
        let source = r#"
            while { x } {
                bar
            }
        "#;

        let while_stmt: WhileStmt = parse_stmt(source);
        assert!(while_stmt.cond().is_some());
        assert!(while_stmt.body().is_some());
        assert_ne!(while_stmt.cond(), while_stmt.body());
    }

    #[test]
    #[wasm_bindgen_test]
    fn return_() {
        let ret_stmt: ReturnStmt = parse_stmt("return x");
        assert!(ret_stmt.expr().is_some());

        let ret_stmt: ReturnStmt = parse_stmt("return");
        assert!(ret_stmt.expr().is_none());
    }
}
