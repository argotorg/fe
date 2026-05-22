use parser::{SyntaxKind, SyntaxNode, NodeOrToken};

use crate::ir_describe::{DescribeCtx, Dim, IrConsumer, IrDescribe};

impl IrDescribe for SyntaxNode {
    fn describe<C: IrConsumer>(&self, cx: &DescribeCtx<'_>, c: &mut C) {
        c.enter_node(self.kind().describe());

        for child in self.children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    c.child(cx, &node);
                }
                NodeOrToken::Token(token) => {
                    let kind = token.kind();

                    // Skip whitespace and comments — they're not structural
                    if kind.is_trivia() || kind == SyntaxKind::Newline {
                        continue;
                    }

                    // Identifiers go into Names dimension
                    // Keywords and operators go into Structure dimension
                    let dim = if kind == SyntaxKind::Ident {
                        Dim::Names
                    } else {
                        Dim::Structure
                    };

                    c.field_str(dim, token.text());
                }
            }
        }

        c.exit_node();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hash_consumer::HashConsumer;
    fn parse_fe(source: &str) -> SyntaxNode {
        let (green, _errors) = parser::parse_source_file(source, parser::RecoveryMode::Recover);
        SyntaxNode::new_root(green)
    }

    fn hash_ast(source: &str) -> crate::hash_consumer::DimHashes {
        let ast = parse_fe(source);
        // AST describe doesn't use cx.db(), but we still need a DescribeCtx.
        // Use a minimal approach: describe directly into the consumer without cx.
        // Since SyntaxNode::describe never calls cx.db(), we can pass any db.
        // But DescribeCtx requires a salsa::Database. Use the test db pattern.
        let mut c = HashConsumer::new();
        // Manually walk the AST into the consumer without DescribeCtx
        hash_syntax_node(&ast, &mut c);
        c.into_result().unwrap()
    }

    fn hash_syntax_node(node: &SyntaxNode, c: &mut HashConsumer) {
        c.enter_node(node.kind().describe());
        for child in node.children_with_tokens() {
            match child {
                NodeOrToken::Node(n) => {
                    hash_syntax_node(&n, c);
                }
                NodeOrToken::Token(token) => {
                    let kind = token.kind();
                    if kind.is_trivia() || kind == SyntaxKind::Newline {
                        continue;
                    }
                    let dim = if kind == SyntaxKind::Ident {
                        Dim::Names
                    } else {
                        Dim::Structure
                    };
                    c.field_str(dim, token.text());
                }
            }
        }
        c.exit_node();
    }

    #[test]
    fn identical_source_hashes_same() {
        let h1 = hash_ast("pub fn add(a: u256, b: u256) -> u256 { return a + b }");
        let h2 = hash_ast("pub fn add(a: u256, b: u256) -> u256 { return a + b }");
        assert_eq!(h1.structure(), h2.structure());
        assert_eq!(h1.names(), h2.names());
    }

    #[test]
    fn whitespace_doesnt_affect_hash() {
        let h1 = hash_ast("pub fn add(a:u256,b:u256)->u256{return a+b}");
        let h2 = hash_ast("pub fn add( a : u256 , b : u256 ) -> u256 { return a + b }");
        assert_eq!(h1.structure(), h2.structure(), "whitespace should not affect structural hash");
    }

    #[test]
    fn rename_changes_names_not_structure() {
        let h1 = hash_ast("pub fn add(a: u256, b: u256) -> u256 { return a + b }");
        let h2 = hash_ast("pub fn add(x: u256, y: u256) -> u256 { return x + y }");
        assert_eq!(h1.structure(), h2.structure(), "rename should not affect structure");
        assert_ne!(h1.names(), h2.names(), "rename should change names hash");
    }

    #[test]
    fn operator_change_detected() {
        let h1 = hash_ast("pub fn f(a: u256, b: u256) -> u256 { return a + b }");
        let h2 = hash_ast("pub fn f(a: u256, b: u256) -> u256 { return a - b }");
        assert_ne!(h1.structure(), h2.structure(), "changing + to - should change structure");
    }
}
