#![allow(unused)]

use fe_parser::{
    SyntaxKind, lexer,
    parser::{
        Parser, RootScope, expr::parse_expr, item::ItemListScope, parse_pat, stmt::parse_stmt,
    },
    syntax_node::SyntaxNode,
};
use tracing::error;

type BoxedParseFn = Box<dyn Fn(&mut Parser<lexer::Lexer>)>;
pub struct TestRunner {
    f: BoxedParseFn,
    should_success: bool,
}

impl TestRunner {
    /// Constructs a new test runner.
    pub fn new<F>(f: F, should_success: bool) -> Self
    where
        F: Fn(&mut Parser<lexer::Lexer>) + 'static,
    {
        Self {
            f: Box::new(f),
            should_success,
        }
    }

    /// Constructs a test runner for parsing a list of expressions.
    pub fn item_list(should_success: bool) -> Self {
        fn parse(parser: &mut Parser<lexer::Lexer>) {
            parser.parse(ItemListScope::default());
        }

        Self::new(parse, should_success)
    }

    /// Constructs a test runner for parsing a list of statements.
    pub fn stmt_list(should_success: bool) -> Self {
        fn parse(parser: &mut Parser<lexer::Lexer>) {
            parser.set_newline_as_trivia(false);

            bump_newlines(parser);
            while parser.current_kind().is_some() {
                bump_newlines(parser);
                parse_stmt(parser);
                bump_newlines(parser);
            }
        }

        Self::new(parse, should_success)
    }

    /// Constructs a test runner for parsing a list of expressions.
    pub fn expr_list(should_success: bool) -> Self {
        fn parse(parser: &mut Parser<lexer::Lexer>) {
            parser.set_newline_as_trivia(false);

            bump_newlines(parser);
            while parser.current_kind().is_some() {
                bump_newlines(parser);
                parse_expr(parser);
                bump_newlines(parser);
            }
        }

        Self::new(parse, should_success)
    }

    /// Constructs a test runner for parsing a list of patterns.
    pub fn pat_list(should_success: bool) -> Self {
        fn parse(parser: &mut Parser<lexer::Lexer>) {
            while parser.current_kind().is_some() {
                parse_pat(parser);
            }
        }

        Self::new(parse, should_success)
    }

    pub fn run(&self, input: &str) -> SyntaxNode {
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let checkpoint = parser.enter(RootScope::default(), None);
        (self.f)(&mut parser);
        parser.leave(checkpoint);

        let (cst, errors) = parser.finish_to_node();

        for error in &errors {
            error!("{}@{:?}", error.msg(), error.range());
        }
        if self.should_success {
            error!("{cst:#?}");
            assert! {errors.is_empty()}
        } else {
            assert! {!errors.is_empty()}
        }
        assert_eq!(input, cst.to_string());

        cst
    }
}

pub fn bump_newlines(parser: &mut Parser<lexer::Lexer>) {
    while parser.current_kind() == Some(SyntaxKind::Newline) {
        parser.bump();
    }
}
