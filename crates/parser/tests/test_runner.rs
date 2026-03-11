#![allow(unused)]

use std::sync::Once;

use fe_parser::{
    SyntaxKind, lexer,
    parser::{
        ErrProof, ParseError, Parser, Recovery, RecoveryMode, RootScope, expr::parse_expr,
        item::ItemListScope, parse_pat, stmt::parse_stmt,
    },
    syntax_node::SyntaxNode,
};
use test_utils::normalize::normalize_newlines;
use tracing::error;

static INIT: Once = Once::new();

fn init_tracing() {
    INIT.call_once(|| {
        tracing_subscriber::fmt()
            .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
            .with_test_writer()
            .init();
    });
}

type BoxedParseFn = Box<dyn Fn(&mut Parser<lexer::Lexer>) -> Result<(), Recovery<ErrProof>>>;
pub struct TestRunner {
    f: BoxedParseFn,
    should_success: bool,
    use_recovery_mode: bool,
}

impl TestRunner {
    /// Constructs a new test runner.
    pub fn new<F>(f: F, should_success: bool) -> Self
    where
        F: Fn(&mut Parser<lexer::Lexer>) -> Result<(), Recovery<ErrProof>> + 'static,
    {
        Self {
            f: Box::new(f),
            should_success,
            use_recovery_mode: true,
        }
    }

    /// Whether or not the parser should attempt to automatically recover from syntax errors.
    pub fn set_recovery_mode(mut self, use_recovery_mode: bool) -> Self {
        self.use_recovery_mode = use_recovery_mode;
        self
    }

    /// Constructs a test runner for parsing a list of expressions.
    pub fn item_list(should_success: bool) -> Self {
        fn parse(parser: &mut Parser<lexer::Lexer>) -> Result<(), Recovery<ErrProof>> {
            parser.parse(ItemListScope::default())
        }

        Self::new(parse, should_success)
    }

    /// Constructs a test runner for parsing a list of statements.
    pub fn stmt_list(should_success: bool) -> Self {
        fn parse(parser: &mut Parser<lexer::Lexer>) -> Result<(), Recovery<ErrProof>> {
            parser.set_newline_as_trivia(false);

            bump_newlines(parser);
            while parser.current_kind().is_some() {
                bump_newlines(parser);
                parse_stmt(parser)?;
                bump_newlines(parser);
            }
            Ok(())
        }

        Self::new(parse, should_success)
    }

    /// Constructs a test runner for parsing a list of expressions.
    pub fn expr_list(should_success: bool) -> Self {
        fn parse(parser: &mut Parser<lexer::Lexer>) -> Result<(), Recovery<ErrProof>> {
            parser.set_newline_as_trivia(false);

            bump_newlines(parser);
            while parser.current_kind().is_some() {
                bump_newlines(parser);
                parse_expr(parser)?;
                bump_newlines(parser);
            }
            Ok(())
        }

        Self::new(parse, should_success)
    }

    /// Constructs a test runner for parsing a list of patterns.
    pub fn pat_list(should_success: bool) -> Self {
        fn parse(parser: &mut Parser<lexer::Lexer>) -> Result<(), Recovery<ErrProof>> {
            while parser.current_kind().is_some() {
                parse_pat(parser)?;
            }
            Ok(())
        }

        Self::new(parse, should_success)
    }

    pub fn run(&self, input: &str) -> Result<SyntaxNode, Vec<ParseError>> {
        init_tracing();
        let input = normalize_newlines(input);
        let input = input.as_ref();
        let lexer = lexer::Lexer::new(input);
        let mut parser = Parser::new(lexer, RecoveryMode::new(self.use_recovery_mode));

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
            return Err(errors);
        }
        assert_eq!(input, cst.to_string());

        Ok(cst)
    }
}

pub fn bump_newlines(parser: &mut Parser<lexer::Lexer>) {
    while parser.current_kind() == Some(SyntaxKind::Newline) {
        parser.bump();
    }
}
