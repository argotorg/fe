pub mod ast;
pub mod lexer;
pub mod parser;
pub mod syntax_kind;
pub mod syntax_node;

pub use rowan::TextSize;
use smallvec::SmallVec;
pub use syntax_kind::SyntaxKind;
pub use syntax_node::{FeLang, GreenNode, NodeOrToken, SyntaxNode, SyntaxToken, TextRange};

use parser::RootScope;

pub fn parse_source_file(text: &str) -> (GreenNode, Vec<ParseError>) {
    let lexer = lexer::Lexer::new(text);
    let mut parser = parser::Parser::new(lexer);
    let checkpoint = parser.enter(RootScope::default(), None);

    let _ = parser.parse(parser::ItemListScope::default());

    parser.leave(checkpoint);
    let (node, errs) = parser.finish();
    (node, errs)
}

/// An parse error which is accumulated in the [`parser::Parser`] while parsing.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    Expected(SmallVec<SyntaxKind, 2>, ExpectedKind, TextSize),
    Unexpected(String, TextRange),
    Msg(String, TextRange),
}

impl ParseError {
    pub fn expected(tokens: &[SyntaxKind], kind: Option<ExpectedKind>, pos: TextSize) -> Self {
        ParseError::Expected(
            SmallVec::from_iter(tokens.iter().copied()),
            kind.unwrap_or(ExpectedKind::Unspecified),
            pos,
        )
    }

    pub fn msg(&self) -> String {
        match self {
            ParseError::Expected(_, exp, _) => match exp {
                ExpectedKind::Body(kind) => format!("{} requires a body", kind.describe()),
                ExpectedKind::Name(kind) => format!("expected name for {}", kind.describe()),
                ExpectedKind::ClosingBracket { bracket, parent } => format!(
                    "missing closing {} for {}",
                    bracket.describe(),
                    parent.describe()
                ),
                ExpectedKind::Separator { separator, element } => {
                    format!(
                        "expected {} separator after {}",
                        separator.describe(),
                        element.describe()
                    )
                }
                ExpectedKind::TypeSpecifier(kind) => {
                    format!("missing type bound for {}", kind.describe())
                }
                ExpectedKind::Syntax(kind) => format!("expected {}", kind.describe()),
                ExpectedKind::Unspecified => self.label(),
            },
            ParseError::Unexpected(m, _) => m.clone(),
            ParseError::Msg(m, _) => m.clone(),
        }
    }

    pub fn label(&self) -> String {
        match self {
            ParseError::Expected(tokens, _, _) => {
                if tokens.len() == 1 {
                    return format!("expected {}", tokens[0].describe());
                }

                let mut s = "expected ".to_string();
                let mut delim = "";
                for (i, t) in tokens.iter().enumerate() {
                    s.push_str(delim);
                    s.push_str(t.describe());

                    delim = if i + 2 == tokens.len() { " or " } else { ", " };
                }
                s
            }
            ParseError::Unexpected(_, _) => "unexpected".into(),
            ParseError::Msg(msg, _) => msg.clone(),
        }
    }

    pub fn range(&self) -> TextRange {
        match self {
            ParseError::Expected(_, _, pos) => TextRange::empty(*pos),
            ParseError::Unexpected(_, r) | ParseError::Msg(_, r) => *r,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExpectedKind {
    Body(SyntaxKind),
    Name(SyntaxKind),
    ClosingBracket {
        bracket: SyntaxKind,
        parent: SyntaxKind,
    },
    TypeSpecifier(SyntaxKind),
    Separator {
        separator: SyntaxKind,
        element: SyntaxKind,
    },
    Syntax(SyntaxKind),
    Unspecified,
    // TODO:
    //  - newline after attribute in attrlistscope
    //
}
