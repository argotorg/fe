//! Error recovery must finish, and keep every character, on any input.
//!
//! The inputs are generated without randomness, so a failure is replayed from
//! the input printed in its message.
#![cfg(not(target_family = "wasm"))]

use std::{collections::BTreeSet, time::Duration};

use fe_parser::{
    RecoveryMode, SyntaxKind, SyntaxNode,
    lexer::Lexer,
    parse_source_file,
    parser::token_stream::{LexicalToken, TokenStream},
};
use test_utils::deadline::run_inputs;

/// A termination guard, not a performance assertion: a parse takes
/// milliseconds at most. A parse that loops keeps allocating until the test
/// process exits, so the deadline is short.
const DEADLINE: Duration = Duration::from_secs(3);

/// Valid sources that the typing and mutation inputs are derived from.
const VALID_SOURCES: &[&str] = &[
    include_str!("../test_files/syntax_node/items/contract_field_slot.fe"),
    include_str!("../test_files/syntax_node/items/contract_init_recv.fe"),
    include_str!("../test_files/syntax_node/items/extern.fe"),
    include_str!("../test_files/syntax_node/items/func.fe"),
    include_str!("../test_files/syntax_node/items/impl_trait.fe"),
    include_str!("../test_files/syntax_node/items/mod.fe"),
    include_str!("../test_files/syntax_node/items/trait_assoc_type.fe"),
    include_str!("../test_files/syntax_node/items/unsafe_methods.fe"),
    include_str!("../test_files/syntax_node/structs/attr.fe"),
    "#![arithmetic(unchecked)]\n\n#[inline]\nfn f() {}\n\nmod m {\n    #![arithmetic(checked)]\n    #[test]\n    fn g() {}\n}\n",
];

/// Source that opens each place where a list of elements is parsed.
const CONTAINERS: &[&str] = &[
    "",
    "mod m {",
    "fn f() {",
    "fn f() { {",
    "fn f() { if x {",
    "fn f() { for i in x {",
    "fn f() { match x {",
    "fn f() { unsafe {",
    "impl S {",
    "impl T for S {",
    "trait T {",
    "extern {",
    "struct S {",
    "enum E {",
    "msg M {",
    "contract C {",
    "contract C { init() {",
    "contract C { recv {",
];

/// What follows the token under test: nothing, the closing braces, or the
/// closing braces and a valid item.
const ENDINGS: &[&str] = &["", "\n}", "\n}\n}\nfn g() {}\n"];

fn check(input: &str) {
    let (green, _) = parse_source_file(input, RecoveryMode::Recover);
    assert_eq!(
        SyntaxNode::new_root(green).to_string(),
        input,
        "the tree does not reproduce the input"
    );
}

fn check_all(inputs: impl IntoIterator<Item = String>) {
    let inputs: BTreeSet<String> = inputs.into_iter().collect();
    run_inputs(inputs, DEADLINE, || check as fn(&str));
}

fn lexes_to(text: &str, kind: SyntaxKind) -> bool {
    let mut lexer = Lexer::new(text);
    matches!((lexer.next(), lexer.next()), (Some(tok), None) if tok.syntax_kind() == kind)
}

/// The source text of every kind of token. Fixed-text tokens come from
/// `SyntaxKind::describe`, so a token added to the lexer is covered.
fn tokens() -> Vec<String> {
    let mut tokens: Vec<String> = ["x", "1", "\"s\"", "// c\n", "/// d\n", "\n"]
        .map(String::from)
        .into();
    for raw in 0..=u16::MAX {
        let Ok(kind) = SyntaxKind::try_from(raw) else {
            continue;
        };
        let text = kind
            .describe()
            .strip_prefix('`')
            .and_then(|description| description.strip_suffix('`'));
        if let Some(text) = text.filter(|text| lexes_to(text, kind)) {
            tokens.push(text.to_string());
        }
    }
    tokens
}

fn prefixes(source: &str) -> impl Iterator<Item = String> + '_ {
    source
        .char_indices()
        .map(|(end, _)| source[..end].to_string())
        .chain([source.to_string()])
}

/// The source with one character or one token deleted or duplicated.
fn mutations(source: &str) -> Vec<String> {
    let mut mutated = Vec::new();
    let mut edit = |start: usize, end: usize| {
        mutated.push(format!("{}{}", &source[..start], &source[end..]));
        mutated.push(format!(
            "{}{}{}",
            &source[..end],
            &source[start..end],
            &source[end..]
        ));
    };

    for (start, c) in source.char_indices() {
        edit(start, start + c.len_utf8());
    }
    let mut lexer = Lexer::new(source);
    let mut start = 0;
    while let Some(token) = lexer.next() {
        let end = start + token.text().len();
        edit(start, end);
        start = end;
    }
    mutated
}

#[test]
fn single_tokens_and_short_sequences_terminate() {
    let tokens = tokens();
    assert!(tokens.len() > 80, "found only {} tokens", tokens.len());

    let mut inputs = Vec::new();
    for first in &tokens {
        inputs.push(first.clone());
        for second in &tokens {
            inputs.push(format!("{first} {second}"));
        }
    }

    // Everything that can open or close an attribute, in every order.
    let attribute_parts = [
        "#", "!", "[", "]", "(", ")", "{", "}", ",", "=", "fn", "pub", "x",
    ];
    for first in attribute_parts {
        for second in attribute_parts {
            for third in attribute_parts {
                inputs.push(format!("{first}{second}{third}"));
                inputs.push(format!("{first} {second} {third}"));
            }
        }
    }
    check_all(inputs);
}

#[test]
fn tokens_inside_containers_terminate() {
    let mut inputs = Vec::new();
    for container in CONTAINERS {
        for token in tokens() {
            for ending in ENDINGS {
                inputs.push(format!("{container} {token}{ending}"));
                inputs.push(format!("{container}\n{token}{ending}"));
            }
        }
    }
    check_all(inputs);
}

#[test]
fn typing_a_valid_source_terminates() {
    check_all(VALID_SOURCES.iter().flat_map(|source| prefixes(source)));
}

#[test]
fn mutating_a_valid_source_terminates() {
    check_all(VALID_SOURCES.iter().flat_map(|source| mutations(source)));
}
