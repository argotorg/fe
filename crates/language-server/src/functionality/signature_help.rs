use crate::{backend::Backend, util::to_offset_from_position};
use async_lsp::ResponseError;
use async_lsp::lsp_types::{
    ParameterInformation, ParameterLabel, SignatureHelp, SignatureHelpParams, SignatureInformation,
};
use common::InputDb;
use driver::DriverDataBase;
use hir::{
    core::semantic::reference::typed_body_for_body,
    hir_def::{CallableDef, Expr, Func, ItemKind, Partial, TopLevelMod},
    lower::{map_file_to_mod, parse_file_impl},
    span::{HirOrigin, body_source_map},
};
use parser::{NodeOrToken, SyntaxKind, SyntaxNode, TextSize};

pub async fn handle_signature_help(
    backend: &Backend,
    params: SignatureHelpParams,
) -> Result<Option<SignatureHelp>, ResponseError> {
    let url = backend.map_client_uri_to_internal(
        params
            .text_document_position_params
            .text_document
            .uri
            .clone(),
    );

    let file = backend
        .db
        .workspace()
        .get(&backend.db, &url)
        .ok_or_else(|| {
            ResponseError::new(
                async_lsp::ErrorCode::INTERNAL_ERROR,
                format!("File not found: {url}"),
            )
        })?;

    let file_text = file.text(&backend.db);
    let cursor = to_offset_from_position(params.text_document_position_params.position, file_text);
    let top_mod = map_file_to_mod(&backend.db, file);

    // Find the enclosing function call and get signature info
    if let Some(sig_info) = find_signature_at_cursor(&backend.db, top_mod, cursor) {
        Ok(Some(sig_info))
    } else {
        Ok(None)
    }
}

/// Find the signature help for a function call at the cursor position.
fn find_signature_at_cursor<'db>(
    db: &'db DriverDataBase,
    top_mod: TopLevelMod<'db>,
    cursor: TextSize,
) -> Option<SignatureHelp> {
    // The body of the function, contract `init` or recv arm at the cursor.
    let body = top_mod
        .find_enclosing_items(db, cursor)
        .into_iter()
        .find_map(|item| match item {
            ItemKind::Body(body) => Some(body),
            _ => None,
        })?;
    let typed_body = typed_body_for_body(db, body)?;
    let source_map = body_source_map(db, body);
    let root = SyntaxNode::new_root(parse_file_impl(db, top_mod).root.clone());

    // Find the call whose argument list holds the cursor and whose callee the
    // type checker resolved to a function. A call is lowered after its
    // arguments, so the first one found is the innermost.
    for (expr_id, expr_data) in body.exprs(db).iter() {
        // A method call's receiver is its first parameter, `self`, so its
        // arguments start at the second.
        let first_param = match expr_data {
            // A call of a callable value, such as a closure, runs the method
            // of its `Fn` trait, whose signature isn't the value's.
            Partial::Present(Expr::Call(..)) if typed_body.call_receiver(db, expr_id).is_none() => {
                0
            }
            Partial::Present(Expr::MethodCall(..)) => 1,
            _ => continue,
        };
        // A call a macro such as `format!` expands to isn't written in the
        // source, so it has no argument list there.
        if let HirOrigin::Raw(ptr) = &source_map.expr_map.node_to_source[expr_id]
            && let Some(args) = ptr.syntax_node_ptr().try_to_node(&root).and_then(|call| {
                call.children()
                    .find(|child| child.kind() == SyntaxKind::CallArgList)
            })
            && let Some(arg) = argument_index(&args, cursor)
            && let Some(CallableDef::Func(called_func)) = typed_body
                .callable_expr(expr_id)
                .map(|callable| callable.callable_def)
        {
            return Some(build_signature_help(db, called_func, first_param + arg));
        }
    }

    None
}

/// The index of the argument at `cursor` in the argument list `args`: the
/// number of the list's own commas before it, or `None` if the cursor is
/// outside the list. The end of a list that isn't closed yet, as while
/// typing, holds the cursor too.
fn argument_index(args: &SyntaxNode, cursor: TextSize) -> Option<usize> {
    let tokens: Vec<_> = args
        .children_with_tokens()
        .filter_map(NodeOrToken::into_token)
        .collect();
    let open = tokens
        .iter()
        .find(|token| token.kind() == SyntaxKind::LParen)?;
    let end = tokens
        .iter()
        .find(|token| token.kind() == SyntaxKind::RParen)
        .map_or(args.text_range().end(), |token| token.text_range().start());
    (open.text_range().end() <= cursor && cursor <= end).then(|| {
        tokens
            .iter()
            .filter(|token| token.kind() == SyntaxKind::Comma && token.text_range().end() <= cursor)
            .count()
    })
}

/// Build a SignatureHelp response for a function, with the parameter at
/// index `active` active. An argument past the last parameter is shown at
/// the last one.
fn build_signature_help<'db>(
    db: &'db DriverDataBase,
    func: Func<'db>,
    active: usize,
) -> SignatureHelp {
    let func_name = func
        .name(db)
        .to_opt()
        .map_or_else(|| "<anonymous>".to_string(), |n| n.data(db).to_string());

    // Build parameter info
    let mut params = Vec::new();
    let mut param_strings = Vec::new();

    for param in func.params(db) {
        let ty = param.ty(db);
        let ty_str = ty.pretty_print(db);

        let (label, doc) = if param.is_self_param(db) {
            ("self".to_string(), format!("self: {ty_str}"))
        } else if let Some(name) = param.name(db) {
            let name_str = name.data(db);
            (
                format!("{name_str}: {ty_str}"),
                format!("{name_str}: {ty_str}"),
            )
        } else {
            (ty_str.to_string(), ty_str.to_string())
        };

        param_strings.push(label.clone());
        params.push(ParameterInformation {
            label: ParameterLabel::Simple(label),
            documentation: Some(async_lsp::lsp_types::Documentation::String(doc)),
        });
    }
    // A variadic function's `...` stands for each extra argument.
    if func.is_variadic(db) {
        param_strings.push("...".to_string());
        params.push(ParameterInformation {
            label: ParameterLabel::Simple("...".to_string()),
            documentation: Some(async_lsp::lsp_types::Documentation::String(
                "extra C arguments".to_string(),
            )),
        });
    }
    let active_parameter = params
        .len()
        .checked_sub(1)
        .map(|last| active.min(last) as u32);

    // Build the return type
    let ret_ty = func.return_ty(db);
    let ret_ty_str = ret_ty.pretty_print(db);
    let ret_str = if ret_ty_str == "()" || ret_ty_str.is_empty() {
        String::new()
    } else {
        format!(" -> {ret_ty_str}")
    };

    // Build full signature label
    let signature_label = format!("fn {}({}){}", func_name, param_strings.join(", "), ret_str);

    let signature = SignatureInformation {
        label: signature_label,
        documentation: None,
        parameters: Some(params),
        active_parameter,
    };

    SignatureHelp {
        signatures: vec![signature],
        active_signature: Some(0),
        active_parameter,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use url::Url;

    const CALLS: &str = r#"use inner::helper

mod inner {
    pub fn helper(_ x: i32, _ flag: bool) -> i32 {
        x
    }
}

struct Point {
    x: i32,
    y: i32,
}

impl Point {
    pub fn new(_ x: i32, _ y: i32) -> Point {
        Point { x, y }
    }

    pub fn dot(self, _ other: Point) -> i32 {
        self.x * other.x + self.y * other.y
    }
}

trait Shape {
    fn area(self, _ scale: i32) -> i32
}

impl Shape for Point {
    fn area(self, _ scale: i32) -> i32 {
        self.x * self.y * scale
    }
}

fn plain(_ a: i32, _ b: i32) -> i32 {
    a
}

fn none() {}

fn f() {
    let p = Point::new(1, 2)
    let _ = plain(1, 2)
    let _ = plain(helper(3, true), 4)
    let _ = inner::helper(5, false)
    let _ = p.dot(p)
    let _ = Point::dot(p, p)
    let _ = p.area(6)
    let g = |x: i32| x
    let _ = g(7)
    none()
}

msg Token {
    #[selector = 0x01]
    Transfer { amount: u256 } -> bool,
}

pub contract Wallet {
    init() {
        let _ = plain(8, 9)
    }

    recv Token {
        Transfer { amount } -> bool {
            plain(10, 11) > 0
        }
    }
}
"#;

    /// The label and active parameter of the signature help at the end of
    /// the first `needle` in `code`.
    fn signature_after(code: &str, needle: &str) -> Option<(String, Option<u32>)> {
        let mut db = DriverDataBase::default();
        let file = db.workspace().touch(
            &mut db,
            Url::parse("file:///signature_help.fe").unwrap(),
            Some(code.to_string()),
        );
        let top_mod = map_file_to_mod(&db, file);
        let cursor = code.find(needle).unwrap() + needle.len();
        let help = find_signature_at_cursor(&db, top_mod, (cursor as u32).into())?;
        Some((help.signatures[0].label.clone(), help.active_parameter))
    }

    #[track_caller]
    fn check(code: &str, needle: &str, expected: Option<(&str, Option<u32>)>) {
        assert_eq!(
            signature_after(code, needle),
            expected.map(|(label, active)| (label.to_string(), active)),
            "after `{needle}`"
        );
    }

    #[test]
    fn signature_help_at_calls() {
        let plain = "fn plain(a: i32, b: i32) -> i32";
        let helper = "fn helper(x: i32, flag: bool) -> i32";
        let dot = "fn dot(self, other: Point) -> i32";
        for (needle, expected) in [
            ("plain(1, ", Some((plain, Some(1)))),
            ("helper(3, ", Some((helper, Some(1)))),
            ("true), ", Some((plain, Some(1)))),
            ("inner::helper(5, ", Some((helper, Some(1)))),
            (
                "Point::new(1, ",
                Some(("fn new(x: i32, y: i32) -> Point", Some(1))),
            ),
            // A method call's receiver is `self`; a path call passes it.
            ("p.dot(", Some((dot, Some(1)))),
            ("Point::dot(", Some((dot, Some(0)))),
            ("Point::dot(p, ", Some((dot, Some(1)))),
            (
                "p.area(",
                Some(("fn area(self, scale: i32) -> i32", Some(1))),
            ),
            ("    none(", Some(("fn none()", None))),
            // A call of a closure runs its `Fn` method, not a written signature.
            ("= g(", None),
            ("let _ = plain(8, ", Some((plain, Some(1)))),
            ("plain(10, ", Some((plain, Some(1)))),
        ] {
            check(CALLS, needle, expected);
        }
    }

    /// Signature help is for the call whose argument list holds the cursor.
    #[test]
    fn signature_help_in_argument_list() {
        let plain = "fn plain(a: i32, b: i32) -> i32";
        for (needle, expected) in [
            ("plain(he", Some((plain, Some(0)))),
            ("let _ = pla", None),
            ("let _ = plain(1, 2)", None),
        ] {
            check(CALLS, needle, expected);
        }
    }

    /// The active parameter counts the commas before the cursor, up to the
    /// last parameter.
    #[test]
    fn signature_help_active_parameter() {
        let plain = "fn plain(a: i32, b: i32) -> i32";
        let code = "fn plain(_ a: i32, _ b: i32) -> i32 {
    a
}

fn f() {
    let _ = plain(1, )
    let _ = plain(1, 2, 3)
}
";
        check(code, "plain(1, ", Some((plain, Some(1))));
        check(code, "plain(1, 2, ", Some((plain, Some(1))));
        // An argument list being typed isn't closed yet.
        for (unclosed, active) in [
            ("= plain(", 0),
            ("= plain(1, ", 1),
            ("= plain(plain(1, 2), ", 1),
        ] {
            let code = format!(
                "fn plain(_ a: i32, _ b: i32) -> i32 {{\n    a\n}}\n\nfn f() {{\n    let _ {unclosed}\n}}\n"
            );
            check(&code, unclosed, Some((plain, Some(active))));
        }
    }

    /// Only the argument list's own commas separate its arguments, and only
    /// its own `)` closes it: not those in a closure's parameters, a string,
    /// a nested call, a tuple or array, generic arguments or a comment.
    #[test]
    fn signature_help_nested_syntax() {
        let plain = "fn plain(a: i32, b: i32) -> i32";
        let apply = "fn apply(f: F, x: i32) -> i32";
        let code = r#"fn plain(_ a: i32, _ b: i32) -> i32 {
    a
}

fn apply<F>(_ f: F, _ x: i32) -> i32 {
    x
}

fn pick<T, U>(_ t: T, _ u: U) -> T {
    t
}

fn f() {
    let _ = apply(|a: i32, b: i32| a + b, 1)
    let _ = apply("a, b", 2)
    let _ = apply(plain(1, 2), 3)
    let _ = apply((1, 2), 4)
    let _ = apply([1, 2], 5)
    let _ = apply(pick<i32, bool>(1, true), 6)
    let _ = apply("a)", 7)
    let _ = apply( // )
        8,
        9
    )
}
"#;
        for (needle, expected) in [
            ("apply(|a: i32, ", (apply, 0)),
            ("| a + b", (apply, 0)),
            ("apply(\"a, b\"", (apply, 0)),
            ("apply(plain(1, ", (plain, 1)),
            ("apply(plain(1, 2)", (apply, 0)),
            ("apply((1, 2)", (apply, 0)),
            ("apply([1, 2]", (apply, 0)),
            ("apply(pick<i32, ", (apply, 0)),
            ("true), ", (apply, 1)),
            ("apply(\"a)\"", (apply, 0)),
            ("\"a)\", ", (apply, 1)),
            ("// )\n        ", (apply, 0)),
            ("8,\n        ", (apply, 1)),
        ] {
            check(code, needle, Some((expected.0, Some(expected.1))));
        }
    }

    /// A call written in a macro's arguments has signature help, but the
    /// calls the macro expands to don't, and neither has the macro's own
    /// argument list.
    #[test]
    fn signature_help_in_macros() {
        let plain = "fn plain(a: i32, b: i32) -> i32";
        let code = "fn plain(_ a: i32, _ b: i32) -> i32 {
    a
}

fn f() {
    let _ = format!(plain(1, 2))
    println!(plain(3, 4))
}
";
        for (needle, expected) in [
            ("format!(plain(1, ", Some((plain, Some(1)))),
            ("println!(plain(3, ", Some((plain, Some(1)))),
            ("format!(", None),
            ("println!(", None),
            ("format!(plain(1, 2)", None),
        ] {
            check(code, needle, expected);
        }
    }

    #[test]
    fn variadic_signature_ends_in_dots() {
        let code = r#"extern {
    fn printf(_ format: *u8, ...) -> i32
}

fn f(_ format: *u8, _ n: i32) {
    let _ = unsafe { printf(format, n, n) }
}
"#;
        let printf = "fn printf(format: *u8, ...) -> i32";
        // Each extra argument is shown at the `...`.
        for (needle, active) in [
            ("{ printf(", 0),
            ("{ printf(format, ", 1),
            ("{ printf(format, n, ", 1),
        ] {
            check(code, needle, Some((printf, Some(active))));
        }
    }
}
