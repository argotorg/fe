//! Output HIR is built from the original checked quote syntax, never from an
//! emitted source string. Provider evaluation has completed before this query.
use super::FileLowerCtxt;
use crate::hir_def::Func;
use parser::ast::{self, prelude::*};

pub(super) fn append_functions<'db>(ctxt: &mut FileLowerCtxt<'db>) {
    let db = ctxt.db();
    let top_mod = ctxt.top_mod();
    let Some(url) = top_mod.file(db).url(db) else {
        return;
    };
    let plans = db
        .generation_inputs()
        .functions(db)
        .iter()
        .filter(|plan| plan.target_file == url)
        .cloned()
        .collect::<Vec<_>>();
    let root = super::top_mod_ast(db, top_mod);
    for plan in plans {
        let quote = root
            .syntax()
            .descendants()
            .filter_map(ast::QuoteExpr::cast)
            .find(|quote| quote.syntax().text_range() == plan.quote_range)
            .expect("validated frozen quote belongs to unchanged input source");
        let holes = crate::core::meta::quote_holes(&quote)
            .expect("checked frozen quote retains valid bounded syntax")
            .into_iter()
            .map(|hole| hole.syntax().text_range())
            .zip(plan.holes)
            .collect();
        ctxt.generation_holes = holes;
        ctxt.lowering_generated_function = true;
        let ast::ItemKind::Func(function) = quote
            .body()
            .unwrap()
            .items()
            .next()
            .unwrap()
            .kind()
            .unwrap()
        else {
            unreachable!("validated one-function template")
        };
        super::item::validate_func_attrs(
            ctxt,
            function.attr_list(),
            super::item::FuncKind::Free,
            function.sig().name().map(|name| name.text().to_string()),
        );
        let generated = Func::lower_ast(ctxt, function);
        ctxt.generation_holes.clear();
        ctxt.lowering_generated_function = false;
        ctxt.builder.set_quoted_function_origin(
            generated,
            plan.producer_module,
            plan.producer_block,
        );
    }
}
