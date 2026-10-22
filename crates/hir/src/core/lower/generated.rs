//! Output HIR is built from the original checked quote syntax, never from an
//! emitted source string. Provider evaluation has completed before this query.
use super::FileLowerCtxt;
use crate::hir_def::{Const, Func, Impl, ImplTrait, ItemKind, TopLevelMod};
use common::generation::{FrozenCode, FrozenCodeKind, FrozenHoleValue};
use parser::ast::{self, prelude::*};

#[derive(Debug, Clone, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub struct GenerationLoweringError {
    pub file: common::file::File,
    pub primary_range: parser::TextRange,
    pub message: String,
}

pub(crate) struct ActiveQuoteFrame<'db> {
    pub(super) frame_id: usize,
    pub(super) quote_top_mod: TopLevelMod<'db>,
    pub(super) quote_range: parser::TextRange,
    pub(super) producer_scope: crate::hir_def::scope_graph::ScopeId<'db>,
    pub(super) is_sub_quote: bool,
    /// Members keep their public/local binder names but resolve free names in
    /// the spliced template's producer, not the containing quote's producer.
    pub(super) is_member_quote: bool,
    pub(super) open_names: Vec<(String, parser::TextRange)>,
    pub(super) bound_names: rustc_hash::FxHashSet<String>,
    pub(super) holes: Vec<(parser::TextRange, FrozenHoleValue)>,
}

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
            .zip(plan.holes.into_iter().map(FrozenHoleValue::Scalar))
            .collect();
        let open_names = quote
            .open_list()
            .map(|list| {
                list.names()
                    .map(|n| (n.text().to_string(), n.text_range()))
                    .collect()
            })
            .unwrap_or_default();
        let producer_scope =
            ctxt.builder
                .resolve_producer_scope(top_mod, plan.producer_module, plan.producer_block);
        let frame_id = ctxt.next_quote_frame_id();
        ctxt.quote_frames.push(ActiveQuoteFrame {
            frame_id,
            quote_top_mod: top_mod,
            quote_range: plan.quote_range,
            producer_scope,
            is_sub_quote: false,
            is_member_quote: false,
            open_names,
            bound_names: rustc_hash::FxHashSet::default(),
            holes,
        });
        let function = if let Some(items) = quote.item_list() {
            match items.into_iter().next().unwrap().kind().unwrap() {
                ast::ItemKind::Func(function) => function,
                _ => unreachable!("validated one-function template"),
            }
        } else {
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
            function
        };
        super::item::validate_func_attrs(
            ctxt,
            function.attr_list(),
            super::item::FuncKind::Free,
            function.sig().name().map(|name| name.text().to_string()),
        );
        let generated = Func::lower_ast(ctxt, function);
        ctxt.pop_quote_frame();
        ctxt.builder.set_quoted_function_origin(
            generated,
            plan.producer_module,
            plan.producer_block,
        );
    }

    let requests = db
        .generation_inputs()
        .requests(db)
        .iter()
        .filter(|request| request.target_file == url)
        .cloned()
        .collect::<Vec<_>>();
    for request in requests {
        lower_generated_item_code(ctxt, &request.code);
    }
}

fn lower_quoted_ast_item<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    item: ast::Item,
) -> Option<ItemKind<'db>> {
    match item.kind()? {
        ast::ItemKind::Func(func) => {
            super::item::validate_func_attrs(
                ctxt,
                func.attr_list(),
                super::item::FuncKind::Free,
                func.sig().name().map(|name| name.text().to_string()),
            );
            Some(Func::lower_ast(ctxt, func).into())
        }
        ast::ItemKind::Const(const_) => {
            super::item::validate_unsupported_item_attrs(
                ctxt,
                const_.attr_list(),
                "const",
                const_.name().map(|name| name.text().to_string()),
            );
            Some(Const::lower_ast(ctxt, const_).into())
        }
        ast::ItemKind::Impl(impl_) => {
            super::item::validate_unsupported_item_attrs(ctxt, impl_.attr_list(), "impl", None);
            Some(Impl::lower_ast(ctxt, impl_).into())
        }
        ast::ItemKind::ImplTrait(impl_trait) => {
            super::item::validate_unsupported_item_attrs(
                ctxt,
                impl_trait.attr_list(),
                "impl trait",
                None,
            );
            Some(ImplTrait::lower_ast(ctxt, impl_trait).into())
        }
        _ => {
            ctxt.record_generation_error(
                item.syntax().text_range(),
                "item quotation supports function, constant, impl, and trait impl declarations",
            );
            None
        }
    }
}

pub(super) fn lower_generated_item_code<'db>(ctxt: &mut FileLowerCtxt<'db>, code: &FrozenCode) {
    match code {
        FrozenCode::Empty { .. } => {}
        FrozenCode::Template {
            kind: FrozenCodeKind::Item,
            quote_file,
            quote_range,
            producer_module,
            producer_block,
            holes,
        } => {
            let (quote, frame) = ctxt.resolve_quote_template(
                quote_file,
                *quote_range,
                *producer_module,
                *producer_block,
                false,
                holes.clone(),
            );
            let quote_top_mod = frame.quote_top_mod;
            ctxt.quote_frames.push(frame);
            if let Some(item_list) = quote.item_list() {
                for child in item_list.syntax().children() {
                    if let Some(item) = ast::Item::cast(child.clone()) {
                        if let Some(lowered) = lower_quoted_ast_item(ctxt, item) {
                            ctxt.builder.set_quoted_item_origin(
                                lowered,
                                quote_top_mod,
                                *producer_module,
                                *producer_block,
                            );
                        }
                    } else if let Some(hole) = ast::QuoteHole::cast(child) {
                        if let Some(FrozenHoleValue::Code(nested)) =
                            ctxt.current_hole(hole.syntax().text_range())
                        {
                            lower_generated_item_code(ctxt, &nested);
                        } else {
                            ctxt.record_generation_error(
                                hole.syntax().text_range(),
                                "item-level quotation hole requires Code<Item> splice",
                            );
                        }
                    }
                }
            } else if let Some(block) = quote.body() {
                for item in block.items() {
                    if let Some(lowered) = lower_quoted_ast_item(ctxt, item) {
                        ctxt.builder.set_quoted_item_origin(
                            lowered,
                            quote_top_mod,
                            *producer_module,
                            *producer_block,
                        );
                    }
                }
                for stmt in block.stmts() {
                    if let ast::StmtKind::Expr(expr_stmt) = stmt.kind()
                        && let Some(expr) = expr_stmt.expr()
                        && let ast::ExprKind::QuoteHole(hole) = expr.kind()
                    {
                        if let Some(FrozenHoleValue::Code(nested)) =
                            ctxt.current_hole(hole.syntax().text_range())
                        {
                            lower_generated_item_code(ctxt, &nested);
                        } else {
                            ctxt.record_generation_error(
                                hole.syntax().text_range(),
                                "item-level quotation hole requires Code<Item> splice",
                            );
                        }
                    } else {
                        ctxt.record_generation_error(
                            stmt.syntax().text_range(),
                            "item quotation requires item declarations",
                        );
                    }
                }
            } else {
                ctxt.record_generation_error(
                    *quote_range,
                    "item quotation requires item declarations",
                );
            }
            ctxt.pop_quote_frame();
        }
        _ => {
            let range = ctxt
                .quote_frames
                .last()
                .map_or_else(|| parser::TextRange::empty(0.into()), |f| f.quote_range);
            ctxt.record_generation_error(range, "item-level quotation splice requires Code<Item>");
        }
    }
}

/// Copy members into the already-entered destination declaration. A nested
/// template owns its source/hole frame, but never changes the destination scope.
pub(super) fn lower_member_hole<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    hole: ast::QuoteHole,
    lower: &mut dyn FnMut(&mut FileLowerCtxt<'db>, ast::Item),
) {
    fn code<'db>(
        ctxt: &mut FileLowerCtxt<'db>,
        value: &FrozenCode,
        lower: &mut dyn FnMut(&mut FileLowerCtxt<'db>, ast::Item),
    ) {
        match value {
            FrozenCode::Empty {
                kind: FrozenCodeKind::Item,
            } => {}
            FrozenCode::Template {
                kind: FrozenCodeKind::Item,
                quote_file,
                quote_range,
                producer_module,
                producer_block,
                holes,
            } => {
                let (quote, mut frame) = ctxt.resolve_quote_template(
                    quote_file,
                    *quote_range,
                    *producer_module,
                    *producer_block,
                    false,
                    holes.clone(),
                );
                frame.is_member_quote = true;
                ctxt.quote_frames.push(frame);
                if let Some(items) = quote.item_list() {
                    for child in items.syntax().children() {
                        if let Some(item) = ast::Item::cast(child.clone()) {
                            lower(ctxt, item);
                        } else if let Some(hole) = ast::QuoteHole::cast(child) {
                            lower_member_hole(ctxt, hole, lower);
                        }
                    }
                } else if let Some(block) = quote.body() {
                    for child in block.syntax().children() {
                        if let Some(item) = ast::Item::cast(child.clone()) {
                            lower(ctxt, item);
                        } else if let Some(stmt) = ast::Stmt::cast(child)
                            && let ast::StmtKind::Expr(expr) = stmt.kind()
                            && let Some(expr) = expr.expr()
                            && let ast::ExprKind::QuoteHole(hole) = expr.kind()
                            && let Some(FrozenHoleValue::Code(nested)) =
                                ctxt.current_hole(hole.syntax().text_range())
                        {
                            code(ctxt, &nested, lower);
                        } else {
                            ctxt.record_generation_error(
                                *quote_range,
                                "member quotation requires function or constant declarations",
                            );
                        }
                    }
                }
                ctxt.pop_quote_frame();
            }
            _ => ctxt.record_generation_error(
                ctxt.quote_frames
                    .last()
                    .map_or_else(|| parser::TextRange::empty(0.into()), |f| f.quote_range),
                "member quotation requires Code<Item>",
            ),
        }
    }
    if let Some(FrozenHoleValue::Code(value)) = ctxt.current_hole(hole.syntax().text_range()) {
        code(ctxt, &value, lower);
    } else {
        ctxt.record_generation_error(
            hole.syntax().text_range(),
            "member quotation hole requires Code<Item>",
        );
    }
}
