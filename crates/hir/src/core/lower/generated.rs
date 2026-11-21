//! Output HIR is built from the original checked quote syntax, never from an
//! emitted source string. Provider evaluation has completed before this query.
use super::FileLowerCtxt;
use crate::hir_def::{Const, Func, Impl, ImplTrait, ItemKind, QuotedPathOrigin, TopLevelMod};
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
    /// Where the quotation was written; its free names resolve there.
    pub(super) producer: crate::hir_def::AuthoredScope<'db>,
    pub(super) is_sub_quote: bool,
    /// Members keep their public/local binder names but resolve free names in
    /// the spliced template's producer, not the containing quote's producer.
    pub(super) is_member_quote: bool,
    pub(super) open_names: Vec<(String, parser::TextRange)>,
    pub(super) bound_names: rustc_hash::FxHashSet<String>,
    pub(super) holes: Vec<(parser::TextRange, FrozenHoleValue)>,
}

impl<'db> ActiveQuoteFrame<'db> {
    pub(super) fn path_origin(&self) -> QuotedPathOrigin<'db> {
        QuotedPathOrigin {
            producer: self.producer,
            range: self.quote_range,
            frame_id: self.frame_id,
        }
    }
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
        let producer = crate::hir_def::AuthoredScope::producer(
            db,
            top_mod,
            plan.producer_module,
            plan.producer_block,
        );
        let frame_id = ctxt.next_quote_frame_id();
        ctxt.quote_frames.push(ActiveQuoteFrame {
            frame_id,
            quote_top_mod: top_mod,
            quote_range: plan.quote_range,
            producer,
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
        ctxt.builder
            .set_quoted_item_origin(generated.into(), producer);
    }

    append_module_output(ctxt);
}

/// Lower the outputs that join the file's root module after its authored
/// items, from frozen item code that names no derive header.
pub(super) fn append_module_output<'db>(ctxt: &mut FileLowerCtxt<'db>) {
    let db = ctxt.db();
    let Some(url) = ctxt.top_mod().file(db).url(db) else {
        return;
    };
    let requests = db
        .generation_inputs()
        .requests(db)
        .iter()
        .filter(|request| request.target_file == url && request.request_impl.is_none())
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
        ast::ItemKind::TypeAlias(_) => {
            ctxt.record_generation_error(
                item.syntax().text_range(),
                "a quoted type is an associated type member; generated module type aliases are not supported",
            );
            None
        }
        _ => {
            ctxt.record_generation_error(
                item.syntax().text_range(),
                "item quotation supports function, const, impl, and trait impl declarations",
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
            let producer = frame.producer;
            ctxt.quote_frames.push(frame);
            if let Some(item_list) = quote.item_list() {
                for child in item_list.syntax().children() {
                    if let Some(item) = ast::Item::cast(child.clone()) {
                        if let Some(lowered) = lower_quoted_ast_item(ctxt, item) {
                            ctxt.builder.set_quoted_item_origin(lowered, producer);
                        }
                    } else if ast::TraitShapeItem::can_cast(child.kind()) {
                        ctxt.record_generation_error(
                            child.text_range(),
                            "a quoted shape is an associated shape member; modules have no shape items",
                        );
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
                        ctxt.builder.set_quoted_item_origin(lowered, producer);
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

/// A member written in a member quotation: an item, or an associated shape,
/// which has no module item form.
pub(super) enum QuotedMember {
    Item(ast::Item),
    Shape(ast::TraitShapeItem),
}

impl QuotedMember {
    /// The item form of this member; a shape has none.
    pub(super) fn item_kind(&self) -> Option<ast::ItemKind> {
        match self {
            Self::Item(item) => item.kind(),
            Self::Shape(_) => None,
        }
    }

    pub(super) fn syntax(&self) -> &parser::SyntaxNode {
        match self {
            Self::Item(item) => item.syntax(),
            Self::Shape(shape) => shape.syntax(),
        }
    }
}

/// Lower member code into the already-entered destination declaration, in
/// order. Each template resolves its free names in its own producer; nested
/// `Code<Item>` holes splice their members in place; any other block statement
/// is reported rather than dropped.
fn lower_member_code<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    value: &FrozenCode,
    lower: &mut dyn FnMut(&mut FileLowerCtxt<'db>, QuotedMember),
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
                        lower(ctxt, QuotedMember::Item(item));
                    } else if let Some(shape) = ast::TraitShapeItem::cast(child.clone()) {
                        lower(ctxt, QuotedMember::Shape(shape));
                    } else if let Some(hole) = ast::QuoteHole::cast(child) {
                        lower_member_hole(ctxt, hole, lower);
                    }
                }
            } else if let Some(block) = quote.body() {
                for child in block.syntax().children() {
                    if let Some(item) = ast::Item::cast(child.clone()) {
                        lower(ctxt, QuotedMember::Item(item));
                    } else if let Some(stmt) = ast::Stmt::cast(child)
                        && let ast::StmtKind::Expr(expr) = stmt.kind()
                        && let Some(expr) = expr.expr()
                        && let ast::ExprKind::QuoteHole(hole) = expr.kind()
                        && let Some(FrozenHoleValue::Code(nested)) =
                            ctxt.current_hole(hole.syntax().text_range())
                    {
                        lower_member_code(ctxt, &nested, lower);
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

/// Lower an explicit derive's complete output as members of its authored
/// header. The header owns the implementation's identity, binders and
/// requirements; the provider supplies every member, which ordinary impl
/// conformance then admits or rejects as a whole, as for a handwritten impl.
pub(super) fn lower_derive_members<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    code: &FrozenCode,
    lower: &mut dyn FnMut(&mut FileLowerCtxt<'db>, QuotedMember),
) {
    if !matches!(
        code,
        FrozenCode::Template {
            kind: FrozenCodeKind::Item,
            ..
        } | FrozenCode::Empty {
            kind: FrozenCodeKind::Item
        }
    ) {
        ctxt.record_generation_error(
            parser::TextRange::empty(0.into()),
            "a derived implementation requires Code<Item> members",
        );
        return;
    }
    lower_member_code(ctxt, code, lower);
}

/// Copy members into the already-entered destination declaration. A nested
/// template owns its source/hole frame, but never changes the destination scope.
pub(super) fn lower_member_hole<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    hole: ast::QuoteHole,
    lower: &mut dyn FnMut(&mut FileLowerCtxt<'db>, QuotedMember),
) {
    if let Some(FrozenHoleValue::Code(value)) = ctxt.current_hole(hole.syntax().text_range()) {
        lower_member_code(ctxt, &value, lower);
    } else {
        ctxt.record_generation_error(
            hole.syntax().text_range(),
            "member quotation hole requires Code<Item>",
        );
    }
}
