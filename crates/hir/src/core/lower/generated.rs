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
/// items: the named implementations of scoped derives, and the frozen item
/// code of requests that name no header.
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
        if let Some(scoped) = &request.scoped_impl {
            lower_scoped_derive(ctxt, scoped, &request.code);
        } else {
            lower_generated_item_code(ctxt, &request.code);
        }
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

/// Lower the bounded function supplied by a scoped provider.
pub(super) fn lower_derive_method<'db>(ctxt: &mut FileLowerCtxt<'db>, code: &FrozenCode) {
    let FrozenCode::Template {
        kind: FrozenCodeKind::Item,
        quote_file,
        quote_range,
        producer_module,
        producer_block,
        holes,
    } = code
    else {
        ctxt.record_generation_error(
            parser::TextRange::empty(0.into()),
            "bounded derive requires one function template",
        );
        return;
    };
    let (quote, mut frame) = ctxt.resolve_quote_template(
        quote_file,
        *quote_range,
        *producer_module,
        *producer_block,
        false,
        holes.clone(),
    );
    frame.is_member_quote = true;
    let producer = frame.producer;
    // Admission covers the complete output, not just the children that happen
    // to cast to items. In particular, a sibling item hole must not disappear
    // before the bounded single-function check.
    let children = quote
        .item_list()
        .map(|list| list.syntax().children().collect::<Vec<_>>())
        .or_else(|| {
            quote
                .body()
                .map(|block| block.syntax().children().collect::<Vec<_>>())
        })
        .unwrap_or_default();
    let [child] = children.as_slice() else {
        ctxt.record_generation_error(
            *quote_range,
            "bounded derive requires one function template",
        );
        return;
    };
    let single_hole_range = ast::QuoteHole::cast(child.clone())
        .map(|hole| hole.syntax().text_range())
        .or_else(|| {
            let stmt = ast::Stmt::cast(child.clone())?;
            let ast::StmtKind::Expr(expr_stmt) = stmt.kind() else {
                return None;
            };
            let expr = expr_stmt.expr()?;
            let ast::ExprKind::QuoteHole(hole) = expr.kind() else {
                return None;
            };
            Some(hole.syntax().text_range())
        });
    if let Some(hole_range) = single_hole_range {
        ctxt.quote_frames.push(frame);
        let nested = ctxt.current_hole(hole_range);
        if let Some(FrozenHoleValue::Code(nested)) = nested {
            lower_derive_method(ctxt, &nested);
        } else {
            ctxt.record_generation_error(
                *quote_range,
                "bounded derive requires one function template",
            );
        }
        ctxt.pop_quote_frame();
        return;
    }
    let Some(function) = ast::Item::cast(child.clone()).and_then(|item| match item.kind()? {
        ast::ItemKind::Func(function) => Some(function),
        _ => None,
    }) else {
        ctxt.record_generation_error(
            *quote_range,
            "bounded derive requires one function template",
        );
        return;
    };
    ctxt.quote_frames.push(frame);
    super::item::validate_func_attrs(
        ctxt,
        function.attr_list(),
        super::item::FuncKind::Associated,
        function.sig().name().map(|name| name.text().to_string()),
    );
    let generated = Func::lower_ast(ctxt, function);
    ctxt.pop_quote_frame();
    ctxt.builder
        .defer_quoted_item_origin(generated.into(), producer);
}

/// Install a scoped output as a private named candidate, never an ordinary impl.
fn lower_scoped_derive<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    scoped: &common::generation::FrozenScopedImpl,
    code: &FrozenCode,
) {
    use crate::hir_def::{
        AttrListId, GenericArgListId, GenericParamListId, IdentId, ImplTraitModifiers, Partial,
        PathId, PathKind, TrackedItemVariant, TraitRefId, TypeId, TypeKind, Visibility,
        WhereClauseId,
    };
    let db = ctxt.db();
    let (trait_, trait_name) = ctxt.authored_trait(&scoped.demand.trait_ref);
    let trait_path = PathId::new(
        db,
        PathKind::BoundItem {
            item: trait_,
            display: PathId::from_ident(db, trait_name.to_opt().expect("named source declaration")),
        },
        None,
    );
    let trait_ref = TraitRefId::new(db, Partial::Present(trait_path));
    let index = ctxt.next_impl_trait_idx();
    let id = ctxt.joined_id(TrackedItemVariant::ImplTrait(index));
    ctxt.enter_item_scope(id, false);
    // Inside the implementation, so constant arguments are its bodies.
    let target_args = match &scoped.demand.target.args {
        Some(args) => GenericArgListId::thaw(ctxt, args),
        None => GenericArgListId::none(db),
    };
    let target = ctxt
        .authored_record(&scoped.demand.target)
        .path(db, target_args);
    let ty = TypeId::new(db, TypeKind::Path(Partial::Present(target)));
    lower_derive_method(ctxt, code);
    let implementation = ImplTrait::new(
        db,
        id,
        Partial::Present(trait_ref),
        Partial::Present(ty),
        AttrListId::new(db, vec![]),
        GenericParamListId::new(db, vec![]),
        WhereClauseId::new(db, vec![]),
        vec![],
        vec![],
        vec![],
        ImplTraitModifiers {
            is_unsafe: false,
            selection_name: Some(IdentId::new(db, scoped.selection_name.clone())),
            vis: Visibility::Private,
            derive_provider: None,
            quote_chain: None,
        },
        ctxt.top_mod(),
        crate::span::HirOrigin::synthetic(),
    );
    ctxt.leave_item_scope(implementation);
    // The header belongs to the authored opening; the method body independently
    // retains its quote producer's lexical origin.
    let root = super::top_mod_ast(db, ctxt.top_mod());
    let opening_node = root
        .syntax()
        .descendants()
        .find(|node| node.text_range() == scoped.demand.opening.binding_range)
        .expect("validated scoped opening belongs to unchanged authored source");
    let producer_module = opening_node
        .ancestors()
        .find_map(ast::Mod::cast)
        .map(|module| module.syntax().text_range());
    let producer_block = opening_node
        .ancestors()
        .find_map(ast::BlockExpr::cast)
        .map(|block| block.syntax().text_range());
    let producer = crate::hir_def::AuthoredScope::producer(
        db,
        ctxt.top_mod(),
        producer_module,
        producer_block,
    );
    ctxt.builder
        .defer_quoted_item_origin(implementation.into(), producer);
    let demand = crate::hir_def::authored::ScopedDeriveDemandId::new(db, scoped.demand.clone());
    ctxt.builder
        .record_scoped_implementation(demand, implementation);
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
