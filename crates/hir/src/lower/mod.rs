use common::{InputFile, InputIngot};
use num_bigint::BigUint;
use num_traits::Num;
use parser::{
    ast::{self, prelude::*},
    SyntaxNode, SyntaxToken,
};

use crate::{
    hir_def::{
        module_tree_impl, scope_graph::ScopeGraph, ExprId, IdentId, IngotId, IntegerId, ItemKind,
        LitKind, ModuleTree, Partial, StringId, TopLevelMod, TrackedItemId,
    },
    HirDb, LowerHirDb,
};

use self::{item::lower_module_items, parse::parse_file_impl, scope_builder::ScopeGraphBuilder};

pub(crate) mod parse;

mod attr;
mod body;
mod expr;
mod item;
mod params;
mod pat;
mod path;
mod scope_builder;
mod stmt;
mod types;
mod use_tree;

/// Maps the given file to a top-level module.
/// This function just maps the file to a top-level module, and doesn't perform
/// any parsing or lowering.
/// To perform the actual lowering, use [`scope_graph`] instead.
pub fn map_file_to_mod(db: &dyn LowerHirDb, file: InputFile) -> TopLevelMod {
    let ingot = module_tree_impl(db.as_hir_db(), file.ingot(db.as_input_db())).ingot;
    map_file_to_mod_impl(db.as_hir_db(), ingot, file)
}

/// Returns the scope graph of the given top-level module.
pub fn scope_graph(db: &dyn LowerHirDb, top_mod: TopLevelMod) -> &ScopeGraph {
    scope_graph_impl(db.as_hir_db(), top_mod)
}

/// Returns the ingot module tree of the given ingot.
pub fn module_tree(db: &dyn LowerHirDb, ingot: InputIngot) -> &ModuleTree {
    module_tree_impl(db.as_hir_db(), ingot)
}

#[salsa::tracked]
pub(crate) fn map_file_to_mod_impl(db: &dyn HirDb, ingot: IngotId, file: InputFile) -> TopLevelMod {
    let path = file.path(db.as_input_db());
    let name = path.file_stem().unwrap();
    let mod_name = IdentId::new(db, name.to_string());
    TopLevelMod::new(db, mod_name, ingot, file)
}

#[salsa::tracked(return_ref)]
pub(crate) fn scope_graph_impl(db: &dyn HirDb, top_mod: TopLevelMod) -> ScopeGraph {
    let ast = top_mod_ast(db, top_mod);
    let mut ctxt = FileLowerCtxt::enter_top_mod(db, top_mod);

    if let Some(items) = ast.items() {
        lower_module_items(&mut ctxt, items);
    }
    ctxt.leave_item_scope(top_mod);

    ctxt.build()
}

pub(crate) fn top_mod_ast(db: &dyn HirDb, top_mod: TopLevelMod) -> ast::Root {
    let node = SyntaxNode::new_root(parse_file_impl(db, top_mod));
    // This cast never fails even if the file content is empty.
    ast::Root::cast(node).unwrap()
}

pub(super) struct FileLowerCtxt<'db> {
    builder: ScopeGraphBuilder<'db>,
}

impl<'db> FileLowerCtxt<'db> {
    pub(super) fn enter_top_mod(db: &'db dyn HirDb, top_mod: TopLevelMod) -> Self {
        Self {
            builder: ScopeGraphBuilder::enter_top_mod(db, top_mod),
        }
    }

    pub(super) fn build(self) -> ScopeGraph {
        self.builder.build()
    }

    pub(super) fn db(&self) -> &'db dyn HirDb {
        self.builder.db
    }

    pub(super) fn top_mod(&self) -> TopLevelMod {
        self.builder.top_mod
    }

    pub(super) fn enter_block_scope(&mut self) {
        self.builder.enter_block_scope();
    }

    pub(super) fn leave_block_scope(&mut self, block: ExprId) {
        self.builder.leave_block_scope(block);
    }

    pub(super) fn joined_id(&self, id: TrackedItemId) -> TrackedItemId {
        self.builder.joined_id(id)
    }

    /// Creates a new scope for an item.
    fn enter_item_scope(&mut self, id: TrackedItemId, is_mod: bool) {
        self.builder.enter_item_scope(id, is_mod);
    }

    fn enter_body_scope(&mut self, id: TrackedItemId) {
        self.builder.enter_body_scope(id);
    }

    /// Leaves the current scope, `item` should be the generated item which owns
    /// the scope.
    fn leave_item_scope<I>(&mut self, item: I) -> I
    where
        I: Into<ItemKind> + Copy,
    {
        self.builder.leave_item_scope(item.into());
        item
    }
}

impl IdentId {
    fn lower_token(ctxt: &mut FileLowerCtxt<'_>, token: SyntaxToken) -> Self {
        Self::new(ctxt.db(), token.text().to_string())
    }

    fn lower_token_partial(
        ctxt: &mut FileLowerCtxt<'_>,
        token: Option<SyntaxToken>,
    ) -> Partial<Self> {
        token.map(|token| Self::lower_token(ctxt, token)).into()
    }
}

impl LitKind {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::Lit) -> Self {
        match ast.kind() {
            ast::LitKind::Int(int) => Self::Int(IntegerId::lower_ast(ctxt, int)),
            ast::LitKind::String(string) => {
                let text = string.token().text();
                Self::String(StringId::new(
                    ctxt.db(),
                    text[1..text.len() - 1].to_string(),
                ))
            }
            ast::LitKind::Bool(bool) => match bool.token().text() {
                "true" => Self::Bool(true),
                "false" => Self::Bool(false),
                _ => unreachable!(),
            },
        }
    }
}

impl IntegerId {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'_>, ast: ast::LitInt) -> Self {
        let text = ast.token().text();
        // Parser ensures that the text is valid pair with a radix and a number.
        if text.len() < 2 {
            return Self::new(ctxt.db(), BigUint::from_str_radix(text, 10).unwrap());
        }

        let int = match &text[0..2] {
            "0x" | "0X" => BigUint::from_str_radix(&text[2..], 16).unwrap(),
            "0o" | "0O" => BigUint::from_str_radix(&text[2..], 8).unwrap(),
            "0b" | "0B" => BigUint::from_str_radix(&text[2..], 2).unwrap(),
            _ => BigUint::from_str_radix(text, 10).unwrap(),
        };

        Self::new(ctxt.db(), int)
    }
}
