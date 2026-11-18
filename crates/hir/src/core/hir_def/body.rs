// This is necessary because `salsa::tracked` structs generates a
// constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use std::hash::Hash;

use cranelift_entity::{EntityRef, PrimaryMap, SecondaryMap};
use parser::ast::{self, prelude::*};
use rustc_hash::FxHashMap;
use salsa::SalsaValue;

use super::{
    Cond, CondId, Expr, ExprId, Func, ItemKind, Partial, Pat, PatId, Stmt, StmtId, TopLevelMod,
    TrackedItemId,
    scope_graph::{ScopeGraph, ScopeId},
};
use crate::{
    HirDb,
    lower::func_body_graph_impl,
    span::{HirOrigin, item::LazyBodySpan},
    visitor::prelude::*,
};

#[salsa::tracked]
#[derive(Debug, PartialOrd, Ord)]
pub struct Body<'db> {
    //    #[id]
    #[returns(copy)]
    id: TrackedItemId<'db>,

    /// The expression that evaluates to the value of the body.
    /// In case of a function body, this is always be the block expression.
    #[returns(copy)]
    pub expr: ExprId,

    #[returns(copy)]
    pub body_kind: BodyKind,

    pub stmts: NodeStore<StmtId, Partial<Stmt<'db>>>,
    pub exprs: NodeStore<ExprId, Partial<Expr<'db>>>,
    pub conds: NodeStore<CondId, Partial<Cond>>,
    pub pats: NodeStore<PatId, Partial<Pat<'db>>>,
    #[returns(copy)]
    pub top_mod: TopLevelMod<'db>,
    /// The function whose lazily lowered body contains this body, and whose
    /// body scope graph holds this body's scopes. `None` for a body lowered
    /// with its module.
    #[returns(copy)]
    pub(crate) lazy_func: Option<Func<'db>>,
    /// The module whose syntax `source_map` and `origin` point into, and the
    /// quotations that wrote and placed the code of this body.
    pub quote_origins: QuoteOrigins<'db>,

    pub(crate) source_map: BodySourceMap,
    pub(crate) origin: HirOrigin<ast::Expr>,
}

/// Where the syntax of a body came from, and which quotations wrote and placed
/// it. Generated code is written by the package of the quotation that holds its
/// text. A fragment that another quotation splices keeps its own writer, and
/// each quotation around it placed the fragment into the code that runs.
///
/// A chain lists the modules of the quotations active where code was lowered,
/// innermost first: the writer, then the placers with the outermost last. The
/// chain of hand-written code is empty, because its writer is the module that
/// holds it.
#[derive(Clone, Debug, PartialEq, Eq, Hash, SalsaValue)]
pub struct QuoteOrigins<'db> {
    /// The module whose syntax the body's source map and origin point into:
    /// the module that holds an authored body, the producer's module for a
    /// generated one.
    pub syntax_top_mod: TopLevelMod<'db>,
    /// The chain where the body was built, shared by every expression that is
    /// not part of a spliced fragment.
    pub frames: Vec<TopLevelMod<'db>>,
    /// The chain of each expression lowered inside a spliced fragment, by
    /// increasing expression.
    pub spliced: Vec<(ExprId, Vec<TopLevelMod<'db>>)>,
}

impl<'db> QuoteOrigins<'db> {
    /// The chain of `expr`: empty for hand-written code.
    pub fn chain(&self, expr: ExprId) -> &[TopLevelMod<'db>] {
        self.spliced
            .binary_search_by_key(&expr.index(), |(spliced, _)| spliced.index())
            .map_or(&self.frames, |index| &self.spliced[index].1)
    }
}

/// The quotations that wrote and placed an item, innermost first (see
/// `QuoteOrigins`).
#[salsa::interned]
#[derive(Debug)]
pub struct QuoteChain<'db> {
    pub modules: Vec<TopLevelMod<'db>>,
}

impl<'db> Body<'db> {
    pub fn span(self) -> LazyBodySpan<'db> {
        LazyBodySpan::new(self)
    }

    /// The module whose syntax `source_map` and `origin` point into: `top_mod`
    /// for authored bodies, the producer's module for generated ones.
    pub fn syntax_top_mod(self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        self.quote_origins(db).syntax_top_mod
    }

    /// The modules of the quotations that wrote and placed `expr`, writer
    /// first and outermost placer last; the module that holds the syntax of
    /// hand-written code.
    pub fn expr_quote_chain(self, db: &'db dyn HirDb, expr: ExprId) -> Vec<TopLevelMod<'db>> {
        match self.quote_origins(db).chain(expr) {
            [] => vec![self.syntax_top_mod(db)],
            chain => chain.to_vec(),
        }
    }

    /// The modules of the quotations that wrote and placed this body as a
    /// whole (an `unsafe fn` is unsafe code from its signature), writer first.
    pub fn quote_chain(self, db: &'db dyn HirDb) -> Vec<TopLevelMod<'db>> {
        match &self.quote_origins(db).frames[..] {
            [] => vec![self.syntax_top_mod(db)],
            chain => chain.to_vec(),
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    /// Returns the scope graph that holds this body's scopes.
    pub fn scope_graph(self, db: &'db dyn HirDb) -> &'db ScopeGraph<'db> {
        match self.lazy_func(db) {
            Some(func) => func_body_graph_impl(db, func)
                .as_ref()
                .expect("a lazily lowered body has a scope graph"),
            None => self.top_mod(db).scope_graph(db),
        }
    }

    /// Find the function that contains this body.
    ///
    /// Returns `Some(func)` if this body belongs to a function, `None` otherwise
    /// (e.g., for anonymous/closure bodies or const bodies).
    pub fn containing_func(self, db: &'db dyn HirDb) -> Option<Func<'db>> {
        // A function body is lowered inside its function's item scope, so the
        // function is the body's lexical parent.
        match self.scope().parent(db) {
            Some(ScopeId::Item(ItemKind::Func(func))) if func.body(db) == Some(self) => Some(func),
            _ => None,
        }
    }

    /// Whether this body is a side of a static assertion's comparison: an
    /// operand of the condition lowered a second time, so that it can be
    /// evaluated on its own for the failure message. Its source is part of
    /// the condition's.
    pub fn is_static_assert_comparison_side(self, db: &'db dyn HirDb) -> bool {
        matches!(
            self.scope().parent(db),
            Some(ScopeId::Item(ItemKind::StaticAssert(assert_)))
                if assert_
                    .comparison(db)
                    .is_some_and(|comparison| comparison.lhs == self || comparison.rhs == self)
        )
    }

    #[doc(hidden)]
    /// Returns the order of the blocks in the body in lexical order.
    /// e.g.,
    /// ```fe
    /// fn foo() { // 0
    ///     ...
    ///     { // 1
    ///         ...
    ///         { // 2
    ///             ...
    ///         }
    ///     }
    /// }
    ///
    ///
    /// Currently, this is only used for testing.
    /// When it turns out to be generally useful, we need to consider to let
    /// salsa track this method.
    pub fn iter_block(self, db: &dyn HirDb) -> FxHashMap<ExprId, usize> {
        BlockOrderCalculator::new(db, self).calculate()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BodyKind {
    FuncBody,
    Anonymous,
}

#[derive(Debug, Hash, Clone, PartialEq, Eq, SalsaValue)]
pub struct NodeStore<K, V>(
    // SAFETY: A `PrimaryMap` owns its values; its keys are plain indices.
    #[salsa_value(unsafe(prove(V: SalsaValue)))] PrimaryMap<K, V>,
)
where
    K: EntityRef;

impl<K, V> NodeStore<K, V>
where
    K: EntityRef,
{
    pub fn new() -> Self {
        Self(PrimaryMap::new())
    }
}
impl<K, V> Default for NodeStore<K, V>
where
    K: EntityRef,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> std::ops::Deref for NodeStore<K, V>
where
    K: EntityRef,
{
    type Target = PrimaryMap<K, V>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K, V> std::ops::DerefMut for NodeStore<K, V>
where
    K: EntityRef,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<K, V> std::ops::Index<K> for NodeStore<K, V>
where
    K: EntityRef,
{
    type Output = V;

    fn index(&self, k: K) -> &V {
        &self.0[k]
    }
}

/// Mutable indexing into an `PrimaryMap`.
impl<K, V> std::ops::IndexMut<K> for NodeStore<K, V>
where
    K: EntityRef,
{
    fn index_mut(&mut self, k: K) -> &mut V {
        &mut self.0[k]
    }
}

pub trait SourceAst: AstNode + Clone + Hash + PartialEq + Eq {}
impl<T> SourceAst for T where T: AstNode + Clone + Hash + PartialEq + Eq {}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct BodySourceMap {
    pub stmt_map: SourceNodeMap<ast::Stmt, StmtId>,
    pub expr_map: SourceNodeMap<ast::Expr, ExprId>,
    pub pat_map: SourceNodeMap<ast::Pat, PatId>,
    pub quote_mods: Vec<u16>,
    /// The open names of spliced quotes that only bare pattern names bind.
    pub open_name_binders: Vec<OpenNameBinders>,
}

/// An open name of a quote spliced into the body, bound only by the bare
/// pattern names `pats`, innermost first. A bare name binds only if it names no
/// constant or unit variant, which type checking decides; if none of `pats`
/// binds, the open name has no enclosing binder.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct OpenNameBinders {
    pub name: String,
    pub pats: Vec<PatId>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    pub node_to_source: SecondaryMap<Node, HirOrigin<Ast>>,
    pub node_to_quote_mod: SecondaryMap<Node, Option<u16>>,
}

impl<Ast, Node> SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    pub(crate) fn insert(&mut self, node: Node, ast: HirOrigin<Ast>) {
        self.insert_with_quote_mod(node, ast, None);
    }

    pub(crate) fn insert_with_quote_mod(
        &mut self,
        node: Node,
        ast: HirOrigin<Ast>,
        quote_mod: Option<u16>,
    ) {
        self.node_to_source[node] = ast;
        self.node_to_quote_mod[node] = quote_mod;
    }

    pub(crate) fn node_to_source(&self, node: Node) -> &HirOrigin<Ast> {
        &self.node_to_source[node]
    }

    pub(crate) fn node_to_quote_mod(&self, node: Node) -> Option<u16> {
        self.node_to_quote_mod[node]
    }
}

impl<Ast, Node> Default for SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    fn default() -> Self {
        Self {
            node_to_source: SecondaryMap::new(),
            node_to_quote_mod: SecondaryMap::new(),
        }
    }
}

struct BlockOrderCalculator<'db> {
    db: &'db dyn HirDb,
    order: FxHashMap<ExprId, usize>,
    body: Body<'db>,
    fresh_number: usize,
}

impl<'db> Visitor<'db> for BlockOrderCalculator<'db> {
    fn visit_expr(
        &mut self,
        ctxt: &mut crate::visitor::VisitorCtxt<'db, crate::span::expr::LazyExprSpan<'db>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        if ctxt.body() == self.body && matches!(expr_data, Expr::Block(..)) {
            self.order.insert(expr, self.fresh_number);
            self.fresh_number += 1;
        }

        walk_expr(self, ctxt, expr)
    }
}

impl<'db> BlockOrderCalculator<'db> {
    fn new(db: &'db dyn HirDb, body: Body<'db>) -> Self {
        Self {
            db,
            order: FxHashMap::default(),
            body,
            fresh_number: 0,
        }
    }

    fn calculate(mut self) -> FxHashMap<ExprId, usize> {
        let expr = self.body.expr(self.db);
        let Partial::Present(expr_data) = expr.data(self.db, self.body) else {
            return self.order;
        };

        let mut ctxt = VisitorCtxt::with_expr(self.db, self.body.scope(), self.body, expr);
        self.visit_expr(&mut ctxt, expr, expr_data);
        self.order
    }
}
