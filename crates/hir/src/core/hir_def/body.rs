// This is necessary because `salsa::tracked` structs generates a
// constructor
// that may take many arguments depending on the number of fields in the struct.
#![allow(clippy::too_many_arguments)]

use std::hash::Hash;

use common::indexmap::IndexMap;
use cranelift_entity::{EntityRef, PrimaryMap, SecondaryMap};
use parser::ast::{self, prelude::*};
use rustc_hash::FxHashMap;
use salsa::Update;

use super::{
    Expr, ExprId, Func, ItemKind, Partial, Pat, PatId, Stmt, StmtId, TopLevelMod, TrackedItemId,
    scope_graph::ScopeId,
};
use crate::{
    HirDb,
    span::{HirOrigin, item::LazyBodySpan},
    visitor::prelude::*,
};

#[salsa::tracked]
#[derive(Debug)]
pub struct Body<'db> {
    //    #[id]
    id: TrackedItemId<'db>,

    /// The expression that evaluates to the value of the body.
    /// In case of a function body, this is always be the block expression.
    pub expr: ExprId,

    pub body_kind: BodyKind,

    #[return_ref]
    pub stmts: NodeStore<StmtId, Partial<Stmt<'db>>>,
    #[return_ref]
    pub exprs: NodeStore<ExprId, Partial<Expr<'db>>>,
    #[return_ref]
    pub pats: NodeStore<PatId, Partial<Pat<'db>>>,
    pub top_mod: TopLevelMod<'db>,

    #[return_ref]
    pub(crate) source_map: BodySourceMap,
    #[return_ref]
    pub(crate) origin: HirOrigin<ast::Expr>,
}

impl<'db> Body<'db> {
    pub fn span(self) -> LazyBodySpan<'db> {
        LazyBodySpan::new(self)
    }

    pub fn scope(self) -> ScopeId<'db> {
        ScopeId::from_item(self.into())
    }

    /// Find the function that contains this body.
    ///
    /// Returns `Some(func)` if this body belongs to a function, `None` otherwise
    /// (e.g., for anonymous/closure bodies or const bodies).
    pub fn containing_func(self, db: &'db dyn HirDb) -> Option<Func<'db>> {
        let scope_graph = self.scope().scope_graph(db);
        for item in scope_graph.items_dfs(db) {
            if let ItemKind::Func(func) = item
                && func.body(db) == Some(self)
            {
                return Some(func);
            }
        }
        None
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

#[derive(Debug, Hash, Clone)]
pub struct NodeStore<K, V>(PrimaryMap<K, V>)
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

unsafe impl<K, V> Update for NodeStore<K, V>
where
    K: EntityRef + Update,
    V: Update,
{
    unsafe fn maybe_update(old_ptr: *mut Self, new_val: Self) -> bool {
        unsafe {
            let old_val = &mut *old_ptr;
            if old_val.len() != new_val.len() {
                *old_val = new_val;
                return true;
            }

            let mut changed = false;
            for (k, v) in new_val.0.into_iter() {
                let old_val = &mut old_val[k];
                changed |= Update::maybe_update(old_val, v);
            }

            changed
        }
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
}

#[allow(clippy::derived_hash_with_manual_eq)]
#[derive(Clone, Debug, Hash)]
pub struct SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    pub node_to_source: SecondaryMap<Node, HirOrigin<Ast>>,
    pub source_to_node: IndexMap<HirOrigin<Ast>, Node>,
}

impl<Ast, Node> SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    pub(crate) fn insert(&mut self, node: Node, ast: HirOrigin<Ast>) {
        self.node_to_source[node] = ast.clone();
        self.source_to_node.insert(ast, node);
    }

    pub(crate) fn node_to_source(&self, node: Node) -> &HirOrigin<Ast> {
        &self.node_to_source[node]
    }
}

impl<Ast, Node> PartialEq for SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    fn eq(&self, other: &Self) -> bool {
        self.node_to_source == other.node_to_source
    }
}

impl<Ast, Node> Eq for SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
}

impl<Ast, Node> Default for SourceNodeMap<Ast, Node>
where
    Ast: SourceAst,
    Node: EntityRef,
{
    fn default() -> Self {
        Self {
            source_to_node: IndexMap::default(),
            node_to_source: SecondaryMap::new(),
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
