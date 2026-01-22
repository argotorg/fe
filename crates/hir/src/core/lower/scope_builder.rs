use cranelift_entity::{PrimaryMap, entity_impl};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    HirDb,
    hir_def::{
        Body, Enum, EnumVariant, ExprId, FieldDefListId, FieldParent, FuncParamListId,
        FuncParamName, GenericParamListId, GenericParamOwner, HirIngot, ItemKind, TopLevelMod,
        TrackedItemId, TrackedItemVariant, Trait, Use, VariantDefListId, VariantKind, Visibility,
        scope_graph::{EdgeKind, Scope, ScopeEdge, ScopeGraph, ScopeId},
    },
};

/// An [`ScopeGraph`] builder that is used to construct the scope in the hir
/// lowering phase.
//
// The difficulty in constructing a scope graph lies in that the ScopeId must
// hold the corresponding HIR node to represent the scope. However, because HIR
// nodes tracked by salsa are immutable, it is only possible to create HIR nodes
// once the lowering of the item is completely finished. This means that a
// ScopeId can only be constructed after the completion of lowering, or at the
// end point of the scope.
//
// Therefore, the builder's `enter_*_scope` method group does not take any
// concrete item information as arguments. When the `enter_*_scope` method group
// is called, the builder constructs a dummy scope and sets up the relationship
// between this dummy scope and other scopes. Then, when the `leave_*_scope`
// method group is called, the builder substitutes the dummy scope with the real
// scope while keeping the relationship between scopes intact.
pub(super) struct ScopeGraphBuilder<'db> {
    pub(super) db: &'db dyn HirDb,
    pub(super) top_mod: TopLevelMod<'db>,
    graph: IntermediateScopeGraph<'db>,
    scope_stack: Vec<NodeId>,
    module_stack: Vec<NodeId>,
    id_stack: Vec<TrackedItemId<'db>>,
    declared_blocks: Vec<FxHashMap<NodeId, Option<ExprId>>>,
}

impl<'db> ScopeGraphBuilder<'db> {
    pub(super) fn enter_top_mod(db: &'db dyn HirDb, top_mod: TopLevelMod<'db>) -> Self {
        let mut builder = Self {
            db,
            top_mod,
            graph: IntermediateScopeGraph::default(),
            scope_stack: Default::default(),
            module_stack: Default::default(),
            id_stack: Default::default(),
            declared_blocks: vec![],
        };

        let id = TrackedItemId::new(db, TrackedItemVariant::TopLevelMod(top_mod.name(db)));
        builder.enter_item_scope(id, true);
        builder
    }

    pub(super) fn build(self) -> ScopeGraph<'db> {
        self.graph.build(self.top_mod)
    }

    pub(super) fn enter_item_scope(&mut self, id: TrackedItemId<'db>, is_mod: bool) {
        self.id_stack.push(id);
        self.enter_scope_impl(is_mod);
    }

    pub(super) fn enter_body_scope(&mut self, id: TrackedItemId<'db>) {
        self.declared_blocks.push(FxHashMap::default());
        self.enter_item_scope(id, false);
    }

    pub(super) fn leave_item_scope(&mut self, item: ItemKind<'db>) {
        use ItemKind::*;

        self.id_stack.pop();
        let item_node = self.scope_stack.pop().unwrap();
        self.initialize_item_scope(item_node, item);

        if let ItemKind::TopMod(top_mod) = item {
            debug_assert!(self.scope_stack.is_empty());
            self.graph.add_edge(item_node, item_node, EdgeKind::self_());

            self.graph.add_external_edge(
                item_node,
                ScopeId::Item(top_mod.ingot(self.db).root_mod(self.db).into()),
                EdgeKind::ingot(),
            );
            for child in top_mod.child_top_mods(self.db) {
                let child_name = child.name(self.db);
                let edge = EdgeKind::mod_(child_name);
                self.graph
                    .add_external_edge(item_node, ScopeId::Item(child.into()), edge)
            }

            if let Some(parent) = top_mod.parent(self.db) {
                let edge = EdgeKind::super_();
                self.graph
                    .add_external_edge(item_node, ScopeId::Item(parent.into()), edge);
            }
            self.module_stack.pop().unwrap();

            return;
        }

        let parent_node = *self.scope_stack.last().unwrap();
        let parent_to_child_edge = match item {
            Mod(inner) => {
                self.module_stack.pop().unwrap();

                self.graph.add_edge(
                    item_node,
                    *self.module_stack.last().unwrap(),
                    EdgeKind::super_(),
                );
                self.graph.add_external_edge(
                    item_node,
                    ScopeId::Item(self.top_mod.ingot(self.db).root_mod(self.db).into()),
                    EdgeKind::ingot(),
                );
                self.graph.add_edge(item_node, item_node, EdgeKind::self_());

                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::mod_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Func(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    GenericParamOwner::from(inner).params_list(self.db),
                );
                if let Some(params) = inner.params_list(self.db).to_opt() {
                    self.add_func_param_scope(item_node, inner.into(), params);
                }

                // Add self_ty edge if this function is inside a trait or impl
                if !self.scope_stack.is_empty() {
                    let parent_scope_id = self.graph.nodes[parent_node].0;
                    if let Some(parent_item) = parent_scope_id.to_item() {
                        match parent_item {
                            ItemKind::Trait(_) => {
                                // Function is inside a trait, add edge to trait for Self resolution
                                self.graph
                                    .add_edge(item_node, parent_node, EdgeKind::self_ty());
                            }
                            ItemKind::Impl(_) | ItemKind::ImplTrait(_) => {
                                // Function is inside an impl, add edge to impl for Self resolution
                                self.graph
                                    .add_edge(item_node, parent_node, EdgeKind::self_ty());
                            }
                            _ => {}
                        }
                    }
                }

                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::value)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Struct(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_field_scope(item_node, FieldParent::Struct(inner), inner.fields(self.db));
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                self.graph
                    .add_edge(item_node, item_node, EdgeKind::self_ty());
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::type_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Contract(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_field_scope(
                    item_node,
                    FieldParent::Contract(inner),
                    inner.hir_fields(self.db),
                );
                self.graph
                    .add_edge(item_node, item_node, EdgeKind::self_ty());

                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::type_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Enum(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_variant_scope(item_node, inner, inner.variants_list(self.db));
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                self.graph
                    .add_edge(item_node, item_node, EdgeKind::self_ty());
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::type_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            TypeAlias(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::type_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Impl(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                self.graph
                    .add_edge(item_node, item_node, EdgeKind::self_ty());
                EdgeKind::anon()
            }

            Trait(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                self.add_trait_type_scope(item_node, inner);
                self.add_trait_const_scope(item_node, inner);
                self.graph
                    .add_edge(item_node, item_node, EdgeKind::self_ty());
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::trait_)
                    .unwrap_or_else(EdgeKind::anon)
            }

            ImplTrait(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                self.add_generic_param_scope(
                    item_node,
                    inner.into(),
                    inner.generic_params(self.db),
                );
                self.graph
                    .add_edge(item_node, item_node, EdgeKind::self_ty());
                EdgeKind::anon()
            }

            Const(inner) => {
                self.graph.add_lex_edge(item_node, parent_node);
                inner
                    .name(self.db)
                    .to_opt()
                    .map(EdgeKind::value)
                    .unwrap_or_else(EdgeKind::anon)
            }

            Use(use_) => {
                self.graph.unresolved_uses.insert(use_);

                self.graph.add_lex_edge(item_node, parent_node);
                EdgeKind::anon()
            }

            Body(body) => {
                self.graph.add_lex_edge(item_node, parent_node);
                for (node, block) in self.declared_blocks.pop().unwrap() {
                    let block = block.unwrap();
                    self.finalize_block_scope(node, body, block);
                }
                EdgeKind::anon()
            }

            _ => unreachable!(),
        };

        self.graph
            .add_edge(parent_node, item_node, parent_to_child_edge);
    }

    pub(super) fn joined_id(&self, variant: TrackedItemVariant<'db>) -> TrackedItemId<'db> {
        self.id_stack.last().unwrap().join(self.db, variant)
    }

    pub(super) fn enter_block_scope(&mut self) {
        let node = self.enter_scope_impl(false);
        self.declared_blocks.last_mut().unwrap().insert(node, None);
    }

    pub(super) fn leave_block_scope(&mut self, block: ExprId) {
        let block_node = self.scope_stack.pop().unwrap();
        let parent_node = *self.scope_stack.last().unwrap();
        *self
            .declared_blocks
            .last_mut()
            .unwrap()
            .get_mut(&block_node)
            .unwrap() = Some(block);
        self.graph.add_lex_edge(block_node, parent_node);
        self.graph
            .add_edge(parent_node, block_node, EdgeKind::anon());
    }

    fn enter_scope_impl(&mut self, is_mod: bool) -> NodeId {
        // Create dummy scope, the scope kind is initialized when leaving the scope.
        let (dummy_scope_id, dummy_scope) = self.dummy_scope();
        let id = self.graph.push(dummy_scope_id, dummy_scope);
        self.scope_stack.push(id);
        if is_mod {
            self.module_stack.push(id);
        }
        id
    }

    fn initialize_item_scope(&mut self, node: NodeId, item: ItemKind<'db>) {
        self.graph.initialize_item_scope(self.db, node, item)
    }

    fn finalize_block_scope(&mut self, node: NodeId, body: Body<'db>, block: ExprId) {
        self.graph.finalize_block_scope(node, body, block);
    }

    fn add_field_scope(
        &mut self,
        parent_node: NodeId,
        parent: FieldParent<'db>,
        fields: FieldDefListId<'db>,
    ) {
        for (i, field) in fields.data(self.db).iter().enumerate() {
            let scope_id = ScopeId::Field(parent, i as u16);
            let scope_data = Scope::new(scope_id, field.vis);

            let field_node = self.graph.push(scope_id, scope_data);
            self.graph.add_lex_edge(field_node, parent_node);
            let kind = field
                .name
                .to_opt()
                .map(EdgeKind::field)
                .unwrap_or_else(EdgeKind::anon);
            self.graph.add_edge(parent_node, field_node, kind)
        }
    }

    fn add_variant_scope(
        &mut self,
        parent_node: NodeId,
        parent_item: Enum<'db>,
        variants: VariantDefListId<'db>,
    ) {
        let parent_vis = parent_item.vis(self.db);

        for (i, variant_def) in variants.data(self.db).iter().enumerate() {
            let variant = EnumVariant::new(parent_item, i);
            let scope_id = ScopeId::Variant(variant);
            let scope_data = Scope::new(scope_id, parent_vis);

            let variant_node = self.graph.push(scope_id, scope_data);
            self.graph.add_lex_edge(variant_node, parent_node);
            let kind = variant_def
                .name
                .to_opt()
                .map(EdgeKind::variant)
                .unwrap_or_else(EdgeKind::anon);

            if let VariantKind::Record(fields) = variant_def.kind {
                self.add_field_scope(variant_node, FieldParent::Variant(variant), fields)
            }

            self.graph.add_edge(parent_node, variant_node, kind)
        }
    }

    fn add_func_param_scope(
        &mut self,
        parent_node: NodeId,
        parent_item: ItemKind<'db>,
        params: FuncParamListId<'db>,
    ) {
        for (i, param) in params.data(self.db).iter().enumerate() {
            let scope_id = ScopeId::FuncParam(parent_item, i as u16);
            let scope = Scope::new(scope_id, Visibility::Private);
            let func_param_node = self.graph.push(scope_id, scope);

            self.graph.add_lex_edge(func_param_node, parent_node);
            let kind = param
                .name
                .to_opt()
                .map(|name| match name {
                    FuncParamName::Ident(ident) => EdgeKind::value(ident),
                    FuncParamName::Underscore => EdgeKind::anon(),
                })
                .unwrap_or_else(EdgeKind::anon);
            self.graph.add_edge(parent_node, func_param_node, kind)
        }
    }

    fn add_generic_param_scope(
        &mut self,
        parent_node: NodeId,
        parent_item: ItemKind<'db>,
        params: GenericParamListId<'db>,
    ) {
        for (i, param) in params.data(self.db).iter().enumerate() {
            let scope_id = ScopeId::GenericParam(parent_item, i as u16);
            let scope = Scope::new(scope_id, Visibility::Private);

            let generic_param_node = self.graph.push(scope_id, scope);
            self.graph.add_lex_edge(generic_param_node, parent_node);
            let kind = param
                .name()
                .to_opt()
                .map(EdgeKind::generic_param)
                .unwrap_or_else(EdgeKind::anon);
            self.graph.add_edge(parent_node, generic_param_node, kind)
        }
    }

    fn add_trait_type_scope(&mut self, parent_node: NodeId, trait_: Trait<'db>) {
        for (i, trait_type) in trait_.types(self.db).iter().enumerate() {
            let scope_id = ScopeId::TraitType(trait_, i as u16);
            let scope = Scope::new(scope_id, Visibility::Private);
            let trait_type_node = self.graph.push(scope_id, scope);

            self.graph.add_lex_edge(trait_type_node, parent_node);
            let kind = trait_type
                .name
                .to_opt()
                .map(EdgeKind::trait_type)
                .unwrap_or_else(EdgeKind::anon);
            self.graph.add_edge(parent_node, trait_type_node, kind)
        }
    }

    fn add_trait_const_scope(&mut self, parent_node: NodeId, trait_: Trait<'db>) {
        for (i, assoc_const) in trait_.consts(self.db).iter().enumerate() {
            let scope_id = ScopeId::TraitConst(trait_, i as u16);
            let scope = Scope::new(scope_id, Visibility::Private);
            let const_node = self.graph.push(scope_id, scope);

            self.graph.add_lex_edge(const_node, parent_node);
            let kind = assoc_const
                .name
                .to_opt()
                .map(EdgeKind::value)
                .unwrap_or_else(EdgeKind::anon);
            self.graph.add_edge(parent_node, const_node, kind)
        }
    }

    fn dummy_scope(&self) -> (ScopeId<'db>, Scope<'db>) {
        let scope_id = ScopeId::Item(self.top_mod.into());
        (scope_id, Scope::new(scope_id, Visibility::Public))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NodeId(u32);
entity_impl!(NodeId);

#[derive(Default)]
struct IntermediateScopeGraph<'db> {
    nodes: PrimaryMap<NodeId, (ScopeId<'db>, Scope<'db>)>,
    edges: FxHashMap<NodeId, Vec<(NodeId, EdgeKind<'db>)>>,
    unresolved_uses: FxHashSet<Use<'db>>,
}

impl<'db> IntermediateScopeGraph<'db> {
    fn build(mut self, top_mod: TopLevelMod<'db>) -> ScopeGraph<'db> {
        for (from_node, edges) in self.edges {
            for (dest_node, kind) in edges {
                let dest = self.nodes[dest_node].0;
                let edge = ScopeEdge { dest, kind };
                self.nodes[from_node].1.edges.insert(edge);
            }
        }

        let scopes = self
            .nodes
            .into_iter()
            .map(|(_, (id, data))| (id, data))
            .collect();

        ScopeGraph {
            top_mod,
            scopes,
            unresolved_uses: self.unresolved_uses,
        }
    }

    fn push(&mut self, scope_id: ScopeId<'db>, scope_data: Scope<'db>) -> NodeId {
        self.nodes.push((scope_id, scope_data))
    }

    fn initialize_item_scope(&mut self, db: &dyn HirDb, node: NodeId, item: ItemKind<'db>) {
        let scope_id = ScopeId::Item(item);

        let scope_data = &mut self.nodes[node];
        scope_data.0 = scope_id;
        scope_data.1.id = scope_id;
        scope_data.1.vis = item.vis(db);
    }

    fn finalize_block_scope(&mut self, node: NodeId, body: Body<'db>, block: ExprId) {
        let scope_id = ScopeId::Block(body, block);
        let scope_data = &mut self.nodes[node];
        scope_data.0 = scope_id;
        scope_data.1.id = scope_id;
        scope_data.1.vis = Visibility::Private;
    }

    fn add_lex_edge(&mut self, child: NodeId, parent: NodeId) {
        self.edges
            .entry(child)
            .or_default()
            .push((parent, EdgeKind::lex()));
    }

    fn add_edge(&mut self, from: NodeId, dest: NodeId, kind: EdgeKind<'db>) {
        self.edges.entry(from).or_default().push((dest, kind));
    }

    /// Add an edge to the graph that is not part of the current file.
    fn add_external_edge(&mut self, from: NodeId, dest: ScopeId<'db>, kind: EdgeKind<'db>) {
        self.nodes[from].1.edges.insert(ScopeEdge { dest, kind });
    }
}
