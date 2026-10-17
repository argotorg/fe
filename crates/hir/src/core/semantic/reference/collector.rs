//! Reference collection infrastructure.
//!
//! This module contains the unified reference collector and salsa queries
//! for caching reference collection results.

use crate::{
    HirDb,
    analysis::ty::ty_check::EffectParamSite,
    hir_def::{
        Body, Const, Contract, ContractInit, ContractRecvArm, EffectParam, EffectParamListId, Enum,
        Expr, ExprId, Func, Impl, ImplTrait, ItemKind, Pat, PatId, StaticAssert, Struct, Trait,
        TraitRefId, TypeAlias, Use, UsePathId,
    },
    span::lazy_spans::{
        LazyBodySpan, LazyContractInitSpan, LazyExprSpan, LazyItemSpan, LazyPatSpan, LazyPathSpan,
        LazyRecvArmSpan, LazyTraitRefSpan, LazyUsePathSpan, LazyUsesParamSpan,
    },
    visitor::{
        Visitor, VisitorCtxt, walk_body, walk_contract_init, walk_contract_recv_arm,
        walk_effect_param, walk_expr, walk_pat, walk_path, walk_trait_ref, walk_use_path,
    },
};

use super::{
    BodyPathContext, FieldAccessView, MethodCallView, PathId, PathView, ReferenceView, UsePathView,
    UsesEntry,
};

/// Tracks context for path references during body traversal.
#[derive(Clone, Copy)]
enum PathContext {
    /// Path is in an expression
    Expr(ExprId),
    /// Path is in a pattern (could be binding or reference, determined later)
    Pat(PatId),
    /// No body context (signatures, types, etc.)
    None,
}

/// Single unified collector for all reference types.
///
/// A collector records the references of one item, not of the items nested
/// in it: a trait's or impl's methods, an item in a block, and every body,
/// including a field type's array length, a `where` condition and an array
/// length in another body. Each of those is an item of its own in the scope
/// graph, with a collector of its own, so visiting it from the enclosing item
/// would record each of its references twice.
pub(super) struct ReferenceCollector<'db> {
    db: &'db dyn HirDb,
    pub refs: Vec<ReferenceView<'db>>,
    /// The body whose references are collected, for a body's collector.
    body: Option<Body<'db>>,
    current_use: Option<Use<'db>>,
    /// Tracks the current context when visiting paths inside a body.
    /// Set before walking expressions/patterns that contain paths.
    path_context: PathContext,
    /// Set while visiting a trait reference, until its own path is visited.
    /// Paths in that path's generic arguments are not trait references.
    trait_ref: bool,
    /// The `uses` list of the contract `init` or recv arm being visited, whose
    /// unlabeled entries name the contract's fields and effects.
    uses_list: Option<(EffectParamSite<'db>, EffectParamListId<'db>)>,
    /// Set while visiting such an entry, with its key, until the key is
    /// visited.
    uses_entry: Option<(PathId<'db>, UsesEntry<'db>)>,
}

impl<'db> ReferenceCollector<'db> {
    pub fn new(db: &'db dyn HirDb) -> Self {
        Self {
            db,
            refs: Vec::new(),
            body: None,
            current_use: None,
            path_context: PathContext::None,
            trait_ref: false,
            uses_list: None,
            uses_entry: None,
        }
    }

    pub fn for_body(db: &'db dyn HirDb, body: Body<'db>) -> Self {
        Self {
            body: Some(body),
            ..Self::new(db)
        }
    }
}

impl<'db> Visitor<'db> for ReferenceCollector<'db> {
    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        let trait_ref = std::mem::take(&mut self.trait_ref);
        let uses_entry = self
            .uses_entry
            .take_if(|(key, _)| *key == path)
            .map(|(_, entry)| entry);
        if let Some(span) = ctxt.span() {
            let mut path_view = PathView::new(path, ctxt.scope(), span, trait_ref);
            path_view.uses_entry = uses_entry;

            // Attach body context if we're inside a body expression/pattern
            match self.path_context {
                PathContext::Expr(expr_id) => {
                    path_view = path_view.with_body_ctx(BodyPathContext::Expr(expr_id));
                }
                PathContext::Pat(pat_id) => {
                    // For patterns, we store PatBinding initially.
                    // local_target() will determine if it's actually a binding or reference.
                    path_view = path_view.with_body_ctx(BodyPathContext::PatBinding(pat_id));
                }
                PathContext::None => {}
            }

            self.refs.push(ReferenceView::Path(path_view));
        }
        walk_path(self, ctxt, path);
    }

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
        trait_ref: TraitRefId<'db>,
    ) {
        self.trait_ref = true;
        walk_trait_ref(self, ctxt, trait_ref);
        self.trait_ref = false;
    }

    fn visit_contract_init(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyContractInitSpan<'db>>,
        init: ContractInit<'db>,
    ) {
        if let ItemKind::Contract(contract) = ctxt.scope().item() {
            let site = EffectParamSite::ContractInit { contract };
            self.uses_list = Some((site, init.effects(self.db)));
        }
        walk_contract_init(self, ctxt, init);
        self.uses_list = None;
    }

    fn visit_contract_recv_arm(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyRecvArmSpan<'db>>,
        arm: &ContractRecvArm<'db>,
    ) {
        if let ItemKind::Contract(contract) = ctxt.scope().item() {
            self.uses_list = contract
                .recvs(self.db)
                .data(self.db)
                .iter()
                .enumerate()
                .find_map(|(recv_idx, recv)| {
                    let arm_idx = recv
                        .arms
                        .data(self.db)
                        .iter()
                        .position(|other| other.body == arm.body)?;
                    let site = EffectParamSite::ContractRecvArm {
                        contract,
                        recv_idx: recv_idx as u32,
                        arm_idx: arm_idx as u32,
                    };
                    Some((site, arm.effects))
                });
        }
        walk_contract_recv_arm(self, ctxt, arm);
        self.uses_list = None;
    }

    fn visit_effect_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyUsesParamSpan<'db>>,
        effect: &EffectParam<'db>,
    ) {
        // Identical entries name the same thing, so the first one stands for
        // each of them.
        if let Some((site, effects)) = self.uses_list
            && effect.name.is_none()
            && let Some(key) = effect.key_ty.to_opt().and_then(|key| key.as_path(self.db))
            && let Some(idx) = effects
                .data(self.db)
                .iter()
                .position(|other| other == effect)
        {
            self.uses_entry = Some((key, UsesEntry { site, idx }));
        }
        walk_effect_param(self, ctxt, effect);
        self.uses_entry = None;
    }

    fn visit_item(&mut self, _: &mut VisitorCtxt<'db, LazyItemSpan<'db>>, _: ItemKind<'db>) {}

    fn visit_body(&mut self, ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, body: Body<'db>) {
        if self.body == Some(body) {
            walk_body(self, ctxt, body);
        }
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
        expr: ExprId,
        expr_data: &Expr<'db>,
    ) {
        if let Some(body) = self.body {
            match expr_data {
                Expr::Path(_) => {
                    // Set context for path expressions before walking
                    let old_ctx = self.path_context;
                    self.path_context = PathContext::Expr(expr);
                    walk_expr(self, ctxt, expr);
                    self.path_context = old_ctx;
                    return;
                }
                Expr::Field(_, _) => {
                    if let Some(span) = ctxt.span() {
                        self.refs.push(ReferenceView::FieldAccess(FieldAccessView {
                            body,
                            expr,
                            span: span.into_field_expr(),
                        }));
                    }
                }
                Expr::MethodCall(_, _, _, _) => {
                    if let Some(span) = ctxt.span() {
                        self.refs.push(ReferenceView::MethodCall(MethodCallView {
                            body,
                            expr,
                            span: span.into_method_call_expr(),
                        }));
                    }
                }
                _ => {}
            }
        }
        walk_expr(self, ctxt, expr);
    }

    fn visit_pat(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>,
        pat: PatId,
        pat_data: &Pat<'db>,
    ) {
        // Set context for pattern paths before walking
        if matches!(pat_data, Pat::Path(_, _)) {
            let old_ctx = self.path_context;
            self.path_context = PathContext::Pat(pat);
            walk_pat(self, ctxt, pat);
            self.path_context = old_ctx;
            return;
        }
        walk_pat(self, ctxt, pat);
    }

    fn visit_use(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, crate::span::item::LazyUseSpan<'db>>,
        use_: Use<'db>,
    ) {
        let old = self.current_use.replace(use_);
        crate::visitor::walk_use(self, ctxt, use_);
        self.current_use = old;
    }

    fn visit_use_path(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyUsePathSpan<'db>>,
        use_path: UsePathId<'db>,
    ) {
        if let (Some(use_item), Some(span)) = (self.current_use, ctxt.span()) {
            let segment_count = use_path.data(self.db).len();
            for idx in 0..segment_count {
                self.refs.push(ReferenceView::UsePath(UsePathView {
                    use_item,
                    segment: idx,
                    span: span.clone(),
                }));
            }
        }
        walk_use_path(self, ctxt, use_path);
    }
}

#[salsa::tracked(return_ref)]
pub fn body_references<'db>(db: &'db dyn HirDb, body: Body<'db>) -> Vec<ReferenceView<'db>> {
    // The condition's body records the references of a comparison's sides.
    if body.is_static_assert_comparison_side(db) {
        return Vec::new();
    }
    let mut collector = ReferenceCollector::for_body(db, body);
    let mut ctxt = VisitorCtxt::with_body(db, body);
    collector.visit_body(&mut ctxt, body);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn func_signature_references<'db>(
    db: &'db dyn HirDb,
    func: Func<'db>,
) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_func(db, func);
    collector.visit_func(&mut ctxt, func);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn struct_references<'db>(db: &'db dyn HirDb, struct_: Struct<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_struct(db, struct_);
    collector.visit_struct(&mut ctxt, struct_);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn enum_references<'db>(db: &'db dyn HirDb, enum_: Enum<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_enum(db, enum_);
    collector.visit_enum(&mut ctxt, enum_);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn type_alias_references<'db>(
    db: &'db dyn HirDb,
    type_alias: TypeAlias<'db>,
) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_type_alias(db, type_alias);
    collector.visit_type_alias(&mut ctxt, type_alias);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn impl_references<'db>(db: &'db dyn HirDb, impl_: Impl<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_impl(db, impl_);
    collector.visit_impl(&mut ctxt, impl_);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn trait_references<'db>(db: &'db dyn HirDb, trait_: Trait<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_trait(db, trait_);
    collector.visit_trait(&mut ctxt, trait_);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn impl_trait_references<'db>(
    db: &'db dyn HirDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_impl_trait(db, impl_trait);
    collector.visit_impl_trait(&mut ctxt, impl_trait);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn const_references<'db>(db: &'db dyn HirDb, const_: Const<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_const(db, const_);
    collector.visit_const(&mut ctxt, const_);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn static_assert_references<'db>(
    db: &'db dyn HirDb,
    assert_: StaticAssert<'db>,
) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_static_assert(db, assert_);
    collector.visit_static_assert(&mut ctxt, assert_);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn use_references<'db>(db: &'db dyn HirDb, use_item: Use<'db>) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_use(db, use_item);
    collector.visit_use(&mut ctxt, use_item);
    collector.refs
}

#[salsa::tracked(return_ref)]
pub fn contract_references<'db>(
    db: &'db dyn HirDb,
    contract: Contract<'db>,
) -> Vec<ReferenceView<'db>> {
    let mut collector = ReferenceCollector::new(db);
    let mut ctxt = VisitorCtxt::with_contract(db, contract);
    collector.visit_contract(&mut ctxt, contract);
    collector.refs
}
