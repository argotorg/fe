use std::{marker::PhantomData, mem};

use crate::{
    HirDb,
    core::span::{
        SpanDowncast, item::LazySuperTraitListSpan, lazy_spans::*, params::LazyTraitRefSpan,
        transition::ChainRoot,
    },
    hir_def::{
        Body, CallArg, Const, Contract, ContractRecv, ContractRecvArm, ContractRecvArmListId,
        EffectParam, EffectParamListId, Enum, EnumVariant, Expr, ExprId, Field, FieldDef,
        FieldDefListId, FieldIndex, FieldParent, Func, FuncParam, FuncParamListId, FuncParamName,
        GenericArg, GenericArgListId, GenericParam, GenericParamListId, IdentId, Impl, ImplTrait,
        ItemKind, KindBound, LitKind, MatchArm, Mod, Partial, Pat, PatId, PathId, PathKind, Stmt,
        StmtId, Struct, TopLevelMod, Trait, TraitRefId, TupleTypeId, TypeAlias, TypeBound, TypeId,
        TypeKind, Use, UseAlias, UsePathId, UsePathSegment, VariantDef, VariantDefListId,
        VariantKind, WhereClauseId, WherePredicate,
        attr::{self, AttrArgValue},
        scope_graph::ScopeId,
    },
};

pub mod prelude {
    pub use super::{
        Visitor, VisitorCtxt, walk_arm, walk_attribute, walk_attribute_list, walk_body,
        walk_call_arg, walk_call_arg_list, walk_const, walk_contract, walk_contract_recv,
        walk_contract_recv_arm, walk_contract_recv_arm_list, walk_effect_param,
        walk_effect_param_list, walk_enum, walk_expr, walk_field, walk_field_def,
        walk_field_def_list, walk_field_list, walk_func, walk_func_param, walk_func_param_list,
        walk_generic_arg, walk_generic_arg_list, walk_generic_param, walk_generic_param_list,
        walk_impl, walk_impl_trait, walk_item, walk_kind_bound, walk_mod, walk_pat, walk_path,
        walk_stmt, walk_struct, walk_super_trait_list, walk_top_mod, walk_trait, walk_trait_ref,
        walk_type, walk_type_alias, walk_type_bound, walk_type_bound_list, walk_use, walk_use_path,
        walk_variant_def, walk_variant_def_list, walk_where_clause, walk_where_predicate,
    };
    pub use crate::core::span::lazy_spans::*;
}

/// A visitor for traversing the HIR.
pub trait Visitor<'db> {
    fn visit_item(&mut self, ctxt: &mut VisitorCtxt<'db, LazyItemSpan<'db>>, item: ItemKind<'db>) {
        walk_item(self, ctxt, item)
    }

    fn visit_top_mod(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTopModSpan<'db>>,
        top_mod: TopLevelMod<'db>,
    ) {
        walk_top_mod(self, ctxt, top_mod)
    }

    fn visit_mod(&mut self, ctxt: &mut VisitorCtxt<'db, LazyModSpan<'db>>, module: Mod<'db>) {
        walk_mod(self, ctxt, module)
    }

    fn visit_func(&mut self, ctxt: &mut VisitorCtxt<'db, LazyFuncSpan<'db>>, func: Func<'db>) {
        walk_func(self, ctxt, func)
    }

    fn visit_struct(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyStructSpan<'db>>,
        struct_: Struct<'db>,
    ) {
        walk_struct(self, ctxt, struct_)
    }

    fn visit_contract(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyContractSpan<'db>>,
        contract: Contract<'db>,
    ) {
        walk_contract(self, ctxt, contract)
    }

    fn visit_effect_param_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyUsesClauseSpan<'db>>,
        effects: EffectParamListId<'db>,
    ) {
        walk_effect_param_list(self, ctxt, effects)
    }

    fn visit_effect_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyUsesParamSpan<'db>>,
        effect: &EffectParam<'db>,
    ) {
        walk_effect_param(self, ctxt, effect)
    }

    fn visit_contract_recv(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyContractRecvSpan<'db>>,
        recv: &ContractRecv<'db>,
    ) {
        walk_contract_recv(self, ctxt, recv)
    }

    fn visit_contract_recv_arm_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyRecvArmListSpan<'db>>,
        arms: ContractRecvArmListId<'db>,
    ) {
        walk_contract_recv_arm_list(self, ctxt, arms)
    }

    fn visit_contract_recv_arm(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyRecvArmSpan<'db>>,
        arm: &ContractRecvArm<'db>,
    ) {
        walk_contract_recv_arm(self, ctxt, arm)
    }

    fn visit_enum(&mut self, ctxt: &mut VisitorCtxt<'db, LazyEnumSpan<'db>>, enum_: Enum<'db>) {
        walk_enum(self, ctxt, enum_)
    }

    fn visit_type_alias(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTypeAliasSpan<'db>>,
        alias: TypeAlias<'db>,
    ) {
        walk_type_alias(self, ctxt, alias)
    }

    fn visit_impl(&mut self, ctxt: &mut VisitorCtxt<'db, LazyImplSpan<'db>>, impl_: Impl<'db>) {
        walk_impl(self, ctxt, impl_)
    }

    fn visit_trait(&mut self, ctxt: &mut VisitorCtxt<'db, LazyTraitSpan<'db>>, trait_: Trait<'db>) {
        walk_trait(self, ctxt, trait_)
    }

    fn visit_impl_trait(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyImplTraitSpan<'db>>,
        impl_trait: ImplTrait<'db>,
    ) {
        walk_impl_trait(self, ctxt, impl_trait)
    }

    fn visit_const(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyConstSpan<'db>>,
        constant: Const<'db>,
    ) {
        walk_const(self, ctxt, constant)
    }

    fn visit_use(&mut self, ctxt: &mut VisitorCtxt<'db, LazyUseSpan<'db>>, use_: Use<'db>) {
        walk_use(self, ctxt, use_)
    }

    fn visit_body(&mut self, ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>, body: Body<'db>) {
        walk_body(self, ctxt, body)
    }

    fn visit_attribute_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyAttrListSpan<'db>>,
        attrs: AttrListId<'db>,
    ) {
        walk_attribute_list(self, ctxt, attrs);
    }

    fn visit_attribute(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyAttrSpan<'db>>,
        attr: &Attr<'db>,
    ) {
        walk_attribute(self, ctxt, attr);
    }

    fn visit_generic_param_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericParamListSpan<'db>>,
        params: GenericParamListId<'db>,
    ) {
        walk_generic_param_list(self, ctxt, params);
    }

    fn visit_generic_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericParamSpan<'db>>,
        param: &GenericParam<'db>,
    ) {
        walk_generic_param(self, ctxt, param);
    }

    fn visit_generic_arg_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericArgListSpan<'db>>,
        args: GenericArgListId<'db>,
    ) {
        walk_generic_arg_list(self, ctxt, args);
    }

    fn visit_generic_arg(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyGenericArgSpan<'db>>,
        arg: &GenericArg<'db>,
    ) {
        walk_generic_arg(self, ctxt, arg);
    }

    fn visit_call_arg_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyCallArgListSpan<'db>>,
        args: &[CallArg<'db>],
    ) {
        walk_call_arg_list(self, ctxt, args);
    }

    fn visit_call_arg(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyCallArgSpan<'db>>,
        arg: CallArg<'db>,
    ) {
        walk_call_arg(self, ctxt, arg);
    }

    fn visit_type_bound_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTypeBoundListSpan<'db>>,
        bounds: &[TypeBound<'db>],
    ) {
        walk_type_bound_list(self, ctxt, bounds);
    }

    fn visit_type_bound(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTypeBoundSpan<'db>>,
        bound: &TypeBound<'db>,
    ) {
        walk_type_bound(self, ctxt, bound);
    }

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
        trait_ref: TraitRefId<'db>,
    ) {
        walk_trait_ref(self, ctxt, trait_ref);
    }

    fn visit_super_trait_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazySuperTraitListSpan<'db>>,
        super_traits: &[TraitRefId<'db>],
    ) {
        walk_super_trait_list(self, ctxt, super_traits);
    }

    fn visit_kind_bound(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyKindBoundSpan<'db>>,
        bound: &KindBound,
    ) {
        walk_kind_bound(self, ctxt, bound);
    }

    fn visit_where_clause(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyWhereClauseSpan<'db>>,
        where_clause: WhereClauseId<'db>,
    ) {
        walk_where_clause(self, ctxt, where_clause);
    }

    fn visit_where_predicate(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyWherePredicateSpan<'db>>,
        where_predicate: &WherePredicate<'db>,
    ) {
        walk_where_predicate(self, ctxt, where_predicate);
    }

    fn visit_func_param_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFuncParamListSpan<'db>>,
        params: FuncParamListId<'db>,
    ) {
        walk_func_param_list(self, ctxt, params);
    }

    fn visit_func_param(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFuncParamSpan<'db>>,
        param: &FuncParam<'db>,
    ) {
        walk_func_param(self, ctxt, param);
    }

    fn visit_field_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFieldListSpan<'db>>,
        fields: &[Field<'db>],
    ) {
        walk_field_list(self, ctxt, fields);
    }

    fn visit_field(&mut self, ctxt: &mut VisitorCtxt<'db, LazyFieldSpan<'db>>, field: Field<'db>) {
        walk_field(self, ctxt, field);
    }

    fn visit_field_def_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFieldDefListSpan<'db>>,
        fields: FieldDefListId<'db>,
    ) {
        walk_field_def_list(self, ctxt, fields);
    }

    fn visit_field_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyFieldDefSpan<'db>>,
        field: &FieldDef<'db>,
    ) {
        walk_field_def(self, ctxt, field);
    }

    fn visit_variant_def_list(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyVariantDefListSpan<'db>>,
        variants: VariantDefListId<'db>,
    ) {
        walk_variant_def_list(self, ctxt, variants);
    }

    fn visit_variant_def(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyVariantDefSpan<'db>>,
        variant: &VariantDef<'db>,
    ) {
        walk_variant_def(self, ctxt, variant)
    }

    fn visit_stmt(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyStmtSpan<'db>>,
        stmt: StmtId,
        #[allow(unused_variables)] stmt_data: &Stmt<'db>,
    ) {
        walk_stmt(self, ctxt, stmt)
    }

    fn visit_expr(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
        expr: ExprId,
        #[allow(unused_variables)] expr_data: &Expr<'db>,
    ) {
        walk_expr(self, ctxt, expr)
    }

    fn visit_pat(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>,
        pat: PatId,
        #[allow(unused_variables)] pat_data: &Pat<'db>,
    ) {
        walk_pat(self, ctxt, pat)
    }

    fn visit_arm(&mut self, ctxt: &mut VisitorCtxt<'db, LazyMatchArmSpan<'db>>, arm: &MatchArm) {
        walk_arm(self, ctxt, arm)
    }

    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        walk_path(self, ctxt, path)
    }

    fn visit_use_path(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyUsePathSpan<'db>>,
        use_path: UsePathId<'db>,
    ) {
        walk_use_path(self, ctxt, use_path)
    }

    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>, ty: TypeId<'db>) {
        walk_type(self, ctxt, ty)
    }

    fn visit_tuple_type(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTupleTypeSpan<'db>>,
        ty: TupleTypeId<'db>,
    ) {
        walk_tuple_type(self, ctxt, ty)
    }

    #[allow(unused_variables)]
    fn visit_lit(&mut self, ctxt: &mut VisitorCtxt<'db, LazyLitSpan<'db>>, lit: LitKind<'db>) {}

    #[allow(unused_variables)]
    fn visit_ident(&mut self, ctxt: &mut VisitorCtxt<'db, LazySpanAtom<'db>>, ident: IdentId<'db>) {
    }
}

pub fn walk_item<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyItemSpan<'db>>,
    item: ItemKind<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    match item {
        ItemKind::TopMod(top_mod) => {
            let mut new_ctxt = VisitorCtxt::with_top_mod(ctxt.db, top_mod);
            visitor.visit_top_mod(&mut new_ctxt, top_mod);
        }
        ItemKind::Mod(mod_) => {
            let mut new_ctxt = VisitorCtxt::with_mod(ctxt.db, mod_);
            visitor.visit_mod(&mut new_ctxt, mod_)
        }
        ItemKind::Func(func) => {
            let mut new_ctxt = VisitorCtxt::with_func(ctxt.db, func);
            visitor.visit_func(&mut new_ctxt, func)
        }
        ItemKind::Struct(struct_) => {
            let mut new_ctxt = VisitorCtxt::with_struct(ctxt.db, struct_);
            visitor.visit_struct(&mut new_ctxt, struct_)
        }
        ItemKind::Contract(contract) => {
            let mut new_ctxt = VisitorCtxt::with_contract(ctxt.db, contract);
            visitor.visit_contract(&mut new_ctxt, contract)
        }
        ItemKind::Enum(enum_) => {
            let mut new_ctxt = VisitorCtxt::with_enum(ctxt.db, enum_);
            visitor.visit_enum(&mut new_ctxt, enum_)
        }
        ItemKind::TypeAlias(alias) => {
            let mut new_ctxt = VisitorCtxt::with_type_alias(ctxt.db, alias);
            visitor.visit_type_alias(&mut new_ctxt, alias)
        }
        ItemKind::Impl(impl_) => {
            let mut new_ctxt = VisitorCtxt::with_impl(ctxt.db, impl_);
            visitor.visit_impl(&mut new_ctxt, impl_)
        }
        ItemKind::Trait(trait_) => {
            let mut new_ctxt = VisitorCtxt::with_trait(ctxt.db, trait_);
            visitor.visit_trait(&mut new_ctxt, trait_)
        }
        ItemKind::ImplTrait(impl_trait) => {
            let mut new_ctxt = VisitorCtxt::with_impl_trait(ctxt.db, impl_trait);
            visitor.visit_impl_trait(&mut new_ctxt, impl_trait)
        }
        ItemKind::Const(const_) => {
            let mut new_ctxt = VisitorCtxt::with_const(ctxt.db, const_);
            visitor.visit_const(&mut new_ctxt, const_)
        }
        ItemKind::Use(use_) => {
            let mut new_ctxt = VisitorCtxt::with_use(ctxt.db, use_);
            visitor.visit_use(&mut new_ctxt, use_)
        }
        ItemKind::Body(body) => {
            let mut new_ctxt = VisitorCtxt::with_body(ctxt.db, body);
            visitor.visit_body(&mut new_ctxt, body)
        }
    };
}

pub fn walk_top_mod<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyTopModSpan<'db>>,
    top_mod: TopLevelMod<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    for child in top_mod.children_non_nested(ctxt.db) {
        visitor.visit_item(&mut VisitorCtxt::with_item(ctxt.db, child), child);
    }
}

pub fn walk_mod<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyModSpan<'db>>,
    mod_: Mod<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(name) = mod_.name(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.name(),
            |ctxt| {
                visitor.visit_ident(ctxt, name);
            },
        )
    };

    ctxt.with_new_ctxt(
        |span| span.attributes(),
        |ctxt| {
            let id = mod_.attributes(ctxt.db);
            visitor.visit_attribute_list(ctxt, id);
        },
    );

    for child in mod_.children_non_nested(ctxt.db) {
        visitor.visit_item(&mut VisitorCtxt::with_item(ctxt.db, child), child);
    }
}

pub fn walk_func<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyFuncSpan<'db>>,
    func: Func<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(name) = func.name(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.name(),
            |ctxt| {
                visitor.visit_ident(ctxt, name);
            },
        )
    };

    ctxt.with_new_ctxt(
        |span| span.attributes(),
        |ctxt| {
            let id = func.attributes(ctxt.db);
            visitor.visit_attribute_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.generic_params(),
        |ctxt| {
            let id = func.generic_params(ctxt.db);
            visitor.visit_generic_param_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.where_clause(),
        |ctxt| {
            let id = func.where_clause(ctxt.db);
            visitor.visit_where_clause(ctxt, id);
        },
    );

    if let Some(id) = func.params_list(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.params(),
            |ctxt| {
                visitor.visit_func_param_list(ctxt, id);
            },
        )
    }

    if let Some(ty) = func.ret_type_ref(ctxt.db) {
        ctxt.with_new_ctxt(
            |span| span.ret_ty(),
            |ctxt| {
                visitor.visit_ty(ctxt, ty);
            },
        )
    }

    if let Some(body) = func.body(ctxt.db) {
        visitor.visit_body(&mut VisitorCtxt::with_body(ctxt.db, body), body);
    }

    // Effects (uses clause)
    ctxt.with_new_ctxt(
        |span| span.sig().effects(),
        |ctxt| {
            let effects = func.effects(ctxt.db);
            visitor.visit_effect_param_list(ctxt, effects);
        },
    );
}

pub fn walk_struct<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyStructSpan<'db>>,
    struct_: Struct<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(id) = struct_.name(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.name(),
            |ctxt| {
                visitor.visit_ident(ctxt, id);
            },
        )
    }

    ctxt.with_new_ctxt(
        |span| span.attributes(),
        |ctxt| {
            let id = struct_.attributes(ctxt.db);
            visitor.visit_attribute_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.generic_params(),
        |ctxt| {
            let id = struct_.generic_params(ctxt.db);
            visitor.visit_generic_param_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.where_clause(),
        |ctxt| {
            let id = struct_.where_clause(ctxt.db);
            visitor.visit_where_clause(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.fields(),
        |ctxt| {
            let id = struct_.fields(ctxt.db);
            visitor.visit_field_def_list(ctxt, id);
        },
    );
}

pub fn walk_contract<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyContractSpan<'db>>,
    contract: Contract<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(id) = contract.name(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.name(),
            |ctxt| {
                visitor.visit_ident(ctxt, id);
            },
        )
    }

    ctxt.with_new_ctxt(
        |span| span.attributes(),
        |ctxt| {
            let id = contract.attributes(ctxt.db);
            visitor.visit_attribute_list(ctxt, id);
        },
    );
    // Contract header effects
    ctxt.with_new_ctxt(
        |span| span.effects(),
        |ctxt| {
            let id = contract.effects(ctxt.db);
            visitor.visit_effect_param_list(ctxt, id);
        },
    );

    // Contract fields live under a dedicated AST node; walk them explicitly.
    ctxt.with_new_ctxt(
        |span| span.fields(),
        |ctxt| {
            let id = contract.fields(ctxt.db);
            let parent = FieldParent::Contract(contract);
            for (idx, field) in id.data(ctxt.db).iter().enumerate() {
                ctxt.with_new_scoped_ctxt(
                    ScopeId::Field(parent, idx as u16),
                    |span| span.field(idx),
                    |ctxt| {
                        visitor.visit_field_def(ctxt, field);
                    },
                );
            }
        },
    );

    let s_graph = contract.top_mod(ctxt.db).scope_graph(ctxt.db);
    let scope = ScopeId::from_item(contract.into());
    for child in s_graph.child_items(scope) {
        visitor.visit_item(&mut VisitorCtxt::with_item(ctxt.db, child), child);
    }

    // Recv handlers
    let recvs = contract.recvs(ctxt.db);
    for (idx, recv) in recvs.data(ctxt.db).iter().enumerate() {
        ctxt.with_new_ctxt(
            |span| span.recv(idx),
            |ctxt| {
                visitor.visit_contract_recv(ctxt, recv);
            },
        );
    }
}

pub fn walk_effect_param_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyUsesClauseSpan<'db>>,
    effects: EffectParamListId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    for (idx, eff) in effects.data(ctxt.db).iter().enumerate() {
        ctxt.with_new_ctxt(
            |span| span.param_idx(idx),
            |ctxt| visitor.visit_effect_param(ctxt, eff),
        );
    }
}

pub fn walk_effect_param<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyUsesParamSpan<'db>>,
    effect: &EffectParam<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(name) = effect.name {
        ctxt.with_new_ctxt(|span| span.name(), |ctxt| visitor.visit_ident(ctxt, name));
    }
    if let Some(path) = effect.key_path.to_opt() {
        ctxt.with_new_ctxt(|span| span.path(), |ctxt| visitor.visit_path(ctxt, path));
    }
}

pub fn walk_contract_recv<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyContractRecvSpan<'db>>,
    recv: &ContractRecv<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(p) = recv.msg_path {
        ctxt.with_new_ctxt(|span| span.path(), |ctxt| visitor.visit_path(ctxt, p));
    }

    ctxt.with_new_ctxt(
        |span| span.arms(),
        |ctxt| {
            visitor.visit_contract_recv_arm_list(ctxt, recv.arms);
        },
    );
}

pub fn walk_contract_recv_arm_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyRecvArmListSpan<'db>>,
    arms: ContractRecvArmListId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    for (idx, arm) in arms.data(ctxt.db).iter().enumerate() {
        ctxt.with_new_ctxt(
            |span| span.arm(idx),
            |ctxt| visitor.visit_contract_recv_arm(ctxt, arm),
        );
    }
}

pub fn walk_contract_recv_arm<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyRecvArmSpan<'db>>,
    arm: &ContractRecvArm<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    let mut pat_ctxt = VisitorCtxt::with_pat(ctxt.db, arm.body.scope(), arm.body, arm.pat);
    if let Partial::Present(pat_data) = arm.pat.data(ctxt.db, arm.body) {
        visitor.visit_pat(&mut pat_ctxt, arm.pat, pat_data);
    }

    if let Some(ret_ty) = arm.ret_ty {
        ctxt.with_new_ctxt(|span| span.ret_ty(), |ctxt| visitor.visit_ty(ctxt, ret_ty));
    }

    ctxt.with_new_ctxt(
        |span| span.effects(),
        |ctxt| visitor.visit_effect_param_list(ctxt, arm.effects),
    );

    visitor.visit_body(&mut VisitorCtxt::with_body(ctxt.db, arm.body), arm.body);
}

pub fn walk_enum<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyEnumSpan<'db>>,
    enum_: Enum<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(id) = enum_.name(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.name(),
            |ctxt| {
                visitor.visit_ident(ctxt, id);
            },
        )
    }

    ctxt.with_new_ctxt(
        |span| span.attributes(),
        |ctxt| {
            let id = enum_.attributes(ctxt.db);
            visitor.visit_attribute_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.generic_params(),
        |ctxt| {
            let id = enum_.generic_params(ctxt.db);
            visitor.visit_generic_param_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.where_clause(),
        |ctxt| {
            let id = enum_.where_clause(ctxt.db);
            visitor.visit_where_clause(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.variants(),
        |ctxt| {
            let id = enum_.variants_list(ctxt.db);
            visitor.visit_variant_def_list(ctxt, id);
        },
    );
}

pub fn walk_type_alias<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyTypeAliasSpan<'db>>,
    alias: TypeAlias<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(id) = alias.name(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.alias(),
            |ctxt| {
                visitor.visit_ident(ctxt, id);
            },
        )
    }

    ctxt.with_new_ctxt(
        |span| span.attributes(),
        |ctxt| {
            let id = alias.attributes(ctxt.db);
            visitor.visit_attribute_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.generic_params(),
        |ctxt| {
            let id = alias.generic_params(ctxt.db);
            visitor.visit_generic_param_list(ctxt, id);
        },
    );

    if let Some(ty) = alias.type_ref(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.ty(),
            |ctxt| {
                visitor.visit_ty(ctxt, ty);
            },
        )
    }
}

pub fn walk_impl<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyImplSpan<'db>>,
    impl_: Impl<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(ty) = impl_.type_ref(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.target_ty(),
            |ctxt| {
                visitor.visit_ty(ctxt, ty);
            },
        )
    }

    ctxt.with_new_ctxt(
        |span| span.attributes(),
        |ctxt| {
            let id = impl_.attributes(ctxt.db);
            visitor.visit_attribute_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.generic_params(),
        |ctxt| {
            let id = impl_.generic_params(ctxt.db);
            visitor.visit_generic_param_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.where_clause(),
        |ctxt| {
            let id = impl_.where_clause(ctxt.db);
            visitor.visit_where_clause(ctxt, id);
        },
    );

    for item in impl_.children_non_nested(ctxt.db) {
        visitor.visit_item(&mut VisitorCtxt::with_item(ctxt.db, item), item);
    }
}

pub fn walk_trait<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyTraitSpan<'db>>,
    trait_: Trait<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(name) = trait_.name(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.name(),
            |ctxt| {
                visitor.visit_ident(ctxt, name);
            },
        )
    }

    ctxt.with_new_ctxt(
        |span| span.attributes(),
        |ctxt| {
            let id = trait_.attributes(ctxt.db);
            visitor.visit_attribute_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.generic_params(),
        |ctxt| {
            let id = trait_.generic_params(ctxt.db);
            visitor.visit_generic_param_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.super_traits(),
        |ctxt| visitor.visit_super_trait_list(ctxt, trait_.super_traits_refs(ctxt.db)),
    );

    ctxt.with_new_ctxt(
        |span| span.where_clause(),
        |ctxt| {
            let id = trait_.where_clause(ctxt.db);
            visitor.visit_where_clause(ctxt, id);
        },
    );

    for item in trait_.children_non_nested(ctxt.db) {
        visitor.visit_item(&mut VisitorCtxt::with_item(ctxt.db, item), item);
    }
}

pub fn walk_impl_trait<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyImplTraitSpan<'db>>,
    impl_trait: ImplTrait<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(trait_ref) = impl_trait.trait_ref(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.trait_ref(),
            |ctxt| {
                visitor.visit_trait_ref(ctxt, trait_ref);
            },
        )
    }

    if let Some(ty) = impl_trait.type_ref(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.ty(),
            |ctxt| {
                visitor.visit_ty(ctxt, ty);
            },
        )
    }

    ctxt.with_new_ctxt(
        |span| span.attributes(),
        |ctxt| {
            let id = impl_trait.attributes(ctxt.db);
            visitor.visit_attribute_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.generic_params(),
        |ctxt| {
            let id = impl_trait.generic_params(ctxt.db);
            visitor.visit_generic_param_list(ctxt, id);
        },
    );

    ctxt.with_new_ctxt(
        |span| span.where_clause(),
        |ctxt| {
            let id = impl_trait.where_clause(ctxt.db);
            visitor.visit_where_clause(ctxt, id);
        },
    );

    for item in impl_trait.children_non_nested(ctxt.db) {
        visitor.visit_item(&mut VisitorCtxt::with_item(ctxt.db, item), item);
    }
}

pub fn walk_const<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyConstSpan<'db>>,
    const_: Const<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(name) = const_.name(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.name(),
            |ctxt| {
                visitor.visit_ident(ctxt, name);
            },
        )
    }

    if let Some(ty) = const_.type_ref(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.ty(),
            |ctxt| {
                visitor.visit_ty(ctxt, ty);
            },
        )
    }

    if let Some(body) = const_.body(ctxt.db).to_opt() {
        visitor.visit_body(&mut VisitorCtxt::with_body(ctxt.db, body), body);
    }
}

pub fn walk_use<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyUseSpan<'db>>,
    use_: Use<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    ctxt.with_new_ctxt(
        |span| span.attributes(),
        |ctxt| {
            let id = use_.attributes(ctxt.db);
            visitor.visit_attribute_list(ctxt, id);
        },
    );

    if let Some(use_path) = use_.path(ctxt.db).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.path(),
            |ctxt| {
                visitor.visit_use_path(ctxt, use_path);
            },
        )
    }

    if let Some(Partial::Present(UseAlias::Ident(ident))) = use_.alias(ctxt.db) {
        ctxt.with_new_ctxt(
            |span| span.alias().name(),
            |ctxt| {
                visitor.visit_ident(ctxt, ident);
            },
        )
    }
}

pub fn walk_body<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyBodySpan<'db>>,
    body: Body<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    let body_expr = body.expr(ctxt.db);
    visit_node_in_body!(visitor, ctxt, &body_expr, expr);
}

pub fn walk_stmt<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyStmtSpan<'db>>,
    stmt: StmtId,
) where
    V: Visitor<'db> + ?Sized,
{
    let Partial::Present(stmt) = stmt.data(ctxt.db, ctxt.body()) else {
        return;
    };

    match stmt {
        Stmt::Let(pat_id, ty, expr_id) => {
            visit_node_in_body!(visitor, ctxt, pat_id, pat);

            if let Some(ty) = ty {
                ctxt.with_new_ctxt(
                    |span| span.into_let_stmt().ty(),
                    |ctxt| {
                        visitor.visit_ty(ctxt, *ty);
                    },
                )
            };

            if let Some(expr_id) = expr_id {
                visit_node_in_body!(visitor, ctxt, expr_id, expr);
            }
        }

        Stmt::For(pat_id, cond_id, for_body_id) => {
            visit_node_in_body!(visitor, ctxt, pat_id, pat);
            visit_node_in_body!(visitor, ctxt, cond_id, expr);
            visit_node_in_body!(visitor, ctxt, for_body_id, expr);
        }

        Stmt::While(cond_id, while_body_id) => {
            visit_node_in_body!(visitor, ctxt, cond_id, expr);
            visit_node_in_body!(visitor, ctxt, while_body_id, expr);
        }

        Stmt::Return(Some(expr_id)) | Stmt::Expr(expr_id) => {
            visit_node_in_body!(visitor, ctxt, expr_id, expr);
        }

        Stmt::Return(None) | Stmt::Continue | Stmt::Break => {}
    }
}

pub fn walk_expr<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyExprSpan<'db>>,
    expr: ExprId,
) where
    V: Visitor<'db> + ?Sized,
{
    let Partial::Present(data) = expr.data(ctxt.db, ctxt.body()) else {
        return;
    };

    match data {
        Expr::Lit(lit) => ctxt.with_new_ctxt(
            |span| span.into_lit_expr().lit(),
            |ctxt| {
                visitor.visit_lit(ctxt, *lit);
            },
        ),

        Expr::Block(stmts) => {
            let s_graph = ctxt.top_mod().scope_graph(ctxt.db);
            let scope = ctxt.scope();
            for item in s_graph.child_items(scope) {
                let mut new_ctxt = VisitorCtxt::with_item(ctxt.db, item);
                visitor.visit_item(&mut new_ctxt, item);
            }

            for stmt_id in stmts {
                visit_node_in_body!(visitor, ctxt, stmt_id, stmt);
            }
        }

        Expr::Bin(lhs_id, rhs_id, _) => {
            visit_node_in_body!(visitor, ctxt, lhs_id, expr);
            visit_node_in_body!(visitor, ctxt, rhs_id, expr);
        }

        Expr::Un(expr_id, _) => {
            visit_node_in_body!(visitor, ctxt, expr_id, expr);
        }

        Expr::Call(callee_id, call_args) => {
            visit_node_in_body!(visitor, ctxt, callee_id, expr);
            ctxt.with_new_ctxt(
                |span| span.into_call_expr(),
                |ctxt| {
                    ctxt.with_new_ctxt(
                        |span| span.args(),
                        |ctxt| {
                            visitor.visit_call_arg_list(ctxt, call_args);
                        },
                    );
                },
            );
        }

        Expr::MethodCall(receiver_id, method_name, generic_args, call_args) => {
            visit_node_in_body!(visitor, ctxt, receiver_id, expr);

            ctxt.with_new_ctxt(
                |span| span.into_method_call_expr(),
                |ctxt| {
                    if let Some(method_name) = method_name.to_opt() {
                        ctxt.with_new_ctxt(
                            |span| span.method_name(),
                            |ctxt| visitor.visit_ident(ctxt, method_name),
                        );
                    }

                    ctxt.with_new_ctxt(
                        |span| span.generic_args(),
                        |ctxt| visitor.visit_generic_arg_list(ctxt, *generic_args),
                    );

                    ctxt.with_new_ctxt(
                        |span| span.args(),
                        |ctxt| {
                            visitor.visit_call_arg_list(ctxt, call_args);
                        },
                    );
                },
            );
        }

        Expr::Path(path) => {
            if let Some(path) = path.to_opt() {
                ctxt.with_new_ctxt(
                    |span| span.into_path_expr().path(),
                    |ctxt| {
                        visitor.visit_path(ctxt, path);
                    },
                );
            }
        }

        Expr::RecordInit(path, fields) => {
            ctxt.with_new_ctxt(
                |span| span.into_record_init_expr(),
                |ctxt| {
                    if let Some(path) = path.to_opt() {
                        ctxt.with_new_ctxt(
                            |span| span.path(),
                            |ctxt| {
                                visitor.visit_path(ctxt, path);
                            },
                        );
                    }

                    ctxt.with_new_ctxt(
                        |span| span.fields(),
                        |ctxt| {
                            visitor.visit_field_list(ctxt, fields);
                        },
                    );
                },
            );
        }

        Expr::Field(receiver_id, field_name) => {
            visit_node_in_body!(visitor, ctxt, receiver_id, expr);

            match field_name {
                Partial::Present(FieldIndex::Ident(ident)) => {
                    ctxt.with_new_ctxt(
                        |span| span.into_field_expr().accessor(),
                        |ctxt| visitor.visit_ident(ctxt, *ident),
                    );
                }

                Partial::Present(FieldIndex::Index(index)) => {
                    ctxt.with_new_ctxt(
                        |span| span.into_field_expr().accessor().into_lit_span(),
                        |ctxt| visitor.visit_lit(ctxt, (*index).into()),
                    );
                }

                Partial::Absent => {}
            }
        }

        Expr::Tuple(elems) => {
            for elem_id in elems {
                visit_node_in_body!(visitor, ctxt, elem_id, expr);
            }
        }

        Expr::Array(elems) => {
            for elem_id in elems {
                visit_node_in_body!(visitor, ctxt, elem_id, expr);
            }
        }

        Expr::ArrayRep(val, rep) => {
            visit_node_in_body!(visitor, ctxt, val, expr);
            if let Some(body) = rep.to_opt() {
                visitor.visit_body(&mut VisitorCtxt::with_body(ctxt.db, body), body);
            }
        }

        Expr::If(cond, then, else_) => {
            visit_node_in_body!(visitor, ctxt, cond, expr);
            visit_node_in_body!(visitor, ctxt, then, expr);
            if let Some(else_) = else_ {
                visit_node_in_body!(visitor, ctxt, else_, expr);
            }
        }

        Expr::Match(scrutinee, arms) => {
            visit_node_in_body!(visitor, ctxt, scrutinee, expr);

            if let Partial::Present(arms) = arms {
                ctxt.with_new_ctxt(
                    |span| span.into_match_expr().arms(),
                    |ctxt| {
                        for (i, arm) in arms.iter().enumerate() {
                            ctxt.with_new_ctxt(
                                |span| span.arm(i),
                                |ctxt| {
                                    visitor.visit_arm(ctxt, arm);
                                },
                            );
                        }
                    },
                );
            }
        }

        Expr::Assign(left_expr_id, right_expr_id) => {
            visit_node_in_body!(visitor, ctxt, left_expr_id, expr);
            visit_node_in_body!(visitor, ctxt, right_expr_id, expr);
        }

        Expr::AugAssign(left_expr_id, right_expr_id, _) => {
            visit_node_in_body!(visitor, ctxt, left_expr_id, expr);
            visit_node_in_body!(visitor, ctxt, right_expr_id, expr);
        }

        Expr::With(bindings, body_expr) => {
            for b in bindings {
                visit_node_in_body!(visitor, ctxt, &b.value, expr);
            }
            visit_node_in_body!(visitor, ctxt, body_expr, expr);
        }
    }
}

pub fn walk_arm<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyMatchArmSpan<'db>>,
    arm: &MatchArm,
) where
    V: Visitor<'db> + ?Sized,
{
    visit_node_in_body!(visitor, ctxt, &arm.pat, pat);
    visit_node_in_body!(visitor, ctxt, &arm.body, expr);
}

pub fn walk_pat<'db, V>(visitor: &mut V, ctxt: &mut VisitorCtxt<'db, LazyPatSpan<'db>>, pat: PatId)
where
    V: Visitor<'db> + ?Sized,
{
    let Partial::Present(data) = pat.data(ctxt.db, ctxt.body()) else {
        return;
    };

    match data {
        Pat::Lit(lit) => {
            if let Some(lit) = lit.to_opt() {
                ctxt.with_new_ctxt(
                    |span| span.into_lit_pat().lit(),
                    |ctxt| {
                        visitor.visit_lit(ctxt, lit);
                    },
                )
            };
        }

        Pat::Tuple(elems) => {
            for elem in elems {
                visit_node_in_body!(visitor, ctxt, elem, pat);
            }
        }

        Pat::Path(path, _) => {
            if let Some(path) = path.to_opt() {
                ctxt.with_new_ctxt(
                    |span| span.into_path_pat().path(),
                    |ctxt| {
                        visitor.visit_path(ctxt, path);
                    },
                )
            };
        }

        Pat::PathTuple(path, elems) => {
            if let Some(path) = path.to_opt() {
                ctxt.with_new_ctxt(
                    |span| span.into_path_tuple_pat().path(),
                    |ctxt| {
                        visitor.visit_path(ctxt, path);
                    },
                )
            };

            for elem in elems {
                visit_node_in_body!(visitor, ctxt, elem, pat);
            }
        }

        Pat::Record(path, fields) => ctxt.with_new_ctxt(
            |span| span.into_record_pat(),
            |ctxt| {
                if let Some(path) = path.to_opt() {
                    ctxt.with_new_ctxt(
                        |span| span.path(),
                        |ctxt| {
                            visitor.visit_path(ctxt, path);
                        },
                    );
                }

                ctxt.with_new_ctxt(
                    |span| span.fields(),
                    |ctxt| {
                        for (i, field) in fields.iter().enumerate() {
                            ctxt.with_new_ctxt(
                                |span| span.field(i),
                                |ctxt| {
                                    if let Some(label) = field.label.to_opt() {
                                        ctxt.with_new_ctxt(
                                            |span| span.name(),
                                            |ctxt| {
                                                visitor.visit_ident(ctxt, label);
                                            },
                                        );
                                    }

                                    visit_node_in_body!(visitor, ctxt, &field.pat, pat);
                                },
                            );
                        }
                    },
                );
            },
        ),

        Pat::Or(lhs, rhs) => {
            visit_node_in_body!(visitor, ctxt, lhs, pat);
            visit_node_in_body!(visitor, ctxt, rhs, pat);
        }

        Pat::WildCard | Pat::Rest => {}
    }
}

pub fn walk_attribute_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyAttrListSpan<'db>>,
    attr: AttrListId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    for (idx, attr) in attr.data(ctxt.db).iter().enumerate() {
        ctxt.with_new_ctxt(
            |span| span.attr(idx),
            |ctxt| {
                visitor.visit_attribute(ctxt, attr);
            },
        )
    }
}

pub fn walk_attribute<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyAttrSpan<'db>>,
    attr: &Attr<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    match attr {
        Attr::Normal(attr) => {
            ctxt.with_new_ctxt(
                |span| span.into_normal_attr(),
                |ctxt| {
                    if let Some(path) = attr.path.to_opt() {
                        ctxt.with_new_ctxt(
                            |span| span.path(),
                            |ctxt| {
                                visitor.visit_path(ctxt, path);
                            },
                        )
                    }

                    ctxt.with_new_ctxt(
                        |span| span.args(),
                        |ctxt| {
                            for (i, arg) in attr.args.iter().enumerate() {
                                ctxt.with_new_ctxt(
                                    |span| span.arg(i),
                                    |ctxt| {
                                        let Some(key_path) = arg.key.to_opt() else {
                                            return;
                                        };
                                        ctxt.with_new_ctxt(
                                            |span| span.key(),
                                            |ctxt| {
                                                visitor.visit_path(ctxt, key_path);
                                            },
                                        );

                                        match arg.value.clone() {
                                            Some(AttrArgValue::Ident(id)) => {
                                                ctxt.with_new_ctxt(
                                                    |span| span.value().ident(),
                                                    |ctxt| {
                                                        visitor.visit_ident(ctxt, id);
                                                    },
                                                );
                                            }
                                            Some(AttrArgValue::Lit(l)) => {
                                                ctxt.with_new_ctxt(
                                                    |span| span.value().lit(),
                                                    |ctxt| {
                                                        visitor.visit_lit(ctxt, l);
                                                    },
                                                );
                                            }
                                            None => {}
                                        }
                                    },
                                );
                            }
                        },
                    );
                },
            );
        }

        Attr::DocComment(doc_comment) => ctxt.with_new_ctxt(
            |span| span.into_doc_comment_attr().doc().into_lit_span(),
            |ctxt| {
                visitor.visit_lit(ctxt, doc_comment.text.into());
            },
        ),
    }
}

pub fn walk_generic_param_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyGenericParamListSpan<'db>>,
    params: GenericParamListId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    let parent_item = ctxt.scope().item();
    for (i, param) in params.data(ctxt.db).iter().enumerate() {
        ctxt.with_new_scoped_ctxt(
            ScopeId::GenericParam(parent_item, i as u16),
            |span| span.param(i),
            |ctxt| {
                visitor.visit_generic_param(ctxt, param);
            },
        )
    }
}

pub fn walk_generic_param<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyGenericParamSpan<'db>>,
    param: &GenericParam<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    match param {
        GenericParam::Type(ty_param) => ctxt.with_new_ctxt(
            |span| span.into_type_param(),
            |ctxt| {
                if let Some(name) = ty_param.name.to_opt() {
                    ctxt.with_new_ctxt(
                        |span| span.name(),
                        |ctxt| {
                            visitor.visit_ident(ctxt, name);
                        },
                    );
                }

                ctxt.with_new_ctxt(
                    |span| span.bounds(),
                    |ctxt| {
                        visitor.visit_type_bound_list(ctxt, &ty_param.bounds);
                    },
                );
            },
        ),

        GenericParam::Const(const_param) => ctxt.with_new_ctxt(
            |span| span.into_const_param(),
            |ctxt| {
                if let Some(name) = const_param.name.to_opt() {
                    ctxt.with_new_ctxt(
                        |span| span.name(),
                        |ctxt| {
                            visitor.visit_ident(ctxt, name);
                        },
                    );
                }

                if let Some(ty) = const_param.ty.to_opt() {
                    ctxt.with_new_ctxt(
                        |span| span.ty(),
                        |ctxt| {
                            visitor.visit_ty(ctxt, ty);
                        },
                    );
                }
            },
        ),
    }
}

pub fn walk_generic_arg_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyGenericArgListSpan<'db>>,
    args: GenericArgListId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    for (i, arg) in args.data(ctxt.db).iter().enumerate() {
        ctxt.with_new_ctxt(
            |span| span.arg(i),
            |ctxt| {
                visitor.visit_generic_arg(ctxt, arg);
            },
        )
    }
}

pub fn walk_generic_arg<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyGenericArgSpan<'db>>,
    arg: &GenericArg<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    match arg {
        GenericArg::Type(type_arg) => {
            if let Some(ty) = type_arg.ty.to_opt() {
                ctxt.with_new_ctxt(
                    |span| span.into_type_arg().ty(),
                    |ctxt| {
                        visitor.visit_ty(ctxt, ty);
                    },
                )
            }
        }

        GenericArg::Const(const_arg) => {
            if let Some(body) = const_arg.body.to_opt() {
                visitor.visit_body(&mut VisitorCtxt::with_body(ctxt.db, body), body);
            }
        }

        GenericArg::AssocType(assoc_type_arg) => {
            if let Some(ty) = assoc_type_arg.ty.to_opt() {
                ctxt.with_new_ctxt(
                    |span| span.into_assoc_type_arg().ty(),
                    |ctxt| {
                        visitor.visit_ty(ctxt, ty);
                    },
                )
            }
        }
    }
}

pub fn walk_call_arg_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyCallArgListSpan<'db>>,
    args: &[CallArg<'db>],
) where
    V: Visitor<'db> + ?Sized,
{
    for (idx, arg) in args.iter().copied().enumerate() {
        ctxt.with_new_ctxt(
            |span| span.arg(idx),
            |ctxt| {
                visitor.visit_call_arg(ctxt, arg);
            },
        )
    }
}

pub fn walk_call_arg<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyCallArgSpan<'db>>,
    arg: CallArg<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(label) = arg.label {
        ctxt.with_new_ctxt(|span| span.label(), |ctxt| visitor.visit_ident(ctxt, label));
    }

    visit_node_in_body!(visitor, ctxt, &arg.expr, expr);
}

pub fn walk_func_param_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyFuncParamListSpan<'db>>,
    params: FuncParamListId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    let parent_item = ctxt.scope().item();
    for (idx, param) in params.data(ctxt.db).iter().enumerate() {
        ctxt.with_new_scoped_ctxt(
            ScopeId::FuncParam(parent_item, idx as u16),
            |span| span.param(idx),
            |ctxt| {
                visitor.visit_func_param(ctxt, param);
            },
        )
    }
}

pub fn walk_func_param<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyFuncParamSpan<'db>>,
    param: &FuncParam<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(FuncParamName::Ident(ident)) = param.label {
        ctxt.with_new_ctxt(|span| span.label(), |ctxt| visitor.visit_ident(ctxt, ident));
    }

    if let Some(FuncParamName::Ident(ident)) = param.name.to_opt() {
        ctxt.with_new_ctxt(|span| span.name(), |ctxt| visitor.visit_ident(ctxt, ident));
    }

    if let Some(ty) = param.ty.to_opt() {
        if param.is_self_param(ctxt.db) && param.self_ty_fallback {
            ctxt.with_new_ctxt(
                |span| span.fallback_self_ty(),
                |ctxt| {
                    visitor.visit_ty(ctxt, ty);
                },
            );
        } else {
            ctxt.with_new_ctxt(|span| span.ty(), |ctxt| visitor.visit_ty(ctxt, ty));
        }
    }
}

pub fn walk_field_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyFieldListSpan<'db>>,
    fields: &[Field<'db>],
) where
    V: Visitor<'db> + ?Sized,
{
    for (idx, field) in fields.iter().copied().enumerate() {
        ctxt.with_new_ctxt(
            |span| span.field(idx),
            |ctxt| {
                visitor.visit_field(ctxt, field);
            },
        )
    }
}

pub fn walk_field<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyFieldSpan<'db>>,
    field: Field<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(name) = field.label {
        ctxt.with_new_ctxt(|span| span.label(), |ctxt| visitor.visit_ident(ctxt, name));
    }

    visit_node_in_body!(visitor, ctxt, &field.expr, expr);
}

pub fn walk_field_def_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyFieldDefListSpan<'db>>,
    fields: FieldDefListId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    let parent = match ctxt.scope() {
        ScopeId::Item(ItemKind::Struct(s)) => FieldParent::Struct(s),
        ScopeId::Item(ItemKind::Contract(c)) => FieldParent::Contract(c),
        ScopeId::Variant(v) => FieldParent::Variant(v),
        _ => unreachable!(),
    };
    for (idx, field) in fields.data(ctxt.db).iter().enumerate() {
        ctxt.with_new_scoped_ctxt(
            ScopeId::Field(parent, idx as u16),
            |span| span.field(idx),
            |ctxt| {
                visitor.visit_field_def(ctxt, field);
            },
        )
    }
}

pub fn walk_field_def<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyFieldDefSpan<'db>>,
    field: &FieldDef<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(name) = field.name.to_opt() {
        ctxt.with_new_ctxt(
            |span| span.name(),
            |ctxt| {
                visitor.visit_ident(ctxt, name);
            },
        )
    }

    if let Some(ty) = field.type_ref.to_opt() {
        ctxt.with_new_ctxt(
            |span| span.ty(),
            |ctxt| {
                visitor.visit_ty(ctxt, ty);
            },
        )
    }
}

pub fn walk_variant_def_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyVariantDefListSpan<'db>>,
    variants: VariantDefListId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    let ItemKind::Enum(enum_) = ctxt.scope().item() else {
        unreachable!()
    };
    for (idx, variant) in variants.data(ctxt.db).iter().enumerate() {
        ctxt.with_new_scoped_ctxt(
            ScopeId::Variant(EnumVariant::new(enum_, idx)),
            |span| span.variant(idx),
            |ctxt| {
                visitor.visit_variant_def(ctxt, variant);
            },
        )
    }
}

pub fn walk_variant_def<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyVariantDefSpan<'db>>,
    variant: &VariantDef<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(name) = variant.name.to_opt() {
        ctxt.with_new_ctxt(
            |span| span.name(),
            |ctxt| {
                visitor.visit_ident(ctxt, name);
            },
        )
    }

    match variant.kind {
        VariantKind::Unit => {}
        VariantKind::Tuple(t) => ctxt.with_new_ctxt(
            |span| span.tuple_type(),
            |ctxt| visitor.visit_tuple_type(ctxt, t),
        ),

        VariantKind::Record(fields) => ctxt.with_new_ctxt(
            |span| span.fields(),
            |ctxt| visitor.visit_field_def_list(ctxt, fields),
        ),
    }
}

pub fn walk_path<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>,
    path: PathId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    walk_path_impl(visitor, ctxt, path);
}

fn walk_path_impl<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>,
    path: PathId<'db>,
) -> usize
where
    V: Visitor<'db> + ?Sized,
{
    let idx = if let Some(parent) = path.parent(ctxt.db()) {
        1 + walk_path_impl(visitor, ctxt, parent)
    } else {
        0
    };
    match path.kind(ctxt.db) {
        PathKind::Ident { .. } => {
            if let Some(ident) = path.ident(ctxt.db).to_opt() {
                ctxt.with_new_ctxt(
                    |span| span.segment(idx).into_atom(),
                    |ctxt| {
                        visitor.visit_ident(ctxt, ident);
                    },
                );
            }
        }
        PathKind::QualifiedType { type_, trait_ } => {
            ctxt.with_new_ctxt(
                |span| span.segment(idx).qualified_type().ty(),
                |ctxt| {
                    visitor.visit_ty(ctxt, type_);
                },
            );
            ctxt.with_new_ctxt(
                |span| span.segment(idx).qualified_type().trait_qualifier(),
                |ctxt| {
                    visitor.visit_trait_ref(ctxt, trait_);
                },
            );
        }
    }
    let generic_args = path.generic_args(ctxt.db);
    ctxt.with_new_ctxt(
        |span| span.segment(idx).generic_args(),
        |ctxt| {
            visitor.visit_generic_arg_list(ctxt, generic_args);
        },
    );
    idx
}

pub fn walk_use_path<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyUsePathSpan<'db>>,
    path: UsePathId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    for (i, segment) in path.data(ctxt.db).iter().enumerate() {
        if let Some(UsePathSegment::Ident(ident)) = segment.to_opt() {
            ctxt.with_new_ctxt(
                |span| span.segment(i).into_atom(),
                |ctxt| {
                    visitor.visit_ident(ctxt, ident);
                },
            )
        }
    }
}

pub fn walk_type<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>,
    ty: TypeId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    match ty.data(ctxt.db) {
        TypeKind::Ptr(ty) => {
            if let Some(ty) = ty.to_opt() {
                ctxt.with_new_ctxt(
                    |ctxt| ctxt.into_ptr_type().pointee(),
                    |ctxt| {
                        visitor.visit_ty(ctxt, ty);
                    },
                )
            }
        }

        TypeKind::Path(path) => ctxt.with_new_ctxt(
            |span| span.into_path_type(),
            |ctxt| {
                if let Some(path) = path.to_opt() {
                    ctxt.with_new_ctxt(|span| span.path(), |ctxt| visitor.visit_path(ctxt, path));
                }
            },
        ),

        TypeKind::Tuple(t) => ctxt.with_new_ctxt(
            |span| span.into_tuple_type(),
            |ctxt| walk_tuple_type(visitor, ctxt, *t),
        ),

        TypeKind::Array(elem, body) => ctxt.with_new_ctxt(
            |span| span.into_array_type(),
            |ctxt| {
                if let Some(elem) = elem.to_opt() {
                    ctxt.with_new_ctxt(
                        |span| span.elem(),
                        |ctxt| {
                            visitor.visit_ty(ctxt, elem);
                        },
                    )
                }
                if let Some(body) = body.to_opt() {
                    visitor.visit_body(&mut VisitorCtxt::with_body(ctxt.db, body), body);
                }
            },
        ),

        TypeKind::Never => {}
    }
}

pub fn walk_tuple_type<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyTupleTypeSpan<'db>>,
    ty: TupleTypeId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    for (i, elem) in ty.data(ctxt.db()).iter().enumerate() {
        let Some(elem) = elem.to_opt() else {
            continue;
        };
        ctxt.with_new_ctxt(
            |span| span.elem_ty(i),
            |ctxt| {
                visitor.visit_ty(ctxt, elem);
            },
        )
    }
}

pub fn walk_type_bound_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyTypeBoundListSpan<'db>>,
    bounds: &[TypeBound<'db>],
) where
    V: Visitor<'db> + ?Sized,
{
    for (idx, bound) in bounds.iter().enumerate() {
        ctxt.with_new_ctxt(
            |span| span.bound(idx),
            |ctxt| {
                visitor.visit_type_bound(ctxt, bound);
            },
        )
    }
}

pub fn walk_type_bound<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyTypeBoundSpan<'db>>,
    bound: &TypeBound<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    match bound {
        TypeBound::Trait(trait_ref) => ctxt.with_new_ctxt(
            |span| span.trait_bound(),
            |ctxt| visitor.visit_trait_ref(ctxt, *trait_ref),
        ),
        TypeBound::Kind(Partial::Present(kind_bound)) => ctxt.with_new_ctxt(
            |span| span.kind_bound(),
            |ctxt| {
                visitor.visit_kind_bound(ctxt, kind_bound);
            },
        ),
        _ => {}
    }
}

pub fn walk_trait_ref<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
    trait_ref: TraitRefId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(path) = trait_ref.path(ctxt.db()).to_opt() {
        ctxt.with_new_ctxt(
            |span| span.path(),
            |ctxt| {
                visitor.visit_path(ctxt, path);
            },
        )
    }
}

pub fn walk_super_trait_list<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazySuperTraitListSpan<'db>>,
    super_traits: &[TraitRefId<'db>],
) where
    V: Visitor<'db> + ?Sized,
{
    for (idx, super_trait) in super_traits.iter().enumerate() {
        ctxt.with_new_ctxt(
            |span| span.super_trait(idx),
            |ctxt| {
                visitor.visit_trait_ref(ctxt, *super_trait);
            },
        )
    }
}

pub fn walk_kind_bound<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyKindBoundSpan<'db>>,
    bound: &KindBound,
) where
    V: Visitor<'db> + ?Sized,
{
    let KindBound::Abs(lhs, rhs) = bound else {
        return;
    };

    if let Partial::Present(lhs) = lhs {
        ctxt.with_new_ctxt(
            |span| span.abs().lhs(),
            |ctxt| {
                visitor.visit_kind_bound(ctxt, lhs.as_ref());
            },
        )
    }

    if let Partial::Present(rhs) = rhs {
        ctxt.with_new_ctxt(
            |span| span.abs().rhs(),
            |ctxt| {
                visitor.visit_kind_bound(ctxt, rhs.as_ref());
            },
        )
    }
}

pub fn walk_where_clause<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyWhereClauseSpan<'db>>,
    predicates: WhereClauseId<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    for (idx, predicate) in predicates.data(ctxt.db).iter().enumerate() {
        ctxt.with_new_ctxt(
            |span| span.predicate(idx),
            |ctxt| {
                visitor.visit_where_predicate(ctxt, predicate);
            },
        )
    }
}

pub fn walk_where_predicate<'db, V>(
    visitor: &mut V,
    ctxt: &mut VisitorCtxt<'db, LazyWherePredicateSpan<'db>>,
    predicate: &WherePredicate<'db>,
) where
    V: Visitor<'db> + ?Sized,
{
    if let Some(ty) = predicate.ty.to_opt() {
        ctxt.with_new_ctxt(
            |span| span.ty(),
            |ctxt| {
                visitor.visit_ty(ctxt, ty);
            },
        )
    }

    ctxt.with_new_ctxt(
        |span| span.bounds(),
        |ctxt| {
            visitor.visit_type_bound_list(ctxt, &predicate.bounds);
        },
    )
}

use attr::{Attr, AttrListId};

/// [`VisitorCtxt`] is used to track the span information and the scope of the
/// current node being visited.
/// The context is updated automatically when entering a new node. Thus, the
/// user need to only construct the context when invoking a visitor.
pub struct VisitorCtxt<'db, T>
where
    T: LazySpan,
{
    db: &'db dyn HirDb,
    span: DynLazySpan<'db>,
    scope_stack: Vec<ScopeId<'db>>,

    _t: PhantomData<T>,
}

impl<'db, T> VisitorCtxt<'db, T>
where
    T: LazySpan,
{
    pub fn new(db: &'db dyn HirDb, scope: ScopeId<'db>, span: T) -> Self
    where
        T: Into<DynLazySpan<'db>>,
    {
        Self {
            db,
            span: span.into(),
            scope_stack: vec![scope],
            _t: PhantomData,
        }
    }

    pub fn db(&self) -> &'db dyn HirDb {
        self.db
    }

    pub fn ingot(&self) -> Ingot<'db> {
        self.scope().ingot(self.db)
    }

    pub fn span(&self) -> Option<T>
    where
        T: SpanDowncast<'db>,
    {
        let dyn_span: DynLazySpan = self.span.clone();
        T::downcast(dyn_span)
    }

    pub fn scope(&self) -> ScopeId<'db> {
        *self.scope_stack.last().unwrap()
    }

    pub fn top_mod(&self) -> TopLevelMod<'db> {
        match self.span.0.as_ref().unwrap().root {
            ChainRoot::ItemKind(item) => item.top_mod(self.db),
            ChainRoot::TopMod(top_mod) => top_mod,
            ChainRoot::Mod(mod_) => mod_.top_mod(self.db),
            ChainRoot::Func(func) => func.top_mod(self.db),
            ChainRoot::Struct(struct_) => struct_.top_mod(self.db),
            ChainRoot::Contract(contract) => contract.top_mod(self.db),
            ChainRoot::Enum(enum_) => enum_.top_mod(self.db),
            ChainRoot::TypeAlias(alias) => alias.top_mod(self.db),
            ChainRoot::Impl(impl_) => impl_.top_mod(self.db),
            ChainRoot::Trait(trait_) => trait_.top_mod(self.db),
            ChainRoot::ImplTrait(impl_trait) => impl_trait.top_mod(self.db),
            ChainRoot::Const(const_) => const_.top_mod(self.db),
            ChainRoot::Use(use_) => use_.top_mod(self.db),
            ChainRoot::Body(body) => body.top_mod(self.db),
            ChainRoot::Stmt(_) | ChainRoot::Expr(_) | ChainRoot::Pat(_) => {
                self.body().top_mod(self.db)
            }
        }
    }

    /// Create a new context for visiting a pattern.
    /// `scope` is the scope that encloses the pattern.
    pub fn with_pat(db: &'db dyn HirDb, scope: ScopeId<'db>, body: Body<'db>, pat: PatId) -> Self {
        Self {
            db,
            span: LazyPatSpan::new(body, pat).into(),
            scope_stack: vec![scope],
            _t: PhantomData,
        }
    }

    /// Create a new context for visiting a statement.
    /// `scope` is the scope that encloses the statement.
    pub fn with_stmt(
        db: &'db dyn HirDb,
        scope: ScopeId<'db>,
        body: Body<'db>,
        stmt: StmtId,
    ) -> Self {
        Self {
            db,
            span: LazyStmtSpan::new(body, stmt).into(),
            scope_stack: vec![scope],
            _t: PhantomData,
        }
    }

    /// Create a new context for visiting an expression.
    /// `scope` is the scope that encloses the expression.
    pub fn with_expr(
        db: &'db dyn HirDb,
        scope: ScopeId<'db>,
        body: Body<'db>,
        expr: ExprId,
    ) -> Self {
        let scope_id = match expr.data(db, body) {
            Partial::Present(Expr::Block(_)) => ScopeId::Block(body, expr),
            _ => scope,
        };

        Self {
            db,
            span: LazyExprSpan::new(body, expr).into(),
            scope_stack: vec![scope_id],
            _t: PhantomData,
        }
    }

    /// Returns the body that encloses the current node.
    /// # panic
    /// Panics when the current node is not enclosed by a body.
    pub fn body(&self) -> Body<'db> {
        match self.span.0.as_ref().unwrap().root {
            ChainRoot::Body(body) => body,
            ChainRoot::Expr(expr) => expr.body,
            ChainRoot::Stmt(stmt) => stmt.body,
            ChainRoot::Pat(pat) => pat.body,
            _ => panic!(),
        }
    }

    fn with_new_scoped_ctxt<F1, F2, U>(&mut self, scope_id: ScopeId<'db>, f1: F1, f2: F2)
    where
        T: SpanDowncast<'db>,
        F1: FnOnce(T) -> U,
        F2: FnOnce(&mut VisitorCtxt<'db, U>),
        U: LazySpan + SpanDowncast<'db> + Into<DynLazySpan<'db>>,
    {
        self.scope_stack.push(scope_id);
        self.with_new_ctxt(f1, f2);
        self.scope_stack.pop();
    }

    fn with_new_ctxt<F1, F2, U>(&mut self, f1: F1, f2: F2)
    where
        T: SpanDowncast<'db>,
        F1: FnOnce(T) -> U,
        F2: FnOnce(&mut VisitorCtxt<'db, U>),
        U: LazySpan + SpanDowncast<'db> + Into<DynLazySpan<'db>>,
    {
        let chain_len = self.span.0.as_ref().unwrap().len();
        let mut new_ctxt = self.transition(f1);

        f2(&mut new_ctxt);

        let n_pop = new_ctxt.span.0.as_ref().unwrap().len() - chain_len;
        *self = new_ctxt.pop(n_pop);
    }

    fn transition<F, U>(&mut self, f: F) -> VisitorCtxt<'db, U>
    where
        T: SpanDowncast<'db>,
        F: FnOnce(T) -> U,
        U: LazySpan + SpanDowncast<'db> + Into<DynLazySpan<'db>>,
    {
        let dyn_span = mem::replace(&mut self.span, DynLazySpan::invalid());
        let scope_stack = mem::take(&mut self.scope_stack);
        let span = T::downcast(dyn_span).unwrap();
        let u = f(span);

        Self {
            db: self.db,
            span: u.into(),
            scope_stack,
            _t: PhantomData,
        }
        .cast()
    }

    fn pop<U>(mut self, n_pop: usize) -> VisitorCtxt<'db, U>
    where
        U: LazySpan,
    {
        for _ in 0..n_pop {
            self.span.0.as_mut().unwrap().pop_transition();
        }

        Self {
            db: self.db,
            span: self.span,
            scope_stack: self.scope_stack,
            _t: PhantomData,
        }
        .cast()
    }

    fn cast<U: LazySpan>(self) -> VisitorCtxt<'db, U> {
        VisitorCtxt {
            db: self.db,
            span: self.span,
            scope_stack: self.scope_stack,
            _t: PhantomData,
        }
    }
}

macro_rules! define_item_ctxt_ctor {
    ($((
        $span_ty:ty,
        $ctor:ident($ctor_name:ident: $ctor_ty:ty)),)*) => {
        $(impl<'db> VisitorCtxt<'db, $span_ty> {
            /// Create a new [`VisitorCtxt`] with the given item as the root of the span chain.
            pub fn $ctor(db: &'db dyn HirDb, $ctor_name: $ctor_ty) -> Self {
                Self {
                    db,
                    span: <$span_ty>::new($ctor_name).into(),
                    scope_stack: vec![$ctor_name.scope()],
                    _t: PhantomData,
                }
            }
        })*
    };
}

define_item_ctxt_ctor! {
    (LazyItemSpan<'db>, with_item(item: ItemKind<'db>)),
    (LazyTopModSpan<'db>, with_top_mod(top_mod: TopLevelMod<'db>)),
    (LazyModSpan<'db>, with_mod(mod_: Mod<'db>)),
    (LazyFuncSpan<'db>, with_func(func: Func<'db>)),
    (LazyStructSpan<'db>, with_struct(struct_: Struct<'db>)),
    (LazyContractSpan<'db>, with_contract(contract: Contract<'db>)),
    (LazyEnumSpan<'db>, with_enum(enum_: Enum<'db>)),
    (LazyTypeAliasSpan<'db>, with_type_alias(type_alias: TypeAlias<'db>)),
    (LazyImplSpan<'db>, with_impl(impl_: Impl<'db>)),
    (LazyTraitSpan<'db>, with_trait(trait_: Trait<'db>)),
    (LazyImplTraitSpan<'db>, with_impl_trait(impl_trait: ImplTrait<'db>)),
    (LazyConstSpan<'db>, with_const(const_: Const<'db>)),
    (LazyUseSpan<'db>, with_use(use_: Use<'db>)),
    (LazyBodySpan<'db>, with_body(body: Body<'db>)),
}

macro_rules! visit_node_in_body {
    ($visitor:expr,  $ctxt:expr,  $id:expr, $inner:ident) => {
        if let Partial::Present(data) = $id.data($ctxt.db, $ctxt.body()) {
            let scope = *$ctxt.scope_stack.last().unwrap();
            paste::paste! {
                $visitor.[<visit_ $inner>](&mut VisitorCtxt::[<with_ $inner>]($ctxt.db, scope, $ctxt.body(), *$id), *$id, data);

            }
        }
    }
}
use common::ingot::Ingot;
use visit_node_in_body;

#[cfg(test)]
mod tests {

    use super::*;
    use crate::test_db::TestDb;
    struct MyVisitor<'db> {
        generic_param_list: Option<LazyGenericParamListSpan<'db>>,
        attributes: Vec<LazyAttrSpan<'db>>,
        lit_ints: Vec<LazyLitSpan<'db>>,
    }

    impl<'db> Visitor<'db> for MyVisitor<'db> {
        fn visit_attribute(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyAttrSpan<'db>>,
            _attrs: &Attr<'db>,
        ) {
            self.attributes.push(ctxt.span().unwrap());
        }

        fn visit_generic_param_list(
            &mut self,
            ctxt: &mut VisitorCtxt<'db, LazyGenericParamListSpan<'db>>,
            _params: GenericParamListId<'db>,
        ) {
            self.generic_param_list = Some(ctxt.span().unwrap());
        }

        fn visit_lit(&mut self, ctxt: &mut VisitorCtxt<'db, LazyLitSpan<'db>>, lit: LitKind<'db>) {
            if let LitKind::Int(_) = lit {
                self.lit_ints.push(ctxt.span().unwrap());
            }
        }
    }

    #[test]
    fn visitor() {
        let mut db = TestDb::default();
        let text = r#"
            #[attr1]
            #[attr2]
            fn foo<T: 'static, V: Add>() {
                1
                "foo"
                42
            }"#;

        let file = db.standalone_file(text);

        let func = db.expect_item::<Func>(file);
        let top_mod = func.top_mod(&db);

        let mut visitor = MyVisitor {
            generic_param_list: None,
            attributes: Vec::new(),
            lit_ints: Vec::new(),
        };

        let mut ctxt = VisitorCtxt::with_func(&db, func);
        visitor.visit_func(&mut ctxt, func);

        assert_eq!(
            "<T: 'static, V: Add>",
            db.text_at(top_mod, &visitor.generic_param_list.unwrap())
        );

        assert_eq!(visitor.attributes.len(), 2);
        assert_eq!("#[attr1]", db.text_at(top_mod, &visitor.attributes[0]));
        assert_eq!("#[attr2]", db.text_at(top_mod, &visitor.attributes[1]));

        assert_eq!(visitor.lit_ints.len(), 2);
        assert_eq!("1", db.text_at(top_mod, &visitor.lit_ints[0]));
        assert_eq!("42", db.text_at(top_mod, &visitor.lit_ints[1]));
    }
}
