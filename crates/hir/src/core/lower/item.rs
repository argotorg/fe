use parser::ast::{self, prelude::*};

use super::{FileLowerCtxt, body::BodyCtxt};
use crate::{
    hir_def::{
        AttrListId, Body, BodyKind, EffectParamListId, Expr, FuncParamListId, GenericParamListId,
        IdentId, Pat, PathId, TraitRefId, TupleTypeId, TypeBound, TypeId, WhereClauseId, item::*,
    },
    span::HirOrigin,
};

pub(crate) fn lower_module_items(ctxt: &mut FileLowerCtxt<'_>, items: ast::ItemList) {
    for item in items {
        ItemKind::lower_ast(ctxt, item);
    }
}

impl<'db> ItemKind<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Item) {
        let Some(kind) = ast.kind() else {
            return;
        };

        match kind {
            ast::ItemKind::Mod(mod_) => {
                Mod::lower_ast(ctxt, mod_);
            }
            ast::ItemKind::Func(fn_) => {
                Func::lower_ast(ctxt, fn_);
            }
            ast::ItemKind::Struct(struct_) => {
                Struct::lower_ast(ctxt, struct_);
            }
            ast::ItemKind::Contract(contract) => {
                Contract::lower_ast(ctxt, contract);
            }
            ast::ItemKind::Enum(enum_) => {
                Enum::lower_ast(ctxt, enum_);
            }
            ast::ItemKind::Msg(msg) => {
                Msg::lower_ast(ctxt, msg);
            }
            ast::ItemKind::TypeAlias(alias) => {
                TypeAlias::lower_ast(ctxt, alias);
            }
            ast::ItemKind::Impl(impl_) => {
                Impl::lower_ast(ctxt, impl_);
            }
            ast::ItemKind::Trait(trait_) => {
                Trait::lower_ast(ctxt, trait_);
            }
            ast::ItemKind::ImplTrait(impl_trait) => {
                ImplTrait::lower_ast(ctxt, impl_trait);
            }
            ast::ItemKind::Const(const_) => {
                Const::lower_ast(ctxt, const_);
            }
            ast::ItemKind::Use(use_) => {
                Use::lower_ast(ctxt, use_);
            }
            ast::ItemKind::Extern(extern_) => {
                if let Some(extern_block) = extern_.extern_block() {
                    for fn_ in extern_block {
                        Func::lower_ast(ctxt, fn_);
                    }
                }
            }
        }
    }
}

impl<'db> Mod<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Mod) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Mod(name));
        ctxt.enter_item_scope(id, true);

        ctxt.insert_synthetic_prelude_use();

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        if let Some(items) = ast.items() {
            lower_module_items(ctxt, items);
        }

        let origin = HirOrigin::raw(&ast);
        let mod_ = Self::new(ctxt.db(), id, name, attributes, vis, ctxt.top_mod(), origin);
        ctxt.leave_item_scope(mod_)
    }
}

impl<'db> Func<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Func) -> Self {
        let sig = ast.sig();
        let name = IdentId::lower_token_partial(ctxt, sig.name());
        let id = ctxt.joined_id(TrackedItemVariant::Func(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, sig.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, sig.where_clause());
        let params = sig
            .params()
            .map(|params| FuncParamListId::lower_ast(ctxt, params))
            .into();
        let ret_ty = sig.ret_ty().map(|ty| TypeId::lower_ast(ctxt, ty));
        let effects = lower_effects(ctxt, &ast);
        let modifier = ItemModifier::lower_ast(ast.modifier());
        let body = ast
            .body()
            .map(|body| Body::lower_ast(ctxt, ast::Expr::cast(body.syntax().clone()).unwrap()));
        let origin = HirOrigin::raw(&ast);

        let fn_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            generic_params,
            where_clause,
            params,
            effects,
            ret_ty,
            modifier,
            body,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(fn_)
    }
}

fn lower_effects<'db>(ctxt: &mut FileLowerCtxt<'db>, ast: &ast::Func) -> EffectParamListId<'db> {
    use crate::hir_def::{EffectParam, EffectParamListId};

    let mut data: Vec<EffectParam<'db>> = Vec::new();

    if let Some(uses) = ast.sig().uses_clause() {
        if let Some(list) = uses.param_list() {
            for p in list {
                let name = p.name().map(|n| IdentId::lower_token(ctxt, n.syntax()));

                let is_mut = p.mut_token().is_some();
                let key_path = p.path().map(|path| PathId::lower_ast(ctxt, path)).into();

                data.push(EffectParam {
                    name,
                    key_path,
                    is_mut,
                });
            }
        } else if let Some(p) = uses.param() {
            let name = p.name().map(|n| IdentId::lower_token(ctxt, n.syntax()));
            let is_mut = p.mut_token().is_some();
            let key_path = p.path().map(|path| PathId::lower_ast(ctxt, path)).into();

            data.push(EffectParam {
                name,
                key_path,
                is_mut,
            });
        }
    }

    EffectParamListId::new(ctxt.db(), data)
}

impl<'db> Struct<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Struct) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Struct(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let fields = FieldDefListId::lower_ast_opt(ctxt, ast.fields());
        let origin = HirOrigin::raw(&ast);

        let struct_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            where_clause,
            fields,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(struct_)
    }
}

impl<'db> Contract<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Contract) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Contract(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let fields = FieldDefListId::lower_contract_fields_opt(ctxt, ast.fields());
        // Contract-level uses clause
        let effects = lower_uses_clause_opt(ctxt, ast.uses_clause());

        // Optional init block
        let (init_params, init_effects, init_body) = if let Some(init_ast) = ast.init_block() {
            let params = init_ast
                .params()
                .map(|p| FuncParamListId::lower_ast(ctxt, p));
            let effects = lower_uses_clause_opt(ctxt, init_ast.uses_clause());
            let body = init_ast
                .body()
                .map(|b| Body::lower_ast(ctxt, ast::Expr::cast(b.syntax().clone()).unwrap()));
            (params, effects, body)
        } else {
            (None, EffectParamListId::new(ctxt.db(), vec![]), None)
        };

        // Recv blocks (message handlers)
        let recvs = {
            let mut data = Vec::new();
            for (recv_idx, r) in ast.recvs().enumerate() {
                let msg_path = r.path().map(|p| PathId::lower_ast(ctxt, p));
                let arms = r
                    .arms()
                    .map(|arms| ContractRecvArmListId::lower_ast(ctxt, recv_idx, arms))
                    .unwrap_or_else(|| ContractRecvArmListId::new(ctxt.db(), vec![]));
                data.push(ContractRecv { msg_path, arms });
            }
            ContractRecvListId::new(ctxt.db(), data)
        };
        let origin = HirOrigin::raw(&ast);

        let contract = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            fields,
            effects,
            init_params,
            init_effects,
            init_body,
            recvs,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(contract)
    }
}

impl<'db> Msg<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Msg) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Msg(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let variants = MsgVariantListId::lower_ast_opt(ctxt, ast.variants());
        let origin = HirOrigin::raw(&ast);

        let msg = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            variants,
            ctxt.top_mod(),
            origin,
        );

        ctxt.leave_item_scope(msg)
    }
}

fn lower_uses_clause_opt<'db>(
    ctxt: &mut FileLowerCtxt<'db>,
    uses: Option<ast::UsesClause>,
) -> EffectParamListId<'db> {
    use crate::hir_def::{EffectParam, EffectParamListId};

    let mut data: Vec<EffectParam<'db>> = Vec::new();

    if let Some(uses) = uses {
        if let Some(list) = uses.param_list() {
            for p in list {
                let name = p.name().map(|n| IdentId::lower_token(ctxt, n.syntax()));
                let is_mut = p.mut_token().is_some();
                let key_path = p.path().map(|path| PathId::lower_ast(ctxt, path)).into();
                data.push(EffectParam {
                    name,
                    key_path,
                    is_mut,
                });
            }
        } else if let Some(p) = uses.param() {
            let name = p.name().map(|n| IdentId::lower_token(ctxt, n.syntax()));
            let is_mut = p.mut_token().is_some();
            let key_path = p.path().map(|path| PathId::lower_ast(ctxt, path)).into();
            data.push(EffectParam {
                name,
                key_path,
                is_mut,
            });
        }
    }

    EffectParamListId::new(ctxt.db(), data)
}

impl<'db> Enum<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Enum) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Enum(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let variants = VariantDefListId::lower_ast_opt(ctxt, ast.variants());
        let origin = HirOrigin::raw(&ast);

        let enum_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            where_clause,
            variants,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(enum_)
    }
}

impl<'db> TypeAlias<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TypeAlias) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.alias());
        let id = ctxt.joined_id(TrackedItemVariant::TypeAlias(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let origin = HirOrigin::raw(&ast);

        let alias = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            ty,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(alias)
    }
}

impl<'db> Impl<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Impl) -> Self {
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let id = ctxt.joined_id(TrackedItemVariant::Impl(ty));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let origin = HirOrigin::raw(&ast);

        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                Func::lower_ast(ctxt, impl_item);
            }
        }

        let impl_ = Self::new(
            ctxt.db(),
            id,
            ty,
            attributes,
            generic_params,
            where_clause,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(impl_)
    }
}

impl<'db> Trait<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Trait) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Trait(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let super_traits = if let Some(super_traits) = ast.super_trait_list() {
            super_traits
                .into_iter()
                .map(|trait_ref| TraitRefId::lower_ast(ctxt, trait_ref))
                .collect()
        } else {
            vec![]
        };
        let origin = HirOrigin::raw(&ast);

        let mut types = vec![];
        let mut consts = vec![];

        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                match impl_item.kind() {
                    ast::TraitItemKind::Func(func) => {
                        Func::lower_ast(ctxt, func);
                    }
                    ast::TraitItemKind::Type(t) => types.push(AssocTyDecl::lower_ast(ctxt, t)),
                    ast::TraitItemKind::Const(c) => {
                        consts.push(AssocConstDecl::lower_ast(ctxt, c));
                    }
                };
            }
        }

        let trait_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            vis,
            generic_params,
            super_traits,
            where_clause,
            types,
            consts,
            ctxt.top_mod(),
            origin,
        );

        ctxt.leave_item_scope(trait_)
    }
}

impl<'db> AssocTyDecl<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitTypeItem) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let bounds = ast
            .bounds()
            .map(|bounds| {
                bounds
                    .into_iter()
                    .map(|bound| TypeBound::lower_ast(ctxt, bound))
                    .collect()
            })
            .unwrap_or_default();

        let default = TypeId::lower_ast_partial(ctxt, ast.ty()).to_opt();

        AssocTyDecl {
            name,
            bounds,
            default,
        }
    }
}

impl<'db> ImplTrait<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::ImplTrait) -> Self {
        let trait_ref = TraitRefId::lower_ast_partial(ctxt, ast.trait_ref());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let id = ctxt.joined_id(TrackedItemVariant::ImplTrait(trait_ref, ty));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());
        let where_clause = WhereClauseId::lower_ast_opt(ctxt, ast.where_clause());
        let origin = HirOrigin::raw(&ast);

        let mut types = vec![];
        let mut consts = vec![];
        if let Some(item_list) = ast.item_list() {
            for impl_item in item_list {
                match impl_item.kind() {
                    ast::TraitItemKind::Func(func) => {
                        Func::lower_ast(ctxt, func);
                    }
                    ast::TraitItemKind::Type(t) => types.push(AssocTyDef::lower_ast(ctxt, t)),
                    ast::TraitItemKind::Const(c) => {
                        consts.push(AssocConstDef::lower_ast(ctxt, c));
                    }
                };
            }
        }

        let impl_trait = Self::new(
            ctxt.db(),
            id,
            trait_ref,
            ty,
            attributes,
            generic_params,
            where_clause,
            types,
            consts,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(impl_trait)
    }
}

impl<'db> AssocTyDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitTypeItem) -> Self {
        AssocTyDef {
            name: IdentId::lower_token_partial(ctxt, ast.name()),
            type_ref: TypeId::lower_ast_partial(ctxt, ast.ty()),
        }
    }
}

impl<'db> AssocConstDecl<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitConstItem) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let default = ast
            .value()
            .map(|expr| crate::hir_def::Partial::Present(Body::lower_ast(ctxt, expr)));
        AssocConstDecl { name, ty, default }
    }
}

impl<'db> AssocConstDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitConstItem) -> Self {
        let value = ast.value().map(|expr| Body::lower_ast(ctxt, expr)).into();

        AssocConstDef {
            name: IdentId::lower_token_partial(ctxt, ast.name()),
            ty: TypeId::lower_ast_partial(ctxt, ast.ty()),
            value,
        }
    }
}

impl<'db> Const<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::Const) -> Self {
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let id = ctxt.joined_id(TrackedItemVariant::Const(name));
        ctxt.enter_item_scope(id, false);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let body = ast.value().map(|ast| Body::lower_ast(ctxt, ast)).into();
        let vis = ItemModifier::lower_ast(ast.modifier()).to_visibility();
        let origin = HirOrigin::raw(&ast);

        let const_ = Self::new(
            ctxt.db(),
            id,
            name,
            attributes,
            ty,
            body,
            vis,
            ctxt.top_mod(),
            origin,
        );
        ctxt.leave_item_scope(const_)
    }
}

impl ItemModifier {
    pub(super) fn lower_ast(ast: Option<ast::ItemModifier>) -> Self {
        let Some(ast) = ast else {
            return Self::None;
        };

        match (ast.pub_kw().is_some(), ast.unsafe_kw().is_some()) {
            (true, true) => Self::PubAndUnsafe,
            (true, false) => Self::Pub,
            (false, true) => Self::Unsafe,
            (false, false) => Self::None,
        }
    }
}

impl<'db> FieldDefListId<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::RecordFieldDefList) -> Self {
        let fields = ast
            .into_iter()
            .map(|field| FieldDef::lower_ast(ctxt, field))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), fields)
    }

    fn lower_ast_opt(ctxt: &mut FileLowerCtxt<'db>, ast: Option<ast::RecordFieldDefList>) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or(Self::new(ctxt.db(), Vec::new()))
    }

    fn lower_contract_fields_opt(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::ContractFields>,
    ) -> Self {
        match ast {
            Some(cf) => {
                let fields = cf
                    .into_iter()
                    .map(|field| FieldDef::lower_ast(ctxt, field))
                    .collect::<Vec<_>>();
                Self::new(ctxt.db(), fields)
            }
            None => Self::new(ctxt.db(), Vec::new()),
        }
    }

    fn lower_msg_params_opt(
        ctxt: &mut FileLowerCtxt<'db>,
        ast: Option<ast::MsgVariantParams>,
    ) -> Self {
        match ast {
            Some(params) => {
                let fields = params
                    .into_iter()
                    .map(|field| FieldDef::lower_ast(ctxt, field))
                    .collect::<Vec<_>>();
                Self::new(ctxt.db(), fields)
            }
            None => Self::new(ctxt.db(), Vec::new()),
        }
    }
}

impl<'db> FieldDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::RecordFieldDef) -> Self {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let type_ref = TypeId::lower_ast_partial(ctxt, ast.ty());
        let vis = if ast.pub_kw().is_some() {
            Visibility::Public
        } else {
            Visibility::Private
        };

        Self::new(attributes, name, type_ref, vis)
    }
}

impl<'db> VariantDefListId<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::VariantDefList) -> Self {
        let variants = ast
            .into_iter()
            .map(|variant| VariantDef::lower_ast(ctxt, variant))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), variants)
    }

    fn lower_ast_opt(ctxt: &mut FileLowerCtxt<'db>, ast: Option<ast::VariantDefList>) -> Self {
        ast.map(|ast| Self::lower_ast(ctxt, ast))
            .unwrap_or(Self::new(ctxt.db(), Vec::new()))
    }
}

impl<'db> VariantDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::VariantDef) -> Self {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let kind = match ast.kind() {
            ast::VariantKind::Unit => VariantKind::Unit,
            ast::VariantKind::Tuple(t) => VariantKind::Tuple(TupleTypeId::lower_ast(ctxt, t)),
            ast::VariantKind::Record(r) => VariantKind::Record(FieldDefListId::lower_ast(ctxt, r)),
        };

        Self {
            attributes,
            name,
            kind,
        }
    }
}

impl<'db> MsgVariantListId<'db> {
    fn lower_ast_opt(ctxt: &mut FileLowerCtxt<'db>, ast: Option<ast::MsgVariantList>) -> Self {
        match ast {
            Some(list) => {
                let variants = list
                    .into_iter()
                    .map(|variant| MsgVariantDef::lower_ast(ctxt, variant))
                    .collect::<Vec<_>>();
                Self::new(ctxt.db(), variants)
            }
            None => Self::new(ctxt.db(), Vec::new()),
        }
    }
}

impl<'db> MsgVariantDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::MsgVariant) -> Self {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let params = FieldDefListId::lower_msg_params_opt(ctxt, ast.params());
        let ret_ty = ast.ret_ty().map(|ty| TypeId::lower_ast(ctxt, ty));

        Self {
            attributes,
            name,
            params,
            ret_ty,
        }
    }
}

impl<'db> ContractRecvArmListId<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, recv_idx: usize, ast: ast::RecvArmList) -> Self {
        let arms = ast
            .into_iter()
            .enumerate()
            .map(|(idx, arm)| ContractRecvArm::lower_ast(ctxt, recv_idx, idx, arm))
            .collect::<Vec<_>>();
        Self::new(ctxt.db(), arms)
    }
}

impl<'db> ContractRecvArm<'db> {
    fn lower_ast(
        ctxt: &mut FileLowerCtxt<'db>,
        recv_idx: usize,
        arm_idx: usize,
        ast: ast::RecvArm,
    ) -> Self {
        let variant = TrackedItemVariant::ContractRecvArm {
            recv_idx: recv_idx as u32,
            arm_idx: arm_idx as u32,
        };
        let id = ctxt.joined_id(variant);
        let mut body_ctxt = BodyCtxt::new(ctxt, id);

        let pat = Pat::lower_ast_opt(&mut body_ctxt, ast.pat());
        let body_ast = ast
            .body()
            .map(|b| ast::Expr::cast(b.syntax().clone()).unwrap());
        let body_expr = Expr::push_to_body_opt(&mut body_ctxt, body_ast.clone());
        let body = body_ctxt.build(body_ast.as_ref(), body_expr, BodyKind::FuncBody);
        let ret_ty = ast.ret_ty().map(|ty| TypeId::lower_ast(ctxt, ty));
        let effects = lower_uses_clause_opt(ctxt, ast.uses_clause());

        ContractRecvArm {
            pat,
            ret_ty,
            effects,
            body,
        }
    }
}
