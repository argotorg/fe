use parser::ast::{self, prelude::*};

use super::FileLowerCtxt;
use crate::{
    hir_def::{
        AttrListId, Body, EffectParamListId, FuncParamListId, GenericParamListId, IdentId,
        Partial, PathId, TraitRefId, TupleTypeId, TypeBound, TypeId, WhereClauseId, item::*,
    },
    lower::msg::lower_msg_as_mod,
    span::HirOrigin,
};

/// Selector-related errors accumulated during msg block lowering.
#[salsa::accumulator]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SelectorError {
    pub kind: SelectorErrorKind,
    pub file: common::file::File,
    /// Range of the primary span (selector attribute or variant name)
    pub primary_range: parser::TextRange,
    /// Range of the secondary span (for duplicates - the first occurrence)
    pub secondary_range: Option<parser::TextRange>,
    pub variant_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SelectorErrorKind {
    /// Selector value overflows u32.
    Overflow,
    /// Selector has invalid type (string or bool).
    InvalidType,
    /// No `#[selector]` attribute found.
    Missing,
    /// Selector attribute has invalid form (e.g. `#[selector(value)]` instead of `#[selector = value]`).
    InvalidForm,
    /// Duplicate selector value.
    Duplicate {
        first_variant_name: String,
        selector: u32,
    },
}

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
                lower_msg_as_mod(ctxt, msg);
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
        let effects = lower_uses_clause_opt(ctxt, ast.sig().uses_clause());
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

pub(super) fn lower_uses_clause_opt<'db>(
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
        // Enter scope FIRST with a preliminary ID so that generic params are
        // available when lowering the target type. This is needed for const
        // generics in array types like `impl<const N: usize> Foo for [T; N]`.
        //
        // Note: the final impl id depends on the lowered `ty`, so we enter
        // scope with a temporary id and then swap in the real id once `ty` is
        // available. This keeps the final impl id stable (i.e. avoids
        // `... -> Impl(Absent) -> Impl(ty)`), while still ensuring that bodies
        // created during type lowering are nested under the impl scope.
        let parent_id = ctxt.current_id();
        let prelim_id = parent_id.join(ctxt.db(), TrackedItemVariant::Impl(Partial::Absent));
        ctxt.enter_item_scope(prelim_id, false);

        // Lower generic params first (now they're in scope for type lowering)
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());

        // Now lower the target type (const expressions can resolve generic params)
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());

        // Switch to the final id so nested items (e.g. methods) are keyed under
        // the real impl identity.
        let id = parent_id.join(ctxt.db(), TrackedItemVariant::Impl(ty));
        ctxt.set_current_id(id);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
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
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
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
            attributes,
            name,
            bounds,
            default,
        }
    }
}

impl<'db> ImplTrait<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::ImplTrait) -> Self {
        // Enter scope FIRST with a preliminary ID so that generic params are
        // available when lowering the trait ref and target type. This is
        // needed for const generics in array types like:
        // `impl<T, const N: usize> Seq<T> for [T; N]`.
        //
        // Like inherent impls, the final impl-trait id depends on the lowered
        // `trait_ref` and `ty`. Enter with a temporary id, then swap in the
        // final id once both are available to avoid
        // `... -> ImplTrait(Absent, Absent) -> ImplTrait(trait_ref, ty)`.
        let parent_id = ctxt.current_id();
        let prelim_id = parent_id.join(
            ctxt.db(),
            TrackedItemVariant::ImplTrait(Partial::Absent, Partial::Absent),
        );
        ctxt.enter_item_scope(prelim_id, false);

        // Lower generic params first (now they're in scope for type lowering)
        let generic_params = GenericParamListId::lower_ast_opt(ctxt, ast.generic_params());

        // Now lower trait ref and target type (const expressions can resolve generic params)
        let trait_ref = TraitRefId::lower_ast_partial(ctxt, ast.trait_ref());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());

        // Switch to the final id so nested items (e.g. methods) are keyed under
        // the real impl-trait identity.
        let id = parent_id.join(ctxt.db(), TrackedItemVariant::ImplTrait(trait_ref, ty));
        ctxt.set_current_id(id);

        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
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
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        AssocTyDef {
            attributes,
            name: IdentId::lower_token_partial(ctxt, ast.name()),
            type_ref: TypeId::lower_ast_partial(ctxt, ast.ty()),
        }
    }
}

impl<'db> AssocConstDecl<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitConstItem) -> Self {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let name = IdentId::lower_token_partial(ctxt, ast.name());
        let ty = TypeId::lower_ast_partial(ctxt, ast.ty());
        let default = ast
            .value()
            .map(|expr| crate::hir_def::Partial::Present(Body::lower_ast(ctxt, expr)));
        AssocConstDecl {
            attributes,
            name,
            ty,
            default,
        }
    }
}

impl<'db> AssocConstDef<'db> {
    fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::TraitConstItem) -> Self {
        let attributes = AttrListId::lower_ast_opt(ctxt, ast.attr_list());
        let value = ast.value().map(|expr| Body::lower_ast(ctxt, expr)).into();

        AssocConstDef {
            attributes,
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
}

impl<'db> FieldDef<'db> {
    pub(super) fn lower_ast(ctxt: &mut FileLowerCtxt<'db>, ast: ast::RecordFieldDef) -> Self {
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
