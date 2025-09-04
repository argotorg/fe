use parser::ast::{self, prelude::AstNode};

use super::{
    attr::LazyAttrListSpan,
    define_lazy_span_node,
    params::{LazyFuncParamListSpan, LazyGenericParamListSpan, LazyWhereClauseSpan},
    transition::SpanTransitionChain,
    types::{LazyTupleTypeSpan, LazyTySpan},
    use_tree::LazyUseAliasSpan,
};
use crate::{
    hir_def::{
        Body, Const, Contract, Enum, Func, Impl, ImplTrait, ItemKind, Mod, Struct, TopLevelMod,
        Trait, TypeAlias, Use,
    },
    span::{
        DesugaredOrigin, DesugaredUseFocus,
        params::LazyTraitRefSpan,
        transition::{LazyArg, LazyTransitionFn, ResolvedOrigin, ResolvedOriginKind},
        use_tree::LazyUsePathSpan,
    },
};

define_lazy_span_node!(LazyTopModSpan, ast::Root);
impl<'db> LazyTopModSpan<'db> {
    pub fn new(t: TopLevelMod<'db>) -> Self {
        Self(SpanTransitionChain::new(t))
    }
}

define_lazy_span_node!(LazyItemSpan);
impl<'db> LazyItemSpan<'db> {
    pub fn new(i: ItemKind<'db>) -> Self {
        Self(SpanTransitionChain::new(i))
    }
}

define_lazy_span_node!(
    LazyModSpan,
    ast::Mod,
    @token
    {
        (name, name),
    }
    @node
    {
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
    }
);
impl<'db> LazyModSpan<'db> {
    pub fn new(m: Mod<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(m))
    }
}

define_lazy_span_node!(
    LazyFuncSpan,
    ast::Func,
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (params, params, LazyFuncParamListSpan),
        (ret_ty, ret_ty, LazyTySpan),
    }
);
impl<'db> LazyFuncSpan<'db> {
    pub fn new(f: Func<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(f))
    }
}

define_lazy_span_node!(
    LazyStructSpan,
    ast::Struct,
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (fields, fields, LazyFieldDefListSpan),
    }
);
impl<'db> LazyStructSpan<'db> {
    pub fn new(s: Struct<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(s))
    }
}

define_lazy_span_node!(
    LazyContractSpan,
    ast::Contract,
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (fields, fields, LazyFieldDefListSpan),
    }
);
impl<'db> LazyContractSpan<'db> {
    pub fn new(c: Contract<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(c))
    }
}

define_lazy_span_node!(
    LazyEnumSpan,
    ast::Enum,
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (variants, variants, LazyVariantDefListSpan),
    }
);
impl<'db> LazyEnumSpan<'db> {
    pub fn new(e: Enum<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(e))
    }
}

define_lazy_span_node!(
    LazyTypeAliasSpan,
    ast::TypeAlias,
    @token {
        (alias, alias),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (ty, ty, LazyTySpan),
    }
);
impl<'db> LazyTypeAliasSpan<'db> {
    pub fn new(t: TypeAlias<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(t))
    }
}

define_lazy_span_node!(
    LazyImplSpan,
    ast::Impl,
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (target_ty, ty, LazyTySpan),
    }
);
impl<'db> LazyImplSpan<'db> {
    pub fn new(i: Impl<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(i))
    }
}

define_lazy_span_node!(
    LazyTraitSpan,
    ast::Trait,
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (super_traits, super_trait_list, LazySuperTraitListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (modifier, modifier, LazyItemModifierSpan),
        (item_list, item_list, LazyTraitItemListSpan),
    }
);
impl<'db> LazyTraitSpan<'db> {
    pub fn new(t: Trait<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(t))
    }
}

define_lazy_span_node!(
    LazySuperTraitListSpan,
    ast::SuperTraitList,
    @idx {
        (super_trait, LazyTraitRefSpan),
    }
);

define_lazy_span_node!(
    LazyTraitItemListSpan,
    ast::TraitItemList,
    @idx {
        (assoc_type, LazyTraitTypeSpan),
    }
);

define_lazy_span_node!(
    LazyTraitTypeSpan,
    ast::TraitTypeItem,
    @token {
        (name, name),
    }
    @node {
        (ty, ty, LazyTySpan),
        (attributes, attr_list, LazyAttrListSpan),
    }
);

define_lazy_span_node!(
    LazyImplTraitSpan,
    ast::ImplTrait,
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (generic_params, generic_params, LazyGenericParamListSpan),
        (where_clause, where_clause, LazyWhereClauseSpan),
        (trait_ref, trait_ref, LazyTraitRefSpan),
        (ty, ty, LazyTySpan),
        (item_list, item_list, LazyTraitItemListSpan),
    }
);
impl<'db> LazyImplTraitSpan<'db> {
    pub fn new(i: ImplTrait<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(i))
    }

    pub fn associated_type(self, idx: usize) -> LazyTraitTypeSpan<'db> {
        self.item_list().assoc_type(idx)
    }
}

define_lazy_span_node!(
    LazyConstSpan,
    ast::Const,
    @token {
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (ty, ty, LazyTySpan),
    }
);
impl<'db> LazyConstSpan<'db> {
    pub fn new(c: Const<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(c))
    }
}

define_lazy_span_node!(LazyUseSpan, ast::Use);
impl<'db> LazyUseSpan<'db> {
    pub fn new(u: Use<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(u))
    }

    pub fn path(mut self) -> LazyUsePathSpan<'db> {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin
                .map(|node| {
                    ast::Use::cast(node)
                        .and_then(|use_| use_.use_tree())
                        .and_then(|tree| tree.path())
                        .map(|n| n.syntax().clone().into())
                })
                .map_desugared(|root, desugared| match desugared {
                    DesugaredOrigin::Use(mut use_) => {
                        use_.focus = DesugaredUseFocus::Path;
                        ResolvedOriginKind::Desugared(root, DesugaredOrigin::Use(use_))
                    }
                })
        }

        let lazy_transition = LazyTransitionFn {
            f,
            arg: LazyArg::None,
        };

        self.0.push(lazy_transition);
        LazyUsePathSpan(self.0)
    }

    pub fn alias(mut self) -> LazyUseAliasSpan<'db> {
        fn f(origin: ResolvedOrigin, _: LazyArg) -> ResolvedOrigin {
            origin
                .map(|node| {
                    ast::Use::cast(node)
                        .and_then(|use_| use_.use_tree())
                        .and_then(|tree| tree.alias())
                        .map(|n| n.syntax().clone().into())
                })
                .map_desugared(|root, desugared| match desugared {
                    DesugaredOrigin::Use(mut use_) => {
                        use_.focus = DesugaredUseFocus::Alias;
                        ResolvedOriginKind::Desugared(root, DesugaredOrigin::Use(use_))
                    }
                })
        }

        let lazy_transition = LazyTransitionFn {
            f,
            arg: LazyArg::None,
        };

        self.0.push(lazy_transition);
        LazyUseAliasSpan(self.0)
    }
}

define_lazy_span_node!(LazyBodySpan, ast::Expr);
impl<'db> LazyBodySpan<'db> {
    pub fn new(b: Body<'db>) -> Self {
        Self(crate::span::transition::SpanTransitionChain::new(b))
    }
}

define_lazy_span_node!(
    LazyFieldDefListSpan,
    ast::RecordFieldDefList,
    @idx {
        (field, LazyFieldDefSpan),
    }
);

define_lazy_span_node!(
    LazyFieldDefSpan,
    ast::RecordFieldDef,
    @token {
        (pub_span, pub_kw),
        (name, name),
    }
    @node {
        (attributes, attr_list, LazyAttrListSpan),
        (ty, ty, LazyTySpan),
    }
);

define_lazy_span_node!(
    LazyVariantDefListSpan,
    ast::VariantDefList,
    @idx {
        (variant, LazyVariantDefSpan),
    }
);

define_lazy_span_node!(
    LazyVariantDefSpan,
    ast::VariantDef,
    @token {
        (name, name),
    }
    @node {
        (fields, fields, LazyFieldDefListSpan),
        (attributes, attr_list, LazyAttrListSpan),
        (tuple_type, tuple_type, LazyTupleTypeSpan),
    }
);

define_lazy_span_node!(
    LazyItemModifierSpan,
    ast::ItemModifier,
    @token {
        (pub_kw, pub_kw),
        (unsafe_kw, unsafe_kw),
    }
);

#[cfg(test)]
mod tests {

    use crate::{
        hir_def::{Enum, Func, Mod, Struct, TypeAlias, Use},
        test_db::TestDb,
    };

    #[test]
    fn top_mod_span() {
        let mut db = TestDb::default();

        let text = r#"
            mod foo {
                fn bar() {}
            }

            mod baz {
                fn qux() {}
            }
        "#;

        let file = db.standalone_file(text);
        let item_tree = db.parse_source(file);
        let top_mod = item_tree.top_mod;
        assert_eq!(text, db.text_at(top_mod, &top_mod.span()));
    }

    #[test]
    fn mod_span() {
        let mut db = TestDb::default();

        let text = r#"

            mod foo {
                fn bar() {}
            }
        "#;

        let file = db.standalone_file(text);
        let mod_ = db.expect_item::<Mod>(file);
        let top_mod = mod_.top_mod(&db);
        let mod_span = mod_.span();
        assert_eq!(
            r#"mod foo {
                fn bar() {}
            }"#,
            db.text_at(top_mod, &mod_span)
        );
        assert_eq!("foo", db.text_at(top_mod, &mod_span.name()));
    }

    #[test]
    fn fn_span() {
        let mut db = TestDb::default();

        let text = r#"
            fn my_func<T: Debug, U, const LEN: usize>(x: u32, label y: foo::Bar<2>) -> FooResult
                where U: Add
        "#;

        let file = db.standalone_file(text);
        let fn_ = db.expect_item::<Func>(file);
        let top_mod = fn_.top_mod(&db);
        assert_eq!("my_func", db.text_at(top_mod, &fn_.span().name()));

        let generic_params = fn_.span().generic_params();
        let type_generic_param_1 = generic_params.clone().param(0).into_type_param();
        let type_generic_param_2 = generic_params.clone().param(1).into_type_param();
        let const_generic_param = generic_params.clone().param(2).into_const_param();

        assert_eq!(
            "T",
            db.text_at(top_mod, &type_generic_param_1.clone().name())
        );
        assert_eq!(
            "Debug",
            db.text_at(top_mod, &type_generic_param_1.bounds().bound(0))
        );
        assert_eq!("U", db.text_at(top_mod, &type_generic_param_2.name()));
        assert_eq!(
            "const",
            db.text_at(top_mod, &const_generic_param.clone().const_token())
        );
        assert_eq!(
            "LEN",
            db.text_at(top_mod, &const_generic_param.clone().name())
        );
        assert_eq!("usize", db.text_at(top_mod, &const_generic_param.ty()));

        let param_1 = fn_.span().params().param(0);
        let param_2 = fn_.span().params().param(1);

        assert_eq!("x", db.text_at(top_mod, &param_1.clone().name()));
        assert_eq!("u32", db.text_at(top_mod, &param_1.ty()));
        assert_eq!("label", db.text_at(top_mod, &param_2.clone().label()));
        assert_eq!("foo::Bar<2>", db.text_at(top_mod, &param_2.ty()));

        assert_eq!("FooResult", db.text_at(top_mod, &fn_.span().ret_ty()));

        let where_clause = fn_.span().where_clause();
        let where_predicate = where_clause.clone().predicate(0);
        assert_eq!(
            "where",
            db.text_at(top_mod, &where_clause.clone().where_token())
        );
        assert_eq!("U", db.text_at(top_mod, &where_predicate.clone().ty()));
        assert_eq!(": Add", db.text_at(top_mod, &where_predicate.bounds()));
    }

    #[test]
    fn struct_span() {
        let mut db = TestDb::default();

        let text = r#"
            struct Foo {
                x: u32
                pub y: foo::Bar<2>
            }"#;

        let file = db.standalone_file(text);
        let struct_ = db.expect_item::<Struct>(file);
        let top_mod = struct_.top_mod(&db);
        assert_eq!("Foo", db.text_at(top_mod, &struct_.span().name()));

        let fields = struct_.span().fields();
        let field_1 = fields.clone().field(0);
        let field_2 = fields.clone().field(1);

        assert_eq!("x", db.text_at(top_mod, &field_1.clone().name()));
        assert_eq!("u32", db.text_at(top_mod, &field_1.ty()));

        assert_eq!("pub", db.text_at(top_mod, &field_2.clone().pub_span()));
        assert_eq!("y", db.text_at(top_mod, &field_2.clone().name()));
        assert_eq!("foo::Bar<2>", db.text_at(top_mod, &field_2.clone().ty()));
    }

    #[test]
    fn enum_span() {
        let mut db = TestDb::default();

        let text = r#"
            enum Foo {
                Bar
                Baz(u32, i32)
                Bux {
                    x: i8
                    y: u8
                }
            }"#;

        let file = db.standalone_file(text);
        let enum_ = db.expect_item::<Enum>(file);
        let top_mod = enum_.top_mod(&db);
        assert_eq!("Foo", db.text_at(top_mod, &enum_.span().name()));

        let variants = enum_.span().variants();
        let variant_1 = variants.clone().variant(0);
        let variant_2 = variants.clone().variant(1);
        let variant_3 = variants.clone().variant(2);

        assert_eq!("Bar", db.text_at(top_mod, &variant_1.clone().name()));
        assert_eq!("Baz", db.text_at(top_mod, &variant_2.clone().name()));
        assert_eq!("(u32, i32)", db.text_at(top_mod, &variant_2.tuple_type()));
        assert_eq!("Bux", db.text_at(top_mod, &variant_3.clone().name()));
        assert!(db.text_at(top_mod, &variant_3.fields()).contains("x: i8"));
    }

    #[test]
    fn type_alias_span() {
        let mut db = TestDb::default();

        let text = r#"
            pub type Foo = u32
        "#;

        let file = db.standalone_file(text);
        let alias = db.expect_item::<TypeAlias>(file);
        let top_mod = alias.top_mod(&db);
        assert_eq!("Foo", db.text_at(top_mod, &alias.span().alias()));
        assert_eq!("u32", db.text_at(top_mod, &alias.span().ty()));
        assert_eq!("pub", db.text_at(top_mod, &alias.span().modifier()));
    }

    #[test]
    fn use_span() {
        let mut db = TestDb::default();

        let text = r#"
            use foo::bar::baz::Trait as _
        "#;

        let file = db.standalone_file(text);
        let use_ = db.expect_item::<Use>(file);

        let top_mod = use_.top_mod(&db);
        assert_eq!("foo", db.text_at(top_mod, &use_.span().path().segment(0)));
        assert_eq!("bar", db.text_at(top_mod, &use_.span().path().segment(1)));
        assert_eq!("baz", db.text_at(top_mod, &use_.span().path().segment(2)));
        assert_eq!("Trait", db.text_at(top_mod, &use_.span().path().segment(3)));
        assert_eq!("as _", db.text_at(top_mod, &use_.span().alias()));
        assert_eq!("_", db.text_at(top_mod, &use_.span().alias().name()));
    }

    #[test]
    fn use_span_desugared() {
        let mut db = TestDb::default();

        let text = r#"
            use foo::bar::{baz::*, qux as Alias}
        "#;

        let file = db.standalone_file(text);
        let uses = db.expect_items::<Use>(file);
        assert_eq!(uses.len(), 2);

        let top_mod = uses[0].top_mod(&db);

        let use_ = uses[0];
        assert_eq!("foo", db.text_at(top_mod, &use_.span().path().segment(0)));
        assert_eq!("bar", db.text_at(top_mod, &use_.span().path().segment(1)));
        assert_eq!("baz", db.text_at(top_mod, &use_.span().path().segment(2)));
        assert_eq!("*", db.text_at(top_mod, &use_.span().path().segment(3)));

        let use_ = uses[1];
        assert_eq!("foo", db.text_at(top_mod, &use_.span().path().segment(0)));
        assert_eq!("bar", db.text_at(top_mod, &use_.span().path().segment(1)));
        assert_eq!("qux", db.text_at(top_mod, &use_.span().path().segment(2)));
        assert_eq!("as Alias", db.text_at(top_mod, &use_.span().alias()));
        assert_eq!("Alias", db.text_at(top_mod, &use_.span().alias().name()));
    }
}
