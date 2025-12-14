use common::{
    diagnostics::{Span, SpanKind},
    file::File,
};
use parser::{
    FeLang, SyntaxNode, SyntaxToken, TextRange, ast, ast::prelude::*, syntax_node::NodeOrToken,
};
use salsa::Update;
use thin_vec::ThinVec;

use super::{
    DesugaredOrigin, DesugaredUseFocus, HirOrigin, LazySpan, UseDesugared, body_ast, const_ast,
    contract_ast, enum_ast, expr::ExprRoot, func_ast, impl_ast, impl_trait_ast, mod_ast,
    pat::PatRoot, stmt::StmtRoot, struct_ast, trait_ast, type_alias_ast, use_ast,
};
use crate::{
    HirDb, SpannedHirDb,
    hir_def::{
        Body, Const, Contract, Enum, Func, Impl, ImplTrait, ItemKind, Mod, Struct, TopLevelMod,
        Trait, TypeAlias, Use,
    },
    lower::top_mod_ast,
};

/// This type represents function from the hir origin to another hir origin to
/// identify the span of HIR node. `LazyTransitionFn` is regarded as a closure
/// that takes a `HirOrigin` and [`LazyArg`], `LazyArg` is considered as
/// captured variables.
/// The reason why we use `LazyTransitionFn` instead of `dyn
/// Fn` is that we want to make all types that use `LazyTransitionFn` to be
/// `Clone` and `Eq`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[allow(unpredictable_function_pointer_comparisons)]
pub(crate) struct LazyTransitionFn {
    pub(super) f: fn(ResolvedOrigin, LazyArg) -> ResolvedOrigin,
    pub(super) arg: LazyArg,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub(crate) enum LazyArg {
    Idx(usize),
    None,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Update)]
pub(crate) struct SpanTransitionChain<'db> {
    pub(crate) root: ChainRoot<'db>,
    pub(super) chain: ThinVec<LazyTransitionFn>,
}

impl<'db> SpanTransitionChain<'db> {
    pub(crate) fn pop_transition(&mut self) {
        self.chain.pop();
    }

    pub(crate) fn len(&self) -> usize {
        self.chain.len()
    }

    pub(super) fn new(root: impl Into<ChainRoot<'db>>) -> Self {
        Self {
            root: root.into(),
            chain: ThinVec::new(),
        }
    }

    pub(super) fn top_mod(&self, db: &'db dyn HirDb) -> TopLevelMod<'db> {
        match self.root {
            ChainRoot::ItemKind(item) => item.top_mod(db),
            ChainRoot::TopMod(top_mod) => top_mod,
            ChainRoot::Mod(m) => m.top_mod(db),
            ChainRoot::Func(f) => f.top_mod(db),
            ChainRoot::Struct(s) => s.top_mod(db),
            ChainRoot::Contract(c) => c.top_mod(db),
            ChainRoot::Enum(e) => e.top_mod(db),
            ChainRoot::TypeAlias(t) => t.top_mod(db),
            ChainRoot::Impl(i) => i.top_mod(db),
            ChainRoot::Trait(t) => t.top_mod(db),
            ChainRoot::ImplTrait(i) => i.top_mod(db),
            ChainRoot::Const(c) => c.top_mod(db),
            ChainRoot::Use(u) => u.top_mod(db),
            ChainRoot::Body(b) => b.top_mod(db),
            ChainRoot::Stmt(s) => s.body.top_mod(db),
            ChainRoot::Expr(e) => e.body.top_mod(db),
            ChainRoot::Pat(p) => p.body.top_mod(db),
        }
    }

    pub(super) fn push(&mut self, transition: LazyTransitionFn) {
        self.chain.push(transition);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug, derive_more::From, Update)]
pub(crate) enum ChainRoot<'db> {
    ItemKind(ItemKind<'db>),
    TopMod(TopLevelMod<'db>),
    Mod(Mod<'db>),
    Func(Func<'db>),
    Struct(Struct<'db>),
    Contract(Contract<'db>),
    Enum(Enum<'db>),
    TypeAlias(TypeAlias<'db>),
    Impl(Impl<'db>),
    Trait(Trait<'db>),
    ImplTrait(ImplTrait<'db>),
    Const(Const<'db>),
    Use(Use<'db>),
    Body(Body<'db>),
    Stmt(StmtRoot<'db>),
    Expr(ExprRoot<'db>),
    Pat(PatRoot<'db>),
}

#[derive(Debug, Clone)]
pub(crate) struct ResolvedOrigin {
    pub(crate) file: File,
    pub(crate) kind: ResolvedOriginKind,
}

impl ResolvedOrigin {
    pub(crate) fn new(file: File, kind: ResolvedOriginKind) -> Self {
        Self { file, kind }
    }

    pub(crate) fn resolve<T>(
        db: &dyn SpannedHirDb,
        top_mod: TopLevelMod,
        origin: &HirOrigin<T>,
    ) -> ResolvedOrigin
    where
        T: AstNode<Language = FeLang>,
    {
        let root = top_mod_ast(db, top_mod).syntax().clone();
        let kind = match origin {
            HirOrigin::Raw(ptr) => ResolvedOriginKind::Node(ptr.syntax_node_ptr().to_node(&root)),
            HirOrigin::Expanded(ptr) => ResolvedOriginKind::Expanded(ptr.to_node(&root)),
            HirOrigin::Desugared(desugared) => {
                ResolvedOriginKind::Desugared(root, desugared.clone())
            }
            HirOrigin::Synthetic | HirOrigin::None => ResolvedOriginKind::None,
        };

        ResolvedOrigin::new(top_mod.file(db), kind)
    }

    pub(crate) fn map<F>(self, f: F) -> Self
    where
        F: FnOnce(SyntaxNode) -> Option<NodeOrToken>,
    {
        let kind = match self.kind {
            ResolvedOriginKind::Node(node) => match f(node) {
                Some(NodeOrToken::Node(node)) => ResolvedOriginKind::Node(node),
                Some(NodeOrToken::Token(token)) => ResolvedOriginKind::Token(token),
                None => ResolvedOriginKind::None,
            },
            kind => kind,
        };

        ResolvedOrigin {
            file: self.file,
            kind,
        }
    }

    pub(crate) fn map_desugared<F>(self, f: F) -> Self
    where
        F: FnOnce(SyntaxNode, DesugaredOrigin) -> ResolvedOriginKind,
    {
        let kind = match self.kind {
            ResolvedOriginKind::Desugared(root, desugared) => f(root, desugared),
            kind => kind,
        };

        ResolvedOrigin {
            file: self.file,
            kind,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum ResolvedOriginKind {
    Node(SyntaxNode),
    Token(SyntaxToken),
    Expanded(SyntaxNode),
    Desugared(SyntaxNode, DesugaredOrigin),
    None,
}

impl ChainInitiator for ChainRoot<'_> {
    fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin {
        match self {
            Self::ItemKind(kind) => match kind {
                ItemKind::TopMod(top_mod) => top_mod.init(db),
                ItemKind::Mod(mod_) => mod_.init(db),
                ItemKind::Func(func) => func.init(db),
                ItemKind::Struct(struct_) => struct_.init(db),
                ItemKind::Contract(contract) => contract.init(db),
                ItemKind::Enum(enum_) => enum_.init(db),
                ItemKind::TypeAlias(type_alias) => type_alias.init(db),
                ItemKind::Impl(impl_) => impl_.init(db),
                ItemKind::Trait(trait_) => trait_.init(db),
                ItemKind::ImplTrait(impl_trait) => impl_trait.init(db),
                ItemKind::Const(const_) => const_.init(db),
                ItemKind::Use(use_) => use_.init(db),
                ItemKind::Body(body) => body.init(db),
            },
            Self::TopMod(top_mod) => top_mod.init(db),
            Self::Mod(mod_) => mod_.init(db),
            Self::Func(func) => func.init(db),
            Self::Struct(struct_) => struct_.init(db),
            Self::Contract(contract) => contract.init(db),
            Self::Enum(enum_) => enum_.init(db),
            Self::TypeAlias(type_alias) => type_alias.init(db),
            Self::Impl(impl_) => impl_.init(db),
            Self::Trait(trait_) => trait_.init(db),
            Self::ImplTrait(impl_trait) => impl_trait.init(db),
            Self::Const(const_) => const_.init(db),
            Self::Use(use_) => use_.init(db),
            Self::Body(body) => body.init(db),
            Self::Stmt(stmt) => stmt.init(db),
            Self::Expr(expr) => expr.init(db),
            Self::Pat(pat) => pat.init(db),
        }
    }
}

impl LazySpan for SpanTransitionChain<'_> {
    fn resolve(&self, db: &dyn SpannedHirDb) -> Option<Span> {
        let mut resolved = self.root.init(db);

        for LazyTransitionFn { f, arg } in &self.chain {
            resolved = f(resolved, *arg);
        }

        Some(match resolved.kind {
            ResolvedOriginKind::Node(node) => {
                Span::new(resolved.file, node.text_range(), SpanKind::Original)
            }
            ResolvedOriginKind::Token(token) => {
                Span::new(resolved.file, token.text_range(), SpanKind::Original)
            }
            ResolvedOriginKind::Expanded(node) => {
                Span::new(resolved.file, node.text_range(), SpanKind::Expanded)
            }
            ResolvedOriginKind::Desugared(root, desugared) => {
                desugared.resolve(db, root, resolved.file)
            }
            ResolvedOriginKind::None => return None,
        })
    }
}

/// A trait for types that can be used as the root of a `SpanTransitionChain`.
pub(crate) trait ChainInitiator {
    /// Returns the `ResolvedOrigin` for the root of the chain.
    fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin;
}

impl ChainInitiator for TopLevelMod<'_> {
    fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin {
        let file = self.file(db);
        let ast = top_mod_ast(db, *self);
        ResolvedOrigin::new(file, ResolvedOriginKind::Node(ast.syntax().clone()))
    }
}

macro_rules! impl_chain_root {
    ($(($ty:ty, $fn:ident),)*) => {
        $(
        impl<'db> ChainInitiator for $ty {
            fn init(&self, db: &dyn crate::SpannedHirDb) -> ResolvedOrigin {
                let top_mod = self.top_mod(db);
                let origin = $fn(db, *self);
                ResolvedOrigin::resolve(db, top_mod, origin)
            }
        })*
    };
}

impl_chain_root! {
    (Mod<'db>, mod_ast),
    (Func<'db>, func_ast),
    (Struct<'db>, struct_ast),
    (Contract<'db>, contract_ast),
    (Enum<'db>, enum_ast),
    (TypeAlias<'db>, type_alias_ast),
    (Impl<'db>, impl_ast),
    (Trait<'db>, trait_ast),
    (ImplTrait<'db>, impl_trait_ast),
    (Const<'db>, const_ast),
    (Use<'db>, use_ast),
    (Body<'db>, body_ast),
}

macro_rules! define_lazy_span_node {
    (
        $name:ident
        $(,
            $sk_node: ty
            $(,
                $(@token {$(($name_token:ident, $getter_token:ident),)*})?
                $(@node {$(($name_node:ident, $getter_node:ident, $result:tt),)*})?
                $(@idx { $(($name_iter:ident, $result_iter:tt),)*})?
                $(,)?
            )?
        )?
    ) => {
        #[derive(Clone, PartialEq, Eq, Hash, Debug, salsa::Update)]
        pub struct $name<'db>(pub(crate) crate::span::transition::SpanTransitionChain<'db>);
        $(
            $(
            impl<'db> $name<'db> {

            pub fn top_mod(&self, db: &'db dyn crate::HirDb) -> Option<crate::hir_def::TopLevelMod<'db>> {
                Some(self.0.top_mod(db))
            }

            $($(
                paste::paste! {
                    pub fn $name_token(mut self) -> crate::span::LazySpanAtom<'db> {
                        use parser::ast::prelude::*;
                        fn f(origin: crate::span::transition::ResolvedOrigin, _: crate::span::transition::LazyArg) -> crate::span::transition::ResolvedOrigin {
                            origin.map(|node| <$sk_node as AstNode>::cast(node)
                                .and_then(|n| n.$getter_token())
                                .map(|n| n.into()))
                        }

                        let lazy_transition = crate::span::transition::LazyTransitionFn {
                            f,
                            arg: crate::span::transition::LazyArg::None,
                        };

                        self.0.push(lazy_transition);
                        crate::span::LazySpanAtom(self.0)
                    }
                }
            )*)?

            $($(
                paste::paste! {
                        pub fn $name_node(mut self) -> $result<'db> {
                        use parser::ast::prelude::*;

                        fn f(origin: crate::span::transition::ResolvedOrigin, _: crate::span::transition::LazyArg) -> crate::span::transition::ResolvedOrigin {
                            origin.map(|node| <$sk_node as AstNode>::cast(node)
                                .and_then(|n| n.$getter_node())
                                .map(|n| n.syntax().clone().into()))
                        }

                        let lazy_transition = crate::span::transition::LazyTransitionFn {
                            f,
                            arg: crate::span::transition::LazyArg::None,
                        };
                        self.0.push(lazy_transition);
                        $result(self.0)
                    }
                }
            )*)?

            $($(
                paste::paste! {
                    pub fn $name_iter(mut self, idx: usize) -> $result_iter<'db> {
                        use parser::ast::prelude::*;
                        fn f(origin: crate::span::transition::ResolvedOrigin, arg: crate::span::transition::LazyArg) -> crate::span::transition::ResolvedOrigin {
                            let idx = match arg {
                                crate::span::transition::LazyArg::Idx(idx) => idx,
                                _ => unreachable!(),
                            };

                            origin.map(|node| <$sk_node as AstNode>::cast(node)
                                .and_then(|f| f.into_iter().nth(idx))
                                .map(|n| n.syntax().clone().into()))
                        }

                        let lazy_transition = crate::span::transition::LazyTransitionFn {
                            f,
                            arg: crate::span::transition::LazyArg::Idx(idx),
                        };

                        self.0.push(lazy_transition);
                        $result_iter(self.0)
                    }
                }
            )*)?
        })?)?


        impl<'db> crate::span::LazySpan for $name<'db> {
            fn resolve(&self, db: &dyn crate::SpannedHirDb) -> Option<common::diagnostics::Span> {
                self.0.resolve(db)
            }
        }

        impl<'db> From<$name<'db>> for crate::span::DynLazySpan<'db> {
            fn from(val: $name<'db>) -> Self {
                Self(val.0.into())
            }
        }

        impl<'db> crate::span::SpanDowncast<'db> for $name<'db>  {
            fn downcast(val: crate::span::DynLazySpan<'db>) -> Option<Self> {
                val.0.map(|inner| Self(inner))
            }
        }
    };
}

impl DesugaredOrigin {
    fn resolve(self, _db: &dyn SpannedHirDb, root: SyntaxNode, file: File) -> Span {
        let range = match self {
            Self::Use(UseDesugared {
                root: use_root,
                path,
                alias,
                focus,
            }) => match focus {
                DesugaredUseFocus::Root => use_root.syntax_node_ptr().to_node(&root).text_range(),
                DesugaredUseFocus::Path => {
                    if let Some(first_seg) = path.first() {
                        let last_seg = path.last().unwrap();
                        TextRange::new(
                            first_seg
                                .syntax_node_ptr()
                                .to_node(&root)
                                .text_range()
                                .start(),
                            last_seg.syntax_node_ptr().to_node(&root).text_range().end(),
                        )
                    } else {
                        return Span::new(
                            file,
                            TextRange::new(0.into(), 0.into()),
                            SpanKind::NotFound,
                        );
                    }
                }
                DesugaredUseFocus::Alias => {
                    if let Some(alias) = alias {
                        alias.syntax_node_ptr().to_node(&root).text_range()
                    } else {
                        return Span::new(
                            file,
                            TextRange::new(0.into(), 0.into()),
                            SpanKind::NotFound,
                        );
                    }
                }
            },
            Self::Msg(super::MsgDesugared {
                msg,
                variant_idx,
                focus,
            }) => {
                use super::MsgDesugaredFocus;

                let msg_node = msg.to_node(&root);

                // If we have a variant index, try to get the specific variant
                let variant = variant_idx
                    .and_then(|idx| msg_node.variants().and_then(|v| v.into_iter().nth(idx)));

                match focus {
                    MsgDesugaredFocus::Block => {
                        // Point to the variant if available, otherwise the whole msg
                        if let Some(v) = variant {
                            v.syntax().text_range()
                        } else {
                            msg_node.syntax().text_range()
                        }
                    }
                    MsgDesugaredFocus::VariantName => {
                        // Point to the variant name token
                        if let Some(v) = variant
                            && let Some(name) = v.name()
                        {
                            return Span::new(file, name.text_range(), SpanKind::Original);
                        }
                        // Fallback to msg name
                        if let Some(name) = msg_node.name() {
                            name.text_range()
                        } else {
                            msg_node.syntax().text_range()
                        }
                    }
                    MsgDesugaredFocus::Selector => {
                        // Point to the selector attribute if present
                        if let Some(v) = &variant
                            && let Some(attr_list) = v.attr_list()
                        {
                            for attr in attr_list {
                                if let ast::AttrKind::Normal(normal) = attr.kind()
                                    && let Some(path) = normal.path()
                                    && path.text() == "selector"
                                {
                                    return Span::new(
                                        file,
                                        attr.syntax().text_range(),
                                        SpanKind::Original,
                                    );
                                }
                            }
                        }
                        // Fallback to variant name or msg
                        if let Some(v) = variant {
                            if let Some(name) = v.name() {
                                return Span::new(file, name.text_range(), SpanKind::Original);
                            }
                            v.syntax().text_range()
                        } else {
                            msg_node.syntax().text_range()
                        }
                    }
                }
            }
        };

        Span::new(file, range, SpanKind::Original)
    }
}

pub(super) use define_lazy_span_node;
