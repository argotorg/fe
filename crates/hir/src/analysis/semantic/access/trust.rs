//! Use-based trust for the unsafe code of dependencies.
//!
//! An ingot's unsafe code is trusted when the ingot is the user's own code
//! (a trust root), the embedded `core` or `std`, or a dependency that a trusted ingot
//! declares with `allow_unsafe = true`. Only use counts: the semantic access
//! walk starts from the bodies of a trust root's modules, follows calls into
//! every ingot, including indexing's generated `locate` calls and the bodies
//! of the contracts a body creates, and reports an untrusted ingot's unsafe
//! code, an `unsafe` block, an `unsafe fn` or a method of an `unsafe impl`,
//! at the call in the user's code that first reaches it. An untrusted ingot's safe API stays
//! usable without trust.
//!
//! Unsafe code is accepted only if every ingot that contributed to it is
//! trusted. Code belongs to the ingot that wrote its syntax. Generated code is
//! written by the package of the quotation that holds its text, although
//! preparation publishes it into the package that asked for it, so the package
//! that asked is not a contributor. A fragment that a quotation splices keeps
//! its writer, and each quotation around it placed the fragment into the code
//! that runs, so the writer and every placer must be trusted for an `unsafe`
//! block in it. The same holds for each operation that needs an unsafe
//! context, such as a dereference: the packages of the operation's own text
//! must be trusted, whoever wrote the block that allows it. An `unsafe impl` asserts all of its members, whoever wrote
//! them, so the ingot that wrote the `unsafe impl` is the one that must be
//! trusted for them.
//!
//! This is build policy, not typing: whether a caller builds depends on what
//! the dependency's bodies contain, but nothing it means does.
use std::{cell::OnceCell, collections::HashSet};

use common::{
    diagnostics::{
        CompleteDiagnostic, DiagnosticPass, GlobalErrorCode, LabelStyle, Severity, SubDiagnostic,
    },
    ingot::{Ingot, IngotKind},
    stdlib::is_embedded_builtin,
};
use rustc_hash::FxHashSet;
use url::Url;

use super::EntryKind;

use crate::{
    analysis::{
        HirAnalysisDb,
        diagnostics::{DiagnosticVoucher, SpannedHirAnalysisDb},
        semantic::{SemOrigin, SemanticInstance, diagnostics::SemanticDiagnosticSpan},
        ty::{
            evidence::{Evidence, EvidenceId},
            trait_def::ImplementorOrigin,
            ty_check::{BodyOwner, infer_body},
            unsafe_check::UnsafeOpSite,
        },
    },
    hir_def::{BlockKind, Expr, HirIngot, ImplTrait, ItemKind, Partial, TopLevelMod},
    span::LazySpan,
};

/// The ingots whose unsafe code may run, for a walk from a trust root.
pub(super) struct UnsafeTrust<'db> {
    trusted: HashSet<Url>,
    root: Ingot<'db>,
    /// The `unsafe impl`s of the program that an untrusted ingot wrote or
    /// placed, found when first asked.
    untrusted_impls: OnceCell<FxHashSet<ImplTrait<'db>>>,
}

impl<'db> UnsafeTrust<'db> {
    /// The trust for a walk from `top_mod`, if its ingot is a trust root.
    pub(super) fn for_root(db: &'db dyn HirAnalysisDb, top_mod: TopLevelMod<'db>) -> Option<Self> {
        let graph = db.dependency_graph();
        let root = top_mod.ingot(db);
        graph.is_trust_root(db, root.base(db)).then(|| Self {
            trusted: graph.trusted_ingots(db),
            root,
            untrusted_impls: OnceCell::new(),
        })
    }

    fn allows(&self, db: &dyn HirAnalysisDb, ingot: Ingot<'_>) -> bool {
        // A package is not `core` or `std` because its manifest says so: the
        // embedded libraries are trusted through the dependency graph, as
        // the members of a checked workspace are.
        let url = ingot.base(db);
        ingot.kind(db) == IngotKind::StandAlone
            || self.trusted.contains(url)
            || !db.dependency_graph().contains_url(db, url)
    }

    /// Whether every ingot that contributed to `owner` is trusted: the one
    /// that wrote its body, and the ones that wrote or placed its unsafe code
    /// (see `owner_site`). A body with an untrusted contributor is not the
    /// trusted code's own: a call into it is judged, at the call.
    pub(super) fn trusts_authors_of(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
    ) -> bool {
        self.allows(db, author_ingot(db, owner)) && self.owner_site(db, owner).is_none()
    }

    /// The first ingot of `chain` that is not trusted. A chain lists the
    /// modules of the quotations that wrote and placed code: the writer
    /// first, the outermost placer last.
    fn first_untrusted(
        &self,
        db: &'db dyn HirAnalysisDb,
        chain: &[TopLevelMod<'db>],
    ) -> Option<Ingot<'db>> {
        chain
            .iter()
            .map(|top_mod| top_mod.ingot(db))
            .find(|ingot| !self.allows(db, *ingot))
    }

    /// The `unsafe impl`s of the program, the root ingot and the ingots it
    /// depends on, that an untrusted ingot wrote or placed. Empty in the
    /// common case, which lets the walk skip every proof it would read.
    fn untrusted_impls(&self, db: &'db dyn HirAnalysisDb) -> &FxHashSet<ImplTrait<'db>> {
        self.untrusted_impls.get_or_init(|| {
            let graph = db.dependency_graph();
            let root_url = self.root.base(db).clone();
            let urls =
                std::iter::once(root_url.clone()).chain(graph.dependency_urls(db, &root_url));
            let mut found = FxHashSet::default();
            for url in urls {
                let Some(ingot) = db.workspace().containing_ingot(db, url) else {
                    continue;
                };
                if is_embedded_builtin(ingot.base(db)) {
                    continue;
                }
                for implementation in ingot.all_impl_traits(db) {
                    if implementation.is_unsafe(db)
                        && self
                            .first_untrusted(db, &impl_chain(db, *implementation))
                            .is_some()
                    {
                        found.insert(*implementation);
                    }
                }
            }
            found
        })
    }

    /// The `unsafe impl`s that the untrusted ingots wrote or placed and that
    /// `instance` relies on: those that prove one of the assumptions its body
    /// is checked under, a bound of the function, of its implementation or of
    /// its trait, `Self: Trait` for a member of a trait, and recursively the
    /// implementations selected for the conditions of each. The proofs are
    /// the ones the instance's body stores (`TypedBody::instance_bound_evidence`,
    /// bound where the instance is made), the ones it runs with, so an
    /// implementation that a `with` choice replaced is not reached. They are
    /// read, not proved again. An assumption of a generic caller proves nothing here: it is
    /// judged at the call that supplies the implementation.
    fn relied_unsafe_impls(
        &self,
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> Vec<ImplTrait<'db>> {
        let untrusted = self.untrusted_impls(db);
        if untrusted.is_empty() {
            return Vec::new();
        }
        let Some(bound) = instance.key(db).typed_body(db).instance_bound_evidence() else {
            return Vec::new();
        };
        let mut pending: Vec<EvidenceId<'db>> = bound
            .entries
            .entries(db)
            .iter()
            .flatten()
            .copied()
            .collect();
        let mut relied = Vec::new();
        while let Some(proof) = pending.pop() {
            if let Evidence::Impl {
                selected, conds, ..
            } = proof.data(db)
            {
                if let ImplementorOrigin::Hir(implementation) = selected.origin(db)
                    && untrusted.contains(&implementation)
                    && !relied.contains(&implementation)
                {
                    relied.push(implementation);
                }
                pending.extend(conds.iter().copied());
            }
        }
        relied
    }

    /// The unsafe code `instance` runs, or relies on, and the first untrusted
    /// ingot that contributed to it: the first of its owner's obligations
    /// (`owner_site`), else the first `unsafe impl` it relies on whose chain
    /// has an untrusted ingot. A method of an `unsafe impl` is reported as
    /// that, so a call gets one error.
    pub(super) fn untrusted_site(
        &self,
        db: &'db dyn HirAnalysisDb,
        instance: SemanticInstance<'db>,
    ) -> Option<(TrustSite<'db>, Ingot<'db>)> {
        self.owner_site(db, instance.key(db).owner(db)).or_else(|| {
            self.relied_unsafe_impls(db, instance)
                .into_iter()
                .find_map(|implementation| {
                    let ingot = self.first_untrusted(db, &impl_chain(db, implementation))?;
                    Some((TrustSite::UnsafeImpl(implementation), ingot))
                })
        })
    }

    /// The first obligation of `owner`'s own body (see `obligations`) whose
    /// chain has an untrusted ingot.
    fn owner_site(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
    ) -> Option<(TrustSite<'db>, Ingot<'db>)> {
        self.obligations(db, owner)
            .into_iter()
            .find_map(|(site, chain)| {
                Some((TrustSite::Origin(site), self.first_untrusted(db, &chain)?))
            })
    }

    /// The places where `owner` relies on an unsafe assertion, each with the
    /// modules of the quotations that wrote and placed the asserting text,
    /// writer first. Each is unsafe code of every package of its chain:
    /// - all of the body, for a method of an `unsafe impl`, whose body is what
    ///   the impl asserts correct (even if a provider of a trusted ingot
    ///   supplied it); the chain is the impl's;
    /// - all of the body, for an `unsafe fn`;
    /// - each `unsafe` block, in any position of the body. Each block has its
    ///   own chain: a fragment spliced into a body keeps the writer of its
    ///   text, and the quotations around the fragment are its placers;
    /// - each operation that needs an unsafe context (a dereference of a
    ///   pointer, a call of an `unsafe fn`, an `unsafe` field), found by the
    ///   unsafe checker. Its chain is that of the operation's own text, which
    ///   may differ from the chain of the context that allows it.
    fn obligations(
        &self,
        db: &'db dyn HirAnalysisDb,
        owner: BodyOwner<'db>,
    ) -> Vec<(SemOrigin<'db>, Vec<TopLevelMod<'db>>)> {
        let Some(body) = owner.body(db) else {
            return Vec::new();
        };
        let mut obligations = Vec::new();
        if let Some(chain) = unsafe_impl_chain(db, owner) {
            obligations.push((SemOrigin::Body(owner), chain));
        }
        if matches!(owner, BodyOwner::Func(func, _) if func.is_unsafe(db)) {
            obligations.push((SemOrigin::Body(owner), body.quote_chain(db)));
        }
        for (expr, data) in body.exprs(db).iter() {
            if matches!(data, Partial::Present(Expr::Block(_, BlockKind::Unsafe))) {
                obligations.push((SemOrigin::Expr(expr), body.expr_quote_chain(db, expr)));
            }
        }
        let typed_body = &infer_body(db, owner).1;
        for site in typed_body.discharged_unsafe_ops() {
            obligations.push(match *site {
                UnsafeOpSite::Expr(expr) => {
                    (SemOrigin::Expr(expr), body.expr_quote_chain(db, expr))
                }
                UnsafeOpSite::Pat(pat) => (SemOrigin::Pat(pat), body.pat_quote_chain(db, pat)),
            });
        }
        obligations
    }
}

/// The ingot that wrote the syntax of `owner`: the one that holds it, except
/// for generated code, which the package of the quotation wrote.
fn author_ingot<'db>(db: &'db dyn HirAnalysisDb, owner: BodyOwner<'db>) -> Ingot<'db> {
    owner.scope().syntax_top_mod(db).ingot(db)
}

/// The modules of the quotations that wrote and placed the `unsafe impl` that
/// `owner` is a member of, if it is one, writer first: a method of it, the
/// value of one of its associated constants, or the default of a constant that
/// it inherits. A header is written by the ingot that holds it, though a
/// provider of another ingot supplies its members.
fn unsafe_impl_chain<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
) -> Option<Vec<TopLevelMod<'db>>> {
    let impl_trait = match owner {
        BodyOwner::Func(func, _) => match func.scope().parent_item(db) {
            Some(ItemKind::ImplTrait(impl_trait)) => impl_trait,
            _ => return None,
        },
        BodyOwner::AnonConstBody { body, .. } => match body.scope().parent_item(db) {
            Some(ItemKind::ImplTrait(impl_trait))
                if impl_trait
                    .hir_consts(db)
                    .iter()
                    .any(|assoc| assoc.value == Partial::Present(body)) =>
            {
                impl_trait
            }
            _ => return None,
        },
        // The default of a trait's constant that an implementation inherits is
        // the implementation's own value of the constant where it is read.
        BodyOwner::InheritedConst { implementation, .. } => implementation,
        _ => return None,
    };
    impl_trait.is_unsafe(db).then(|| impl_chain(db, impl_trait))
}

/// The modules of the quotations that wrote and placed `impl_trait`, writer
/// first and the outermost placer last; the module that holds the syntax of a
/// hand-written one.
fn impl_chain<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_trait: ImplTrait<'db>,
) -> Vec<TopLevelMod<'db>> {
    match impl_trait.quote_chain(db) {
        chain if chain.is_empty() => vec![impl_trait.scope().syntax_top_mod(db)],
        chain => chain,
    }
}

/// The unsafe code that a call reaches: a place in the body of the called
/// function, or an `unsafe impl` that the call relies on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum TrustSite<'db> {
    Origin(SemOrigin<'db>),
    UnsafeImpl(ImplTrait<'db>),
}

/// A call in the user's code reaches unsafe code in a dependency that no
/// trusted ingot allows to use it.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct UntrustedUnsafeUse<'db> {
    /// The trust root whose code makes the call.
    pub root: Ingot<'db>,
    /// The call in the trust root's code.
    pub call: SemanticDiagnosticSpan<'db>,
    /// Why `call` stands for the run of a generated body that no call in the
    /// code names, such as a `#[test]` function that `fe test` runs.
    pub entry: Option<EntryKind>,
    /// The function whose body holds the unsafe code.
    pub user: BodyOwner<'db>,
    pub site: TrustSite<'db>,
    /// The untrusted ingot that wrote the unsafe code.
    pub author: Ingot<'db>,
}

impl DiagnosticVoucher for UntrustedUnsafeUse<'_> {
    fn to_complete(&self, db: &dyn SpannedHirAnalysisDb) -> CompleteDiagnostic {
        let ingot = self.author;
        let name = ingot_name(db, ingot.base(db));
        let (secondary_text, secondary) = match self.site {
            TrustSite::Origin(origin) => (
                format!("unsafe code in `{name}`"),
                SemanticDiagnosticSpan::Origin {
                    owner: self.user,
                    origin,
                }
                .resolve(db),
            ),
            TrustSite::UnsafeImpl(implementation) => (
                format!("`unsafe impl` that `{name}` wrote or placed"),
                implementation.span().resolve(db),
            ),
        };
        let (title, primary) = if let Some(entry) = self.entry {
            let what = match entry {
                EntryKind::Test => "test",
                EntryKind::Export => "export",
                EntryKind::Main => "entry point",
                EntryKind::Contract => "contract",
            };
            (
                format!(
                    "generated {what} runs unsafe code in `{name}`, which is not allowed to use it"
                ),
                format!("this generated {what} runs unsafe code in `{name}`"),
            )
        } else if matches!(self.site, TrustSite::UnsafeImpl(_)) {
            (
                format!(
                    "call relies on an `unsafe impl` that `{name}` wrote, which is not allowed to use unsafe code"
                ),
                format!("this call relies on an `unsafe impl` of `{name}`"),
            )
        } else {
            (
                format!("call reaches unsafe code in `{name}`, which is not allowed to use it"),
                format!("this call reaches unsafe code in `{name}`"),
            )
        };
        CompleteDiagnostic::new(
            Severity::Error,
            title,
            vec![
                SubDiagnostic::new(LabelStyle::Primary, primary, self.call.resolve(db)),
                SubDiagnostic::new(LabelStyle::Secondary, secondary_text, secondary),
            ],
            vec![trust_fix(db, self.root, ingot, &name)],
            GlobalErrorCode::new(DiagnosticPass::SemanticAccess, 15),
        )
    }
}

/// How to trust `ingot`: allow it where the root depends on it, or trust a
/// dependent that already allows it.
fn trust_fix(
    db: &dyn SpannedHirAnalysisDb,
    root: Ingot<'_>,
    ingot: Ingot<'_>,
    name: &str,
) -> String {
    let graph = db.dependency_graph();
    let dependents = graph.dependents(db, ingot.base(db));
    let root = root.base(db);
    if dependents.iter().any(|(dependent, _)| dependent == root) {
        return format!(
            "to trust `{name}`, add `allow_unsafe = true` to its entry under `[dependencies]` in fe.toml"
        );
    }
    let trusted = graph.trusted_ingots(db);
    let vouchers: Vec<_> = dependents
        .iter()
        .filter(|(dependent, allows)| *allows && !trusted.contains(dependent))
        .map(|(dependent, _)| format!("`{}`", ingot_name(db, dependent)))
        .collect();
    if vouchers.is_empty() {
        format!(
            "`{name}` is trusted once an ingot you trust depends on it with `allow_unsafe = true`, \
                 or once you add it to `[dependencies]` with `allow_unsafe = true`"
        )
    } else {
        format!(
            "{} allows `{name}` to use unsafe code but is not trusted itself; \
                 trust it with `allow_unsafe = true` where you depend on it",
            vouchers.join(", ")
        )
    }
}

fn ingot_name(db: &dyn HirAnalysisDb, url: &Url) -> String {
    db.workspace()
        .containing_ingot(db, url.clone())
        .and_then(|ingot| ingot.config(db).as_ref()?.metadata.name.clone())
        .map_or_else(|| url.to_string(), |name| name.to_string())
}
