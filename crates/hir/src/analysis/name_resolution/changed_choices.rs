//! Generated implementations that would change a choice between implementations.
//!
//! Inside the package that generates them, generated items only fill gaps. A
//! name or a member that written code answers keeps that answer. What the
//! compiler decides by whether a type implements a trait (whether a value is
//! copied or moved, what runs where it goes out of use, how a stored value is
//! packed, which implementation a call of a value runs, which protocol a loop
//! runs, what data a revert carries) cannot be changed by a generated
//! implementation either, or the program would silently do something other
//! than what a reader of the written code expects. Those traits declare
//! `#[decides = "..."]` (see `presence_decision`), and a generated
//! implementation of one, or of any trait that a bound of a written
//! implementation of one names, directly or through the bounds of other written
//! implementations (`widened_presence`), is reported at the request that
//! generated it. Traits are compared by identity alone, so no reasoning about
//! when a bound can be met is needed.
//! That is decided from the definitions alone, whether or not written code
//! uses the type (`presence_changes`).
//!
//! A generated implementation can also turn a selection that was unique into
//! an ambiguity or an overlap: a second `Conv<u8>` for a type that has a
//! `Conv<u256>` leaves the inferred type of `conv_of(x)` open, and a generated
//! `Marker` for a type that a written blanket `impl<T: Marker> Tr for T`
//! covers makes it conflict with a written `impl Tr for X`. These are errors
//! where the selection is made (inference, coherence), with the message that
//! a written implementation would give. Generation never changes which
//! implementation a written use selects, only makes the use fail, so there is
//! no silent change to refuse here.
//!
//! The choice of which `with` frame answers a `uses` requirement, and what the
//! name of a `with` operand or of a bare pattern refers to, are decided by
//! use. Every body that makes such a decision records the change it finds as a
//! [`BodyDiag`] at the place where authored code makes it; this module reads
//! those and names the generated implementations responsible.

use common::ingot::IngotKind;
use parser::TextRange;
use rustc_hash::FxHashSet;

use super::{PathRes, Tier, find_associated_type, ingot_has_generated_items, resolve_path};
use crate::{
    analysis::{
        HirAnalysisDb,
        diagnostics::SpannedHirAnalysisDb,
        ty::{
            assoc_const::InherentConstUse,
            canonical::Canonicalized,
            const_expr::ConstExpr,
            const_ty::{ConstTyData, ConstTyId, const_ty_from_inherent_const_use},
            corelib::{presence_decision, presence_traits},
            diagnostics::{BodyDiag, FuncBodyDiag},
            trait_def::{ImplementorId, ImplementorOrigin, TraitInstId, impls_for_trait_def},
            trait_lower::lower_impl_trait_header,
            trait_resolution::{
                GoalSatisfiability, PredicateListId, TraitSolveCx,
                constraint::{
                    PredicateSource, candidate_decl_constraint_pairs, collect_constraints,
                },
            },
            ty_check::{
                BodyOwner, Callable, ConstRef, UseTarget, ValuePathRef, checked_body_diags,
                infer_body, item_target, module_body_owners, target_at,
            },
            ty_def::{AssocTy, BorrowKind, TyBase, TyData, TyId},
            ty_lower::lower_hir_ty,
            unify::UnificationTable,
            visitor::{TyVisitable, TyVisitor, walk_assoc_ty, walk_const_ty, walk_ty},
        },
    },
    core::{hir_def::HirIngot, semantic::reference::enclosing_assumptions},
    hir_def::{
        Body, CallableDef, Func, GenericParamOwner, ImplTrait, ItemKind, PathId, TopLevelMod,
        Trait, TraitRefId, TypeId as HirTyId, scope_graph::ScopeId,
    },
    span::{
        DynLazySpan, LazySpan,
        lazy_spans::{LazyPathSpan, LazyTraitRefSpan, LazyTySpan},
    },
    visitor::{
        Visitor, VisitorCtxt, walk_impl_trait_bounds, walk_impl_trait_header, walk_path,
        walk_trait_ref, walk_type,
    },
};

/// A decision that prefers one implementation to another, made by use.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, salsa::SalsaValue)]
pub enum Decision {
    /// Which `with` frame answers a `uses` requirement of a trait.
    EffectProvider,
    /// What the name of a `with` operand refers to: a name that written code
    /// binds, such as a local binding, keeps its meaning.
    WithOperand,
    /// Whether a bare name in a pattern binds a variable or compares with a
    /// constant: only a constant that written code binds to the name is
    /// compared.
    PatternName,
}

impl Decision {
    /// What is decided, as a noun phrase.
    pub fn what(self) -> &'static str {
        match self {
            Decision::EffectProvider => "which provider answers a `uses` requirement",
            Decision::WithOperand => "what the name of a `with` operand refers to",
            Decision::PatternName => {
                "whether a bare name in a pattern binds a variable or compares with a constant"
            }
        }
    }

    /// How it is decided, as a clause.
    pub fn rule(self) -> &'static str {
        match self {
            Decision::EffectProvider => {
                "the innermost `with` frame with a provider that applies answers it"
            }
            Decision::WithOperand => {
                "a name bound in written code keeps its meaning and is never replaced by a generated implementation of that name"
            }
            Decision::PatternName => {
                "a bare name compares with a constant only if written code binds one to it, and otherwise binds a new variable"
            }
        }
    }
}

impl Decision {
    /// How to resolve a refusal.
    pub fn remedy(self) -> &'static str {
        match self {
            Decision::WithOperand => "rename the generated implementation",
            Decision::PatternName => {
                "rename the generated constant, or match it by a path that names its module"
            }
            _ => "write the implementation in the package's own code, or leave it out",
        }
    }
}

/// A generated item that takes part in a changed choice: an implementation
/// or a constant.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GeneratedItem {
    /// What it is, for example "implementation of `Fn` for `Double`".
    pub description: String,
    pub target: UseTarget,
}

/// Where written code makes a decision.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DecisionSite {
    pub file: url::Url,
    pub range: TextRange,
    pub text: String,
}

/// A decision of written code that generated implementations would change.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChangedChoice {
    /// What is decided, as a noun phrase: "whether a value is copied or
    /// moved".
    pub what: String,
    /// Why the package's own code already answers it, as sentences.
    pub explanation: String,
    /// How to resolve the refusal, as a clause.
    pub remedy: String,
    /// The type the decision is about.
    pub subject: String,
    /// True if the refusal follows from definitions alone, whether or not
    /// written code uses the type: the generated implementation "could
    /// change" what a program does. False if a body makes the decision and
    /// the change was found there: it "would change" it.
    pub by_definition: bool,
    pub site: Option<DecisionSite>,
    /// The generated implementations that make the difference. Empty if none
    /// could be isolated.
    pub generated: Vec<GeneratedItem>,
    pub file: url::Url,
}

impl ChangedChoice {
    fn of(
        decision: Decision,
        subject: String,
        site: Option<DecisionSite>,
        generated: Vec<GeneratedItem>,
        file: url::Url,
    ) -> Self {
        Self {
            what: decision.what().to_string(),
            explanation: format!(
                "In the package's own code {}; what is written there already answers it.",
                decision.rule()
            ),
            remedy: decision.remedy().to_string(),
            subject,
            by_definition: false,
            site,
            generated,
            file,
        }
    }
}

/// The decisions in the bodies of `top_mod` that generated implementations
/// would change.
pub fn changed_choices<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
) -> Vec<ChangedChoice> {
    let ingot = top_mod.ingot(db);
    let Some(file) = top_mod.file(db).url(db) else {
        return Vec::new();
    };
    if !*ingot_has_generated_items(db, ingot) {
        return Vec::new();
    }
    let mut changed: Vec<ChangedChoice> = presence_changes(db, top_mod, &file);
    for owner in module_body_owners(db, top_mod) {
        // A decision that generated code makes is generated code's own: it may
        // use generated implementations.
        if owner.scope().is_within_generated(db, ingot) {
            continue;
        }
        for diag in checked_body_diags(db, owner) {
            let FuncBodyDiag::Body(BodyDiag::GenerationChangesChoice {
                primary,
                decision,
                trait_,
                subject,
                generated,
                ty,
            }) = diag
            else {
                continue;
            };
            let site = primary.resolve(db).and_then(|span| {
                let text = span.file.text(db);
                let (start, end) = (
                    usize::from(span.range.start()),
                    usize::from(span.range.end()),
                );
                Some(DecisionSite {
                    file: span.file.url(db)?,
                    range: span.range,
                    text: text.get(start..end)?.to_string(),
                })
            });
            let choice = ChangedChoice::of(
                decision,
                subject,
                site,
                match (generated, ty) {
                    (Some(scope), _) => generated_item(db, scope).into_iter().collect(),
                    (None, Some(ty)) => generated_support(db, top_mod.scope(), trait_, ty, 3)
                        .into_iter()
                        .filter_map(|imp| generated_item(db, imp.scope()))
                        .collect(),
                    (None, None) => Vec::new(),
                },
                file.clone(),
            );
            if !changed.contains(&choice) {
                changed.push(choice);
            }
        }
    }
    changed
}

/// The generated implementations in `top_mod` that change what the compiler
/// decides by whether an implementation exists, for any type written in the
/// package: an implementation of a trait that declares `#[decides = "..."]`
/// (see `presence_decision`), or of any trait that would make a written
/// implementation of one apply to more types. This is decided from the
/// definitions alone, not from where written code uses the type, so it does
/// not depend on what else the package says. A named implementation is
/// chosen by name and decides nothing implicitly.
fn presence_changes<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    top_mod: TopLevelMod<'db>,
    file: &url::Url,
) -> Vec<ChangedChoice> {
    let ingot = top_mod.ingot(db);
    let mut out: Vec<ChangedChoice> = Vec::new();
    for &imp in top_mod.all_impl_traits(db).iter() {
        if !imp.scope().is_within_generated(db, ingot) || imp.selection_name(db).is_some() {
            continue;
        }
        let (Some(implementor), Some(item)) = (
            lower_impl_trait_header(db, imp),
            generated_item(db, imp.scope()),
        ) else {
            continue;
        };
        let trait_ = implementor.trait_def(db);
        let trait_name = implementor.trait_(db).pretty_print(db, false).to_string();
        let choice = if let Some(what) = presence_decision(db, trait_) {
            ChangedChoice {
                what,
                explanation: format!(
                    "The compiler decides this by whether a type implements `{trait_name}`, so only an implementation that the package's own code or a dependency wrote counts."
                ),
                remedy: request_remedy(&trait_name),
                subject: implementor.self_ty(db).pretty_print(db).to_string(),
                by_definition: true,
                site: None,
                generated: vec![item],
                file: file.clone(),
            }
        } else if let Some(widening) = widened_presence(db, ingot, implementor) {
            ChangedChoice {
                what: widening.what.clone(),
                explanation: widening.explanation(db, ingot, implementor),
                remedy: request_remedy(&trait_name),
                subject: widening.deciding.self_ty(db).pretty_print(db).to_string(),
                by_definition: true,
                site: None,
                generated: vec![item],
                file: file.clone(),
            }
        } else {
            continue;
        };
        if !out.contains(&choice) {
            out.push(choice);
        }
    }
    out
}

/// How to resolve the refusal of a generated implementation: write it, ask a
/// provider for it with a derive header, or leave it out. A header counts as a
/// written implementation, and it can request every trait: the provider is
/// keyed by the bare trait, and its output is checked against the header's
/// arguments.
fn request_remedy(trait_name: &str) -> String {
    format!(
        "write the implementation in the package's own code, request it with a header `impl {trait_name} for Type with (Provider)`, or leave it out"
    )
}

/// The written implementation of a trait that declares `#[decides = "..."]`
/// (and what it decides) that the generated implementation `generated` could
/// make apply to more types, if there is one, with the written implementation
/// whose bound brings the trait of `generated` in.
///
/// A written implementation applies to the types that meet its bounds, and
/// whether a bound is met can depend on a generated implementation in ways
/// that a reading of the declarations does not bound: through a projection
/// such as `<T as HasA>::A: Gate`, through the arguments of a const
/// expression, through another implementation. So no attempt is made to say
/// when a bound can be met. The test is by name only: a generated
/// implementation is refused if its trait is a trait that a bound of a
/// written implementation of a deciding trait names (the trait of the bound,
/// a trait inside its arguments, or a trait that a const expression in them
/// uses, see `named_traits`), or that a bound of a
/// written implementation of such a trait names, without limit. Arguments are
/// never compared, at any level.
///
/// Every written implementation of a deciding trait is searched, whatever its
/// self type is and wherever its package declares the traits it is bounded on:
/// no reading of the declarations says that a generated implementation cannot
/// change what a program does (each narrower rule that was tried was beaten
/// by a case in which a loop, a copy or a call changed, see
/// `package_boundary`). A bound on a deciding trait is not followed in this
/// function, because the written implementations of that trait are searched
/// themselves, and a generated implementation of it is refused as such.
fn widened_presence<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    ingot: common::ingot::Ingot<'db>,
    generated: ImplementorId<'db>,
) -> Option<Widening<'db>> {
    let trait_ = generated.trait_def(db);
    // Most generated implementations are of a trait that no such bound leads
    // to; that is known without looking at a single implementation.
    if !presence_bound_traits(db, ingot).contains(&trait_) {
        return None;
    }
    for presence in presence_traits(db, ingot.root_mod(db).scope()) {
        let Some(what) = presence_decision(db, presence) else {
            continue;
        };
        for &written in impls_for_trait_def(db, ingot, presence, Tier::Authored) {
            if !matches!(written.origin(db), ImplementorOrigin::Hir(_)) {
                continue;
            }
            let named = named_traits(db, written);
            let via = if named.contains(&trait_) {
                Some(written)
            } else {
                sorted_by_name(db, named)
                    .into_iter()
                    .filter(|&named| presence_decision(db, named).is_none())
                    .find(|&named| presence_bound_closure(db, ingot, named).contains(&trait_))
                    .and_then(|named| linking_implementation(db, ingot, named, trait_))
            };
            if let Some(via) = via {
                return Some(Widening {
                    deciding: written,
                    what,
                    via,
                });
            }
        }
    }
    None
}

/// Why a generated implementation could add types to a written implementation
/// of a trait that declares `#[decides = "..."]`.
struct Widening<'db> {
    /// The written implementation of the deciding trait.
    deciding: ImplementorId<'db>,
    /// What the deciding trait decides.
    what: String,
    /// The written implementation with a bound that names the generated
    /// trait: the deciding one itself, or one of a trait that bounds lead to
    /// from it.
    via: ImplementorId<'db>,
}

impl<'db> Widening<'db> {
    /// The sentences that tell which written implementation brought the trait
    /// of `generated` into the set of refused traits, and through which bound.
    fn explanation(
        &self,
        db: &'db dyn SpannedHirAnalysisDb,
        here: common::ingot::Ingot<'db>,
        generated: ImplementorId<'db>,
    ) -> String {
        let named = generated.trait_def(db);
        let named_name = named
            .name(db)
            .to_opt()
            .map_or_else(String::new, |name| name.data(db).to_string());
        let trait_name = generated.trait_(db).pretty_print(db, false).to_string();
        let bound = bounds_naming(db, self.via, named).unwrap_or_else(|| "a bound".to_string());
        let tail =
            format!("so this implementation of `{trait_name}` could add types that meet them.");
        let deciding = describe(db, here, self.deciding);
        if self.via == self.deciding {
            format!(
                "The {deciding} applies to the types that meet its bounds, and {bound} names `{named_name}`, {tail}"
            )
        } else {
            let via = describe(db, here, self.via);
            let via_trait = self
                .via
                .trait_def(db)
                .name(db)
                .to_opt()
                .map_or_else(String::new, |name| name.data(db).to_string());
            format!(
                "The {via} has {bound}, which names `{named_name}`. `{via_trait}` is named by a bound of the {deciding}, directly or through the bounds of other written implementations. That implementation applies to the types that meet its bounds, {tail}"
            )
        }
    }
}

/// `written implementation of `Collection` for `Bag<T>` in package `lib``.
fn describe<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    here: common::ingot::Ingot<'db>,
    written: ImplementorId<'db>,
) -> String {
    let trait_ = written.trait_(db).pretty_print(db, false);
    let ty = written.self_ty(db).pretty_print(db);
    let place = match written.origin(db) {
        ImplementorOrigin::Hir(impl_trait) => {
            let ingot = impl_trait.top_mod(db).ingot(db);
            if ingot == here {
                "this package".to_string()
            } else {
                format!("package `{}`", package_name(db, ingot))
            }
        }
        _ => "a dependency".to_string(),
    };
    format!("written implementation of `{trait_}` for `{ty}` in {place}")
}

fn package_name<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    ingot: common::ingot::Ingot<'db>,
) -> String {
    ingot
        .config(db)
        .as_ref()
        .and_then(|config| config.metadata.name.as_ref())
        .map(|name| name.to_string())
        .unwrap_or_else(|| match ingot.kind(db) {
            IngotKind::Core => "core".to_string(),
            IngotKind::Std => "std".to_string(),
            _ => "a dependency".to_string(),
        })
}

/// The traits in `names`, ordered by name, so that a message does not depend on
/// the order of a set.
fn sorted_by_name<'db>(
    db: &'db dyn HirAnalysisDb,
    names: FxHashSet<Trait<'db>>,
) -> Vec<Trait<'db>> {
    let mut names: Vec<_> = names.into_iter().collect();
    names.sort_by_key(|trait_| {
        trait_
            .name(db)
            .to_opt()
            .map_or_else(String::new, |name| name.data(db).to_string())
    });
    names
}

/// A written implementation of `start`, or of a trait that bounds of written
/// implementations lead to from it, whose bounds name `target`: the nearest
/// one, so that the shortest chain is told. This is the same walk as
/// `presence_bound_closure`, which only says whether there is one.
fn linking_implementation<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: common::ingot::Ingot<'db>,
    start: Trait<'db>,
    target: Trait<'db>,
) -> Option<ImplementorId<'db>> {
    let mut seen: FxHashSet<Trait<'db>> = FxHashSet::default();
    let mut pending = std::collections::VecDeque::from([start]);
    while let Some(next) = pending.pop_front() {
        if !seen.insert(next) {
            continue;
        }
        for &written in impls_for_trait_def(db, ingot, next, Tier::Authored) {
            if !matches!(written.origin(db), ImplementorOrigin::Hir(_)) {
                continue;
            }
            let named = named_traits(db, written);
            if named.contains(&target) {
                return Some(written);
            }
            pending.extend(
                sorted_by_name(db, named)
                    .into_iter()
                    .filter(|&named| presence_decision(db, named).is_none()),
            );
        }
    }
    None
}

/// The first bound of `written` that names `target`, as it is written: the
/// parameter or the `where` predicate that holds it.
fn bounds_naming<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    written: ImplementorId<'db>,
    target: Trait<'db>,
) -> Option<String> {
    let ImplementorOrigin::Hir(impl_trait) = written.origin(db) else {
        return None;
    };
    // The same bounds, in the same order, as `written.constraints`.
    let pairs = candidate_decl_constraint_pairs(db, GenericParamOwner::ImplTrait(impl_trait));
    let Some((_, source)) = pairs.iter().find(|(bound, _)| {
        let mut names = NamedTraits::new(db);
        names.add_bounds(PredicateListId::new(db, vec![*bound]));
        names.found.contains(&target)
    }) else {
        // No bound names it: the type of the implementation or the arguments
        // of its trait do, as written.
        let mut names = NamedTraits::new(db);
        names.read_header_types_as_written(impl_trait);
        if !names.found.contains(&target) {
            return None;
        }
        let span = impl_trait.span().ty().resolve(db)?;
        let text = span.file.text(db);
        return text
            .get(usize::from(span.range.start())..usize::from(span.range.end()))
            .map(|text| {
                format!(
                    "its type `{}`",
                    text.split_whitespace().collect::<Vec<_>>().join(" ")
                )
            });
    };
    let span: DynLazySpan = match *source {
        PredicateSource::GenericParamBound { param_idx, .. } => {
            impl_trait.span().generic_params().param(param_idx).into()
        }
        PredicateSource::WherePredicateBound { pred_idx, .. } => {
            impl_trait.span().where_clause().predicate(pred_idx).into()
        }
    };
    let span = span.resolve(db)?;
    let text = span.file.text(db);
    text.get(usize::from(span.range.start())..usize::from(span.range.end()))
        .map(|text| {
            format!(
                "the bound `{}`",
                text.split_whitespace().collect::<Vec<_>>().join(" ")
            )
        })
}

/// Every trait that the bounds of `written` name: the trait of each bound,
/// and each trait inside the types of its arguments and of its associated
/// type bindings: the trait of a projection (`<T as HasA>::A`, `T::A`), of a
/// qualified type or of a trait descriptor. The names are read from the
/// compiler's own predicate list of the implementation, which holds its
/// inline bounds and its `where` clauses alike.
///
/// A const expression in a bound is read as code, not as a value: what it
/// names counts however it would evaluate, because a bound whose expression
/// fails to evaluate is not met, and an implementation of a trait it uses can
/// be what makes it evaluate (see [`NamedTraits`]).
fn named_traits<'db>(
    db: &'db dyn HirAnalysisDb,
    written: ImplementorId<'db>,
) -> FxHashSet<Trait<'db>> {
    let mut names = NamedTraits::new(db);
    names.add_bounds(written.constraints(db));
    // Which types the implementation covers is decided by its whole header: a
    // const expression in the type or in the arguments of the trait is a
    // condition as a bound is.
    for &arg in written.trait_inst(db).args(db) {
        arg.visit_with(&mut names);
    }
    written.self_ty(db).visit_with(&mut names);
    if let ImplementorOrigin::Hir(impl_trait) = written.origin(db) {
        names.read_bounds_as_written(impl_trait);
    }
    names.found
}

/// Collects the traits that a type names, other than as the trait of a bound.
///
/// A const expression in a type (`Chk<{ <Y as Tr>::N }>`) is read from its
/// source and its inference, and is never evaluated: a bound whose expression
/// cannot be evaluated is not met, and an implementation of a trait that the
/// expression uses can be what makes it evaluate. So every trait counts that
/// the expression uses, or that a function or constant it refers to uses:
/// - what inference resolved: the callable of each call, method call and
///   operator and the traits it is bounded on, the constants its paths name
///   (trait constants by their trait, constants of an inherent implementation
///   with that implementation's bounds), the calls of a `for` loop, and the
///   type of every expression and pattern, which reach nested expressions;
/// - what is written, which inference normalizes away once an implementation
///   exists: a qualified path or type (`<Y as HasA>::A`) and a projection on
///   a concrete type (`Y::A`).
///
/// Functions and constants that the expression names are read in the same
/// way, with the bounds of the functions. The bodies of trait methods and
/// trait constants are not read: a body that needs an implementation of
/// another trait is an error without it, and the trait that selects the body
/// is named here.
struct NamedTraits<'db> {
    db: &'db dyn HirAnalysisDb,
    found: FxHashSet<Trait<'db>>,
    /// The bodies read, each once.
    read: FxHashSet<BodyOwner<'db>>,
    /// The bodies read as written, each once.
    written: FxHashSet<Body<'db>>,
    /// The generic owners whose declared bounds were read, each once.
    bounded: FxHashSet<GenericParamOwner<'db>>,
}

impl<'db> NamedTraits<'db> {
    fn new(db: &'db dyn HirAnalysisDb) -> Self {
        Self {
            db,
            found: FxHashSet::default(),
            read: FxHashSet::default(),
            written: FxHashSet::default(),
            bounded: FxHashSet::default(),
        }
    }

    /// The trait of a use that inference resolved, unless no generated
    /// implementation can be of it: a trait of core or std at an instance of
    /// primitive types only, as the operators of integers are. Implementing
    /// a trait of another package for types of other packages is an error.
    fn name_used(&mut self, inst: TraitInstId<'db>) {
        let from_library = matches!(
            inst.def(self.db).ingot(self.db).kind(self.db),
            IngotKind::Core | IngotKind::Std
        );
        let primitive_only = inst
            .args(self.db)
            .iter()
            .all(|arg| matches!(arg.data(self.db), TyData::TyBase(TyBase::Prim(_))));
        if !(from_library && primitive_only) {
            self.found.insert(inst.def(self.db));
        }
    }

    /// The trait of each bound of `list`, and what each bound names.
    fn add_bounds(&mut self, list: PredicateListId<'db>) {
        for bound in list.list(self.db) {
            self.found.insert(bound.def(self.db));
            bound.visit_with(self);
        }
    }

    /// What the declared bounds of a function, an implementation or any other
    /// generic item name.
    fn add_declared_bounds(&mut self, owner: GenericParamOwner<'db>) {
        if self.bounded.insert(owner) {
            self.add_bounds(collect_constraints(self.db, owner).instantiate_identity());
            self.read_const_conditions(ItemKind::from(owner));
        }
    }

    /// The conditions of the `where { ... }` predicates of an item are const
    /// expressions in its bounds like any other.
    fn read_const_conditions(&mut self, item: ItemKind<'db>) {
        for owner in BodyOwner::const_predicates_of(self.db, item) {
            self.read_body(owner);
        }
    }

    /// A function that a const expression calls or names: its bounds, and,
    /// unless it is a method of a trait or of an implementation of one, its
    /// body.
    fn read_func(&mut self, func: Func<'db>, mode: Option<BorrowKind>) {
        self.add_declared_bounds(GenericParamOwner::Func(func));
        let selected_by_trait = matches!(
            func.scope().parent_item(self.db),
            Some(ItemKind::Trait(_) | ItemKind::ImplTrait(_))
        );
        if !selected_by_trait {
            let owner = if mode.is_some() {
                BodyOwner::func_at(self.db, func, mode)
            } else {
                BodyOwner::func(self.db, func)
            };
            self.read_body(owner);
        }
    }

    /// The owner of a body that a const refers to: a function, whose bounds
    /// count too, or a constant.
    fn read_owner(&mut self, owner: BodyOwner<'db>) {
        match owner {
            BodyOwner::Func(func, mode) => self.read_func(func, mode),
            _ => self.read_body(owner),
        }
    }

    fn read_callable(&mut self, callable: &Callable<'db>) {
        callable.visit_with(self);
        let trait_inst = callable.trait_inst();
        if let Some(inst) = trait_inst {
            self.name_used(inst);
        }
        if let CallableDef::Func(func) = callable.callable_def() {
            if trait_inst.is_some() {
                self.add_declared_bounds(GenericParamOwner::Func(func));
            } else {
                self.read_func(func, callable.mode());
            }
        }
    }

    fn read_const_ref(&mut self, const_ref: ConstRef<'db>) {
        match const_ref {
            ConstRef::Const(const_) => self.read_body(BodyOwner::Const(const_)),
            ConstRef::TraitConst { use_, .. } => {
                self.name_used(use_.inst());
                use_.inst().visit_with(self);
            }
            ConstRef::InherentConst { use_, .. } => self.read_inherent_const(use_),
        }
    }

    /// A constant of an inherent implementation applies to the types that meet
    /// the implementation's bounds.
    fn read_inherent_const(&mut self, use_: InherentConstUse<'db>) {
        self.add_declared_bounds(GenericParamOwner::Impl(use_.impl_()));
        use_.receiver_ty().visit_with(self);
        if let Some(const_ty) = const_ty_from_inherent_const_use(self.db, use_) {
            self.visit_const_ty(&const_ty);
        }
    }

    /// The inference of a body: the type of every expression and pattern, and
    /// each callable, constant and loop step that its paths, methods and
    /// operators resolved to.
    fn read_body(&mut self, owner: BodyOwner<'db>) {
        if !self.read.insert(owner) {
            return;
        }
        let Some(body) = owner.body(self.db) else {
            return;
        };
        let typed = &infer_body(self.db, owner).1;
        for expr in body.exprs(self.db).keys() {
            typed.expr_ty(self.db, expr).visit_with(self);
            if let Some(callable) = typed.callable_expr(expr) {
                self.read_callable(callable);
            }
            if let Some(const_ref) = typed.expr_const_ref(expr) {
                self.read_const_ref(const_ref);
            }
            if let Some(ValuePathRef::TypeConst(ty)) = typed.value_path_ref(expr) {
                ty.visit_with(self);
            }
        }
        for pat in body.pats(self.db).keys() {
            typed.pat_ty(self.db, pat).visit_with(self);
            if let Some(const_ref) = typed.pat_const_ref(pat) {
                self.read_const_ref(const_ref);
            }
        }
        for stmt in body.stmts(self.db).keys() {
            if let Some(plan) = typed.for_loop_plan(stmt) {
                for call in &plan.calls {
                    self.read_callable(&call.callable);
                }
            }
        }
        self.read_written(body);
    }

    /// The types and traits as written in the bounds of an implementation.
    /// Lowering normalizes a projection on a concrete type (`Y::A: Okay`) to
    /// the type that the selected implementation gives it, which leaves no
    /// trace of the trait.
    fn read_bounds_as_written(&mut self, impl_trait: ImplTrait<'db>) {
        let mut ctxt = VisitorCtxt::with_impl_trait(self.db, impl_trait);
        walk_impl_trait_bounds(&mut WrittenNames { names: self }, &mut ctxt, impl_trait);
        self.read_header_types_as_written(impl_trait);
        self.read_const_conditions(ItemKind::ImplTrait(impl_trait));
    }

    /// The type that an implementation is for and the arguments of its trait,
    /// as written. The trait itself is not named by its own header.
    fn read_header_types_as_written(&mut self, impl_trait: ImplTrait<'db>) {
        let mut ctxt = VisitorCtxt::with_impl_trait(self.db, impl_trait);
        walk_impl_trait_header(&mut WrittenNames { names: self }, &mut ctxt, impl_trait);
    }

    /// The types and traits as written in a body. Inference normalizes a
    /// projection such as `<Y as HasA>::A` to the type it selects once an
    /// implementation exists, which leaves no trace of `HasA`.
    fn read_written(&mut self, body: Body<'db>) {
        if !self.written.insert(body) {
            return;
        }
        let mut ctxt = VisitorCtxt::with_body(self.db, body);
        WrittenNames { names: self }.visit_body(&mut ctxt, body);
    }
}

/// The types and trait paths written in a body, read as the lowering of a
/// signature reads them, without normalization.
struct WrittenNames<'a, 'db> {
    names: &'a mut NamedTraits<'db>,
}

impl<'a, 'db> Visitor<'db> for WrittenNames<'a, 'db> {
    fn visit_ty(&mut self, ctxt: &mut VisitorCtxt<'db, LazyTySpan<'db>>, ty: HirTyId<'db>) {
        let db = self.names.db;
        let scope = ctxt.scope();
        let lowered = lower_hir_ty(db, ty, scope, enclosing_assumptions(db, scope));
        lowered.visit_with(self.names);
        walk_type(self, ctxt, ty);
    }

    fn visit_path(&mut self, ctxt: &mut VisitorCtxt<'db, LazyPathSpan<'db>>, path: PathId<'db>) {
        // `Y::A` for a concrete `Y` lowers to the type that the selected
        // implementation gives `A`, with no mention of the trait.
        let db = self.names.db;
        let scope = ctxt.scope();
        if let Some(parent) = path.parent(db)
            && let Some(name) = path.ident(db).to_opt()
        {
            let assumptions = enclosing_assumptions(db, scope);
            if let Ok(PathRes::Ty(ty) | PathRes::TyAlias(_, ty)) =
                resolve_path(db, parent, scope, assumptions, false)
                && let Ok(found) =
                    find_associated_type(db, scope, Canonicalized::new(db, ty), name, assumptions)
            {
                self.names
                    .found
                    .extend(found.into_iter().map(|(inst, _)| inst.def(db)));
            }
        }
        walk_path(self, ctxt, path);
    }

    fn visit_trait_ref(
        &mut self,
        ctxt: &mut VisitorCtxt<'db, LazyTraitRefSpan<'db>>,
        trait_ref: TraitRefId<'db>,
    ) {
        let db = self.names.db;
        let scope = ctxt.scope();
        if let Some(path) = trait_ref.path(db).to_opt()
            && let Ok(PathRes::Trait(inst)) =
                resolve_path(db, path, scope, enclosing_assumptions(db, scope), false)
        {
            self.names.found.insert(inst.def(db));
        }
        walk_trait_ref(self, ctxt, trait_ref);
    }
}

impl<'db> TyVisitor<'db> for NamedTraits<'db> {
    fn db(&self) -> &'db dyn HirAnalysisDb {
        self.db
    }

    fn visit_ty(&mut self, ty: TyId<'db>) {
        match ty.data(self.db) {
            TyData::QualifiedTy(inst) | TyData::ConstraintTerm(inst) => {
                self.found.insert(inst.def(self.db));
            }
            TyData::TraitFamily(family) => {
                self.found.insert(family.def(self.db));
            }
            _ => {}
        }
        walk_ty(self, ty);
    }

    fn visit_assoc_ty(&mut self, assoc_ty: &AssocTy<'db>) {
        self.found.insert(assoc_ty.trait_.def(self.db));
        walk_assoc_ty(self, assoc_ty);
    }

    fn visit_func(&mut self, func: CallableDef<'db>) {
        if let CallableDef::Func(func) = func {
            self.read_func(func, None);
        }
    }

    fn visit_const_ty(&mut self, const_ty: &ConstTyId<'db>) {
        match const_ty.data(self.db) {
            ConstTyData::UnEvaluated {
                body,
                ty,
                template_ty,
                const_def,
                ..
            } => match (const_def, template_ty.or(*ty)) {
                (Some(const_def), _) => self.read_body(BodyOwner::Const(*const_def)),
                (None, Some(expected)) => self.read_body(BodyOwner::AnonConstBody {
                    body: *body,
                    expected,
                }),
                // The type of the expression is not known here, so inference
                // cannot be asked; the body is read as written.
                (None, None) => self.read_written(*body),
            },
            ConstTyData::Computation { source, .. } => self.read_owner(source.owner(self.db)),
            ConstTyData::Abstract(expr, _) => match expr.data(self.db) {
                ConstExpr::TraitConst(use_) => self.name_used(use_.inst()),
                ConstExpr::InherentConst(use_) => self.read_inherent_const(*use_),
                ConstExpr::Invocation(invocation) => {
                    self.read_owner(invocation.key.owner(self.db));
                }
                _ => {}
            },
            _ => {}
        }
        walk_const_ty(self, const_ty);
    }
}

/// The traits that a bound of a written implementation of a trait that
/// declares `#[decides = "..."]` names, and the traits that bounds of written
/// implementations lead to from those, without limit.
#[salsa::tracked(returns(ref))]
fn presence_bound_traits<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: common::ingot::Ingot<'db>,
) -> FxHashSet<Trait<'db>> {
    let mut found: FxHashSet<Trait<'db>> = FxHashSet::default();
    for presence in presence_traits(db, ingot.root_mod(db).scope()) {
        for bound in written_bounds(db, ingot, presence) {
            if found.insert(bound) {
                found.extend(presence_bound_closure(db, ingot, bound).iter().copied());
            }
        }
    }
    found
}

/// The traits that bounds of the written implementations of `trait_` name,
/// and bounds of the written implementations of those, and so on.
#[salsa::tracked(returns(ref))]
fn presence_bound_closure<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: common::ingot::Ingot<'db>,
    trait_: Trait<'db>,
) -> FxHashSet<Trait<'db>> {
    let mut found: FxHashSet<Trait<'db>> = FxHashSet::default();
    let mut pending = vec![trait_];
    while let Some(next) = pending.pop() {
        for bound in written_bounds(db, ingot, next) {
            if found.insert(bound) {
                pending.push(bound);
            }
        }
    }
    found
}

/// The traits named in the bounds of the written implementations of
/// `trait_`, except those that declare `#[decides = "..."]` themselves.
fn written_bounds<'db>(
    db: &'db dyn HirAnalysisDb,
    ingot: common::ingot::Ingot<'db>,
    trait_: Trait<'db>,
) -> Vec<Trait<'db>> {
    impls_for_trait_def(db, ingot, trait_, Tier::Authored)
        .iter()
        .filter(|written| matches!(written.origin(db), ImplementorOrigin::Hir(_)))
        .flat_map(|&written| named_traits(db, written))
        .filter(|&bound| presence_decision(db, bound).is_none())
        .collect()
}

fn generated_item<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    scope: ScopeId<'db>,
) -> Option<GeneratedItem> {
    match scope {
        ScopeId::Item(ItemKind::ImplTrait(imp)) => {
            let implementor = lower_impl_trait_header(db, imp)?;
            let trait_name = implementor.trait_def(db).name(db).to_opt()?;
            let target = target_at(
                db,
                scope,
                imp.span().resolve(db),
                trait_name.data(db).to_string(),
            )?;
            Some(GeneratedItem {
                description: format!(
                    "implementation of `{}` for `{}`",
                    implementor.trait_(db).pretty_print(db, false),
                    implementor.self_ty(db).pretty_print(db)
                ),
                target,
            })
        }
        ScopeId::Item(ItemKind::Const(constant)) => {
            let name = constant.name(db).to_opt()?.data(db).to_string();
            Some(GeneratedItem {
                description: format!("constant `{name}`"),
                target: item_target(db, scope, name)?,
            })
        }
        _ => None,
    }
}

/// The implementations generated in the ingot of `scope` that make `ty`
/// implement `trait_` (any trait if `None`) in the full tier but not in the
/// authored tier. An implementation counts when it is generated and applies to
/// `ty` itself, or when a written implementation applies to `ty` only because
/// one of its bounds is met by generated code, up to `depth` bounds deep.
pub(crate) fn generated_support<'db>(
    db: &'db dyn SpannedHirAnalysisDb,
    scope: ScopeId<'db>,
    trait_: Option<Trait<'db>>,
    ty: TyId<'db>,
    depth: usize,
) -> Vec<ImplTrait<'db>> {
    let ingot = scope.ingot(db);
    let mut found: Vec<ImplTrait<'db>> = Vec::new();
    let applies = |implementor: ImplementorId<'db>| {
        let mut table = UnificationTable::new(db);
        let instance = table.instantiate_with_fresh_vars(implementor);
        table.unify(instance.self_ty(db), ty).is_ok()
    };
    for &imp in ingot.all_impl_traits(db).iter() {
        if !imp.scope().is_within_generated(db, ingot) || imp.selection_name(db).is_some() {
            continue;
        }
        let Some(implementor) = lower_impl_trait_header(db, imp) else {
            continue;
        };
        if trait_.is_some_and(|trait_| implementor.trait_def(db) != trait_) {
            continue;
        }
        if applies(implementor) && !found.contains(&imp) {
            found.push(imp);
        }
    }
    let Some(trait_) = trait_.filter(|_| found.is_empty() && depth > 0) else {
        return found;
    };
    // A written implementation that applies only because a bound is met by
    // generated code.
    let solve =
        |tier| TraitSolveCx::new(db, scope, tier).with_assumptions(PredicateListId::empty_list(db));
    for &implementor in impls_for_trait_def(db, ingot, trait_, Tier::All) {
        let ImplementorOrigin::Hir(imp) = implementor.origin(db) else {
            continue;
        };
        if imp.scope().is_within_generated(db, ingot) || !applies(implementor) {
            continue;
        }
        let mut table = UnificationTable::new(db);
        let instance = table.instantiate_with_fresh_vars(implementor);
        if table.unify(instance.self_ty(db), ty).is_err() {
            continue;
        }
        for constraint in instance.constraints(db).list(db) {
            use crate::analysis::ty::fold::TyFoldable as _;
            let constraint = constraint.fold_with(db, &mut table);
            if constraint.args(db).iter().any(|ty| ty.has_var(db)) {
                continue;
            }
            let met_in = |tier| {
                matches!(
                    crate::analysis::ty::trait_resolution::is_goal_satisfiable(
                        db,
                        solve(tier),
                        constraint,
                    ),
                    GoalSatisfiability::Satisfied(_)
                )
            };
            if met_in(Tier::All) && !met_in(Tier::Authored) {
                for imp in generated_support(
                    db,
                    scope,
                    Some(constraint.def(db)),
                    constraint.self_ty(db),
                    depth - 1,
                ) {
                    if !found.contains(&imp) {
                        found.push(imp);
                    }
                }
            }
        }
    }
    found
}
