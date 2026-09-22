use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, Zero};

use crate::core::hir_def::{
    BinOp, Body, Const, Contract, Expr, ExprId, Func, GenericArgListId, GenericParamOwner, IdentId,
    LitKind, Partial, PatId, PathId, Stmt, TypeAlias as HirTypeAlias, TypeId as HirTypeId, UnOp,
};
use salsa::Update;

use super::const_expr::{ConstExpr, ConstExprId, ConstInvocation, pretty_print_un_op};
use super::{
    adt_def::AdtDef,
    assoc_const::{AssocConstUse, InherentConstUse},
    binder::Binder,
    diagnostics::{BodyDiag, FuncBodyDiag},
    fold::{AssocTySubst, TyFoldable, TyFolder},
    normalize::normalize_ty,
    trait_def::{
        ImplementorId, ResolvedImplInstance, TraitInstId, resolve_trait_impl_instance,
        selected_assoc_const_body_template,
    },
    trait_resolution::{Selection, TraitSolveCx, constraint::collect_constraints},
    ty_check::{BodyOwner, check_anon_const_body, check_const_body},
    ty_def::{InvalidCause, TyId, TyParam, TyVar},
    ty_lower::{ConstDefaultCompletion, collect_generic_params},
    unify::UnificationTable,
    visitor::{TyVisitable, TyVisitor},
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    semantic::{
        ConstDesc, ConstRepr, CtfeConfig, CtfeError, EffectProviderSubst, EvalFailure, EvalOutcome,
        GenericSubst, ImplEnv, PrimitiveFault, SConst, SemConstId, SemConstScalar, SemConstValue,
        SemOrigin, SemanticInstanceKey, VariantIndex, VerifiedConstValueId,
        const_computation_for_instance, describe_const_computation, enum_const,
        eval_body_owner_const, execute_source_int_binary, execute_source_int_unary,
        force_const_description, force_const_term_value, int_const, int_ty_shape,
        normalize_int_to_shape, sem_const_from_ty,
    },
    ty::trait_resolution::PredicateListId,
    ty::ty_def::{Kind, TyBase, TyData, TyVarSort},
};
use crate::hir_def::{CallableDef, ItemKind, attr::ArithmeticMode, scope_graph::ScopeId};
use common::indexmap::IndexMap;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutHoleArgSite<'db> {
    Path(PathId<'db>),
    GenericArgList(GenericArgListId<'db>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutIntroSite<'db> {
    pub root: LayoutIntroRoot<'db>,
    pub path: Vec<LayoutIntroStep>,
}

impl<'db> LayoutIntroSite<'db> {
    pub(crate) fn definition(owner: GenericParamOwner<'db>, param_idx: usize) -> Self {
        Self {
            root: LayoutIntroRoot::Definition { owner },
            path: vec![LayoutIntroStep::ConstParam(param_idx as u32)],
        }
    }

    pub(crate) fn lowering(site: LayoutHoleArgSite<'db>, arg_idx: usize) -> Self {
        Self {
            root: LayoutIntroRoot::Lowering { site },
            path: vec![LayoutIntroStep::ExplicitArg(arg_idx as u32)],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub enum LayoutIntroRoot<'db> {
    Definition { owner: GenericParamOwner<'db> },
    Lowering { site: LayoutHoleArgSite<'db> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutIntroStep {
    ExplicitArg(u32),
    ConstParam(u32),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Update)]
pub struct LayoutHoleTrace<'db> {
    pub introduced_at: LayoutIntroSite<'db>,
    pub landings: Vec<LayoutInstantiationId<'db>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstCanonMode {
    Stored,
    Identity,
    Display,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypePrintMode {
    Symbolic,
    Concrete,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ConstCanonEnv<'db> {
    pub scope: ScopeId<'db>,
    pub assumptions: PredicateListId<'db>,
    pub assoc_ty_subst: Option<TraitInstId<'db>>,
}

impl<'db> ConstCanonEnv<'db> {
    pub fn new(
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
        assoc_ty_subst: Option<TraitInstId<'db>>,
    ) -> Self {
        Self {
            scope,
            assumptions,
            assoc_ty_subst,
        }
    }

    fn without_assoc_ty_subst(self) -> Self {
        Self {
            assoc_ty_subst: None,
            ..self
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Update)]
pub enum CallableInputLayoutHoleOrigin {
    Receiver,
    ValueParam(usize),
    Effect(usize),
}

/// The declaration that owns a callable layout boundary.
///
/// Unlike [`Body`], this identity is available without inspecting or lowering
/// an implementation body, so declaration ABI queries remain body-independent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum CallableLayoutOwner<'db> {
    Func(Func<'db>),
    ContractInit {
        contract: Contract<'db>,
    },
    ContractRecvArm {
        contract: Contract<'db>,
        recv_idx: u32,
        arm_idx: u32,
    },
}

impl<'db> CallableLayoutOwner<'db> {
    pub fn func(self) -> Option<Func<'db>> {
        match self {
            Self::Func(func) => Some(func),
            Self::ContractInit { .. } | Self::ContractRecvArm { .. } => None,
        }
    }

    pub fn scope(self) -> ScopeId<'db> {
        match self {
            Self::Func(func) => func.scope(),
            Self::ContractInit { contract } | Self::ContractRecvArm { contract, .. } => {
                contract.scope()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HoleId<'db> {
    Structural(StructuralHoleId<'db>),
    Bound(BoundHoleId<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoundHoleId<'db> {
    Opaque,
    LayoutShape {
        ordinal: u32,
        kind: LayoutShapeHoleKind,
    },
    CallableInput {
        owner: CallableLayoutOwner<'db>,
        origin: CallableInputLayoutHoleOrigin,
        ordinal: usize,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LayoutShapeHoleKind {
    ExplicitWildcard,
    DefaultHoleParam,
    EffectKeyExistential,
}

impl StructuralHoleOrigin<'_> {
    pub(crate) fn shape_kind(self) -> LayoutShapeHoleKind {
        match self {
            Self::ExplicitWildcard { .. } => LayoutShapeHoleKind::ExplicitWildcard,
            Self::DefaultHoleParam { .. } => LayoutShapeHoleKind::DefaultHoleParam,
            Self::EffectKeyExistential { .. } => LayoutShapeHoleKind::EffectKeyExistential,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum StructuralHoleOrigin<'db> {
    ExplicitWildcard {
        site: LayoutHoleArgSite<'db>,
        arg_idx: usize,
    },
    DefaultHoleParam {
        owner: GenericParamOwner<'db>,
        param_idx: usize,
    },
    EffectKeyExistential {
        path: PathId<'db>,
        arg_idx: usize,
        owner: GenericParamOwner<'db>,
        param_idx: usize,
    },
}

/// A unique syntax position within a body that can introduce layout holes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum BodyHoleSite {
    Expr(ExprId),
    Pat(PatId),
}

/// Where a structural hole's identity is anchored.
///
/// During the shared, content-keyed lowering a hole is anchored at the memo
/// key of the execution that minted it (`Template*`), including its active
/// assumptions; since distinct memo entries have distinct keys, ordinals from
/// different executions can never collide. Anchored entry points re-anchor
/// template holes at a genuinely unique item position (added in later phases).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum HoleAnchor<'db> {
    /// Minted while lowering a HIR type (the `lower_hir_ty` memo key).
    TemplateTy {
        ty: HirTypeId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    },
    /// Minted while resolving a path outside any enclosing HIR-type lowering
    /// (e.g. path expressions in bodies).
    TemplatePath {
        path: PathId<'db>,
        scope: ScopeId<'db>,
        assumptions: PredicateListId<'db>,
    },
    /// Minted at a unique expression or pattern occurrence in a body. Unlike
    /// content-interned HIR types, paths, and argument lists, this is already a
    /// stable semantic source position and must not be re-anchored.
    BodySyntax { body: Body<'db>, site: BodyHoleSite },
    /// A hole owned by a type alias's right-hand side. Instantiating the
    /// alias at a use site replaces these with fresh holes minted from the
    /// use site's minter.
    AliasTemplate(HirTypeAlias<'db>),
    /// A unique associated-type definition in a trait impl. Besides giving
    /// holes a position-stable owner, this tells path lowering that
    /// `Self::Assoc` denotes an impl-local binding rather than a signature
    /// projection.
    ImplAssocType {
        impl_trait: crate::hir_def::ImplTrait<'db>,
        index: u32,
    },
    /// A unique callable input position used as the parent of structural
    /// projection landings discovered for that input.
    CallableInput {
        owner: CallableLayoutOwner<'db>,
        origin: CallableInputLayoutHoleOrigin,
    },
    /// The declared result position of one callable. Output evidence is a
    /// signature property and must never be keyed by a lowered body.
    CallableOutput { owner: CallableLayoutOwner<'db> },
    /// A canonical parent for nested evidence landings in one semantic value.
    /// This identity is local to schema derivation and is never an allocation
    /// identity in a contract root graph.
    SemanticValue { body: Body<'db>, local: u32 },
}

impl<'db> HoleAnchor<'db> {
    /// Whether this anchor is a content-keyed lowering template (as opposed
    /// to an alias template or, in later phases, a unique item position).
    pub(crate) fn is_lowering_template(self) -> bool {
        matches!(self, Self::TemplateTy { .. } | Self::TemplatePath { .. })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutBoundaryIdentity<'db> {
    ContractField {
        contract: Contract<'db>,
        field_index: u32,
    },
    AdtApplication(AdtDef<'db>),
    AliasUse(HirTypeAlias<'db>),
    ProviderTarget(ImplementorId<'db>),
    ArrayElement,
    CallableInput {
        owner: CallableLayoutOwner<'db>,
        origin: CallableInputLayoutHoleOrigin,
    },
    CallableOutput(CallableLayoutOwner<'db>),
    SemanticValue {
        body: Body<'db>,
        local: u32,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutOccurrenceStep {
    Instantiation(u32),
    TypeParam(u32),
    ConstParam(u32),
    GenericArg(u32),
    StructField(u32),
    EnumVariant(u32),
    EnumPayloadField(u32),
    TupleElem(u32),
    ArrayDimension(u32),
    Normalization,
    TemplateBody,
}

pub type LayoutOccurrencePath = Vec<LayoutOccurrenceStep>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutInstantiationContext<'db> {
    Lowering(HoleAnchor<'db>),
    Nested(LayoutInstantiationId<'db>),
}

#[salsa::interned]
#[derive(Debug)]
pub struct LayoutInstantiationId<'db> {
    pub context: LayoutInstantiationContext<'db>,
    pub boundary: LayoutBoundaryIdentity<'db>,
    pub occurrence: LayoutOccurrencePath,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutRootIdentity<'db> {
    Source {
        anchor: HoleAnchor<'db>,
        ordinal: u32,
    },
    Landing {
        source: LayoutRootId<'db>,
        instance: LayoutInstantiationId<'db>,
    },
}

#[salsa::interned]
#[derive(Debug)]
pub struct LayoutRootId<'db> {
    pub identity: LayoutRootIdentity<'db>,
}

impl<'db> LayoutRootId<'db> {
    pub fn source(db: &'db dyn HirAnalysisDb, anchor: HoleAnchor<'db>, ordinal: u32) -> Self {
        Self::new(db, LayoutRootIdentity::Source { anchor, ordinal })
    }

    pub fn landing(
        db: &'db dyn HirAnalysisDb,
        source: Self,
        instance: LayoutInstantiationId<'db>,
    ) -> Self {
        Self::new(db, LayoutRootIdentity::Landing { source, instance })
    }

    pub(crate) fn source_anchor(self, db: &'db dyn HirAnalysisDb) -> HoleAnchor<'db> {
        match self.identity(db) {
            LayoutRootIdentity::Source { anchor, .. } => anchor,
            LayoutRootIdentity::Landing { source, .. } => source.source_anchor(db),
        }
    }
}

/// Mints source layout-root identities for one lowering execution.
///
/// Threaded by reference through the lowering descent so that every mint
/// event within one execution receives a distinct ordinal; a hole cannot be
/// minted without one, which makes "forgot to re-key after a memoized call"
/// impossible by construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ConstBodyLowering {
    Eager,
    Deferred,
}

#[derive(Debug)]
pub(crate) struct HoleMinter<'db> {
    anchor: HoleAnchor<'db>,
    const_bodies: ConstBodyLowering,
    counter: std::cell::Cell<u32>,
    instantiation_counter: std::cell::Cell<u32>,
}

impl<'db> HoleMinter<'db> {
    pub(crate) fn new(anchor: HoleAnchor<'db>) -> Self {
        Self {
            anchor,
            const_bodies: ConstBodyLowering::Eager,
            counter: std::cell::Cell::new(0),
            instantiation_counter: std::cell::Cell::new(0),
        }
    }

    /// Creates lowering state for an item signature. Const bodies stay as
    /// typed-later metadata so candidate discovery never depends on body type
    /// checking or trait selection.
    pub(crate) fn deferred(anchor: HoleAnchor<'db>) -> Self {
        Self {
            anchor,
            const_bodies: ConstBodyLowering::Deferred,
            counter: std::cell::Cell::new(0),
            instantiation_counter: std::cell::Cell::new(0),
        }
    }

    pub(crate) fn const_bodies(&self) -> ConstBodyLowering {
        self.const_bodies
    }

    pub(crate) fn anchor(&self) -> HoleAnchor<'db> {
        self.anchor
    }

    pub(crate) fn mint(&self, db: &'db dyn HirAnalysisDb) -> LayoutRootId<'db> {
        let ordinal = self.counter.get();
        self.counter.set(ordinal + 1);
        LayoutRootId::source(db, self.anchor, ordinal)
    }

    pub(crate) fn next_instantiation_ordinal(&self) -> u32 {
        let ordinal = self.instantiation_counter.get();
        self.instantiation_counter.set(ordinal + 1);
        ordinal
    }
}

/// Identity of a structural layout hole.
///
/// HIR ids (`TypeId`, `PathId`, `GenericArgListId`) are content-interned, so
/// a hole's creation site alone cannot identify a syntactic occurrence:
/// `(Slot<_>, Slot<_>)` shares one child HIR type and `(M, M)` one alias
/// path. Source identity is therefore assigned at mint time: the `anchor`
/// names the lowering execution (or alias template) that minted the hole and
/// the `ordinal` distinguishes mint events within it. Structural landing
/// identity is derived separately from the source root and an explicit
/// instantiation instance. Diagnostic trace changes never change either
/// identity. Two holes silently merging means aliased storage slots, so
/// minting errs on the side of distinctness.
#[salsa::interned]
#[derive(Debug)]
pub struct StructuralHoleId<'db> {
    pub expected_ty: TyId<'db>,
    pub root: LayoutRootId<'db>,
    pub origin: StructuralHoleOrigin<'db>,
    pub trace: LayoutHoleTrace<'db>,
}

impl<'db> StructuralHoleId<'db> {
    pub(crate) fn with_intro(
        db: &'db dyn HirAnalysisDb,
        expected_ty: TyId<'db>,
        root: LayoutRootId<'db>,
        origin: StructuralHoleOrigin<'db>,
        introduced_at: LayoutIntroSite<'db>,
    ) -> Self {
        Self::new(
            db,
            expected_ty,
            root,
            origin,
            LayoutHoleTrace {
                introduced_at,
                landings: Vec::new(),
            },
        )
    }

    pub(crate) fn anchor(self, db: &'db dyn HirAnalysisDb) -> HoleAnchor<'db> {
        self.root(db).source_anchor(db)
    }

    pub(crate) fn introduced_at(self, db: &'db dyn HirAnalysisDb) -> LayoutIntroSite<'db> {
        self.trace(db).introduced_at
    }
}

impl<'db> HoleId<'db> {
    pub(crate) fn bound_callable(
        owner: CallableLayoutOwner<'db>,
        origin: CallableInputLayoutHoleOrigin,
        ordinal: usize,
    ) -> Self {
        Self::Bound(BoundHoleId::CallableInput {
            owner,
            origin,
            ordinal,
        })
    }

    pub(crate) fn bound_opaque() -> Self {
        Self::Bound(BoundHoleId::Opaque)
    }

    pub(crate) fn structural(
        db: &'db dyn HirAnalysisDb,
        expected_ty: TyId<'db>,
        origin: StructuralHoleOrigin<'db>,
        introduced_at: LayoutIntroSite<'db>,
        root: LayoutRootId<'db>,
    ) -> Self {
        Self::Structural(StructuralHoleId::with_intro(
            db,
            expected_ty,
            root,
            origin,
            introduced_at,
        ))
    }
}

fn pretty_print_const_arg<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> String {
    match ty.data(db) {
        TyData::ConstTy(const_ty) if matches!(const_ty.data(db), ConstTyData::TyParam(param, _) if param.is_normal()) =>
        {
            let ConstTyData::TyParam(param, _) = const_ty.data(db) else {
                unreachable!()
            };
            param.name.data(db).to_string()
        }
        _ => ty.pretty_print(db).to_string(),
    }
}

fn generic_const_param_display_map<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    generic_args: &[TyId<'db>],
) -> FxHashMap<IdentId<'db>, String> {
    let Some(owner) = body
        .scope()
        .parent_item(db)
        .and_then(GenericParamOwner::from_item_opt)
    else {
        return FxHashMap::default();
    };

    super::ty_lower::collect_generic_params(db, owner)
        .params(db)
        .iter()
        .copied()
        .enumerate()
        .filter_map(|(idx, param_ty)| {
            let TyData::ConstTy(const_ty) = param_ty.data(db) else {
                return None;
            };
            let ConstTyData::TyParam(param, _) = const_ty.data(db) else {
                return None;
            };
            generic_args
                .get(idx)
                .map(|arg| (param.name, pretty_print_const_arg(db, *arg)))
        })
        .collect()
}

fn pretty_print_const_body_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr_id: crate::hir_def::ExprId,
    generic_param_display: &FxHashMap<IdentId<'db>, String>,
) -> Option<String> {
    ConstBodyExprPrinter {
        db,
        body,
        generic_param_display,
    }
    .pretty_print(expr_id)
}

struct ConstBodyExprPrinter<'a, 'db> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    generic_param_display: &'a FxHashMap<IdentId<'db>, String>,
}

impl<'a, 'db> ConstBodyExprPrinter<'a, 'db> {
    fn pretty_print(&self, expr_id: crate::hir_def::ExprId) -> Option<String> {
        let Partial::Present(expr) = expr_id.data(self.db, self.body) else {
            return None;
        };

        match expr {
            Expr::Lit(lit) => Some(lit.pretty_print(self.db)),
            Expr::Path(path) if path.is_present() => Some(self.pretty_print_path(path.unwrap())),
            Expr::Call(callee, args) => {
                let callee = self.pretty_print(*callee)?;
                let args = args
                    .iter()
                    .map(|arg| self.pretty_print(arg.expr))
                    .collect::<Option<Vec<_>>>()?;
                Some(format!("{callee}({})", args.join(", ")))
            }
            Expr::Bin(lhs, rhs, op) if !matches!(op, BinOp::Index) => {
                let lhs = self.pretty_print(*lhs)?;
                let rhs = self.pretty_print(*rhs)?;
                Some(format!("({lhs} {} {rhs})", op.pretty_print()))
            }
            Expr::Un(expr, op) => Some(pretty_print_un_op(*op, self.pretty_print(*expr)?)),
            Expr::Cast(expr, to) => Some(format!(
                "({} as {})",
                self.pretty_print(*expr)?,
                to.to_opt()?.pretty_print(self.db)
            )),
            Expr::Block(stmts) if stmts.len() == 1 => match stmts[0].data(self.db, self.body) {
                Partial::Present(Stmt::Expr(tail_expr)) => self.pretty_print(*tail_expr),
                Partial::Present(_) | Partial::Absent => None,
            },
            _ => None,
        }
    }

    fn pretty_print_path(&self, path: PathId<'db>) -> String {
        if path.parent(self.db).is_none()
            && let Some(ident) = path.as_ident(self.db)
            && let Some(replacement) = self.generic_param_display.get(&ident)
        {
            replacement.clone()
        } else {
            path.pretty_print(self.db)
        }
    }
}

/// Optional scalar reduction. A reached fault remains in the original term
/// for a required force to diagnose with its source occurrence.
pub fn evaluate_type_level_int_const_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    expr: ConstExprId<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstTyId<'db>> {
    if !matches!(
        expr.data(db),
        ConstExpr::ArithBinOp { .. } | ConstExpr::UnOp { .. } | ConstExpr::Cast { .. }
    ) {
        return None;
    }
    let term = ConstTyId::new(db, ConstTyData::Abstract(expr, expected_ty));
    match force_const_term_value(db, term, CtfeConfig::default(), SemOrigin::Synthetic) {
        EvalOutcome::Ready(value) => Some(const_ty_from_sem_const(db, value.value())),
        EvalOutcome::Blocked(_) | EvalOutcome::Failed(_) => None,
    }
}

fn ty_is_fully_ground<'db>(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> bool {
    match ty.data(db) {
        TyData::TyVar(_)
        | TyData::TyParam(_)
        | TyData::AssocTy(_)
        | TyData::QualifiedTy(_)
        | TyData::Invalid(_) => false,
        TyData::TyApp(abs, arg) => ty_is_fully_ground(db, *abs) && ty_is_fully_ground(db, *arg),
        TyData::ConstTy(const_ty) => const_ty_is_fully_ground(db, *const_ty),
        TyData::TyBase(_) | TyData::Never => true,
    }
}

fn trait_inst_is_fully_ground<'db>(db: &'db dyn HirAnalysisDb, inst: TraitInstId<'db>) -> bool {
    ty_is_fully_ground(db, inst.self_ty(db))
        && inst
            .args(db)
            .iter()
            .copied()
            .all(|arg| ty_is_fully_ground(db, arg))
        && inst
            .assoc_type_bindings(db)
            .values()
            .copied()
            .all(|ty| ty_is_fully_ground(db, ty))
}

fn sem_const_is_fully_ground<'db>(db: &'db dyn HirAnalysisDb, value: SemConstId<'db>) -> bool {
    match value.value(db) {
        SemConstValue::Unit => true,
        SemConstValue::Scalar { ty, .. } => ty_is_fully_ground(db, ty),
        SemConstValue::Description(term) => const_ty_is_fully_ground(db, term),
        SemConstValue::Tuple { ty, elems } | SemConstValue::Array { ty, elems } => {
            ty_is_fully_ground(db, ty)
                && elems
                    .iter()
                    .copied()
                    .all(|elem| sem_const_is_fully_ground(db, elem))
        }
        SemConstValue::Struct { ty, fields } | SemConstValue::Enum { ty, fields, .. } => {
            ty_is_fully_ground(db, ty)
                && fields
                    .iter()
                    .copied()
                    .all(|field| sem_const_is_fully_ground(db, field))
        }
    }
}

fn const_expr_is_fully_ground<'db>(db: &'db dyn HirAnalysisDb, expr: ConstExprId<'db>) -> bool {
    struct GroundCheck<'db> {
        db: &'db dyn HirAnalysisDb,
        ground: bool,
    }

    impl<'db> TyVisitor<'db> for GroundCheck<'db> {
        fn db(&self) -> &'db dyn HirAnalysisDb {
            self.db
        }

        fn visit_ty(&mut self, ty: TyId<'db>) {
            self.ground &= ty_is_fully_ground(self.db, ty);
        }
    }

    match expr.data(db) {
        ConstExpr::Invocation(invocation) => {
            let mut check = GroundCheck { db, ground: true };
            invocation.key.visit_with(&mut check);
            invocation.args.visit_with(&mut check);
            check.ground
        }
        ConstExpr::ArithBinOp { lhs, rhs, .. }
        | ConstExpr::ArrayRepeat {
            value: lhs,
            len: rhs,
        }
        | ConstExpr::ArrayIndex {
            array: lhs,
            index: rhs,
        } => ty_is_fully_ground(db, *lhs) && ty_is_fully_ground(db, *rhs),
        ConstExpr::UnOp { expr, .. }
        | ConstExpr::Cast { expr, .. }
        | ConstExpr::Field { value: expr, .. } => ty_is_fully_ground(db, *expr),
        ConstExpr::TraitConst(assoc) => trait_inst_is_fully_ground(db, assoc.inst()),
        ConstExpr::InherentConst(use_) => ty_is_fully_ground(db, use_.receiver_ty()),
    }
}

struct CanonicalizeInvocation<'db> {
    env: ConstCanonEnv<'db>,
    mode: ConstCanonMode,
}

impl<'db> TyFolder<'db> for CanonicalizeInvocation<'db> {
    fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
        canonicalize_ty_for_mode(db, ty, self.env, self.mode)
    }
}

fn canonicalize_const_expr_for_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    expr: ConstExprId<'db>,
    env: ConstCanonEnv<'db>,
    mode: ConstCanonMode,
) -> ConstExprId<'db> {
    match expr.data(db) {
        ConstExpr::Invocation(invocation) => {
            let mut folder = CanonicalizeInvocation { env, mode };
            ConstExprId::new(
                db,
                ConstExpr::Invocation(ConstInvocation {
                    key: invocation.key.fold_with(db, &mut folder),
                    args: invocation.args.clone().fold_with(db, &mut folder),
                    parameter_owner: invocation.parameter_owner,
                }),
            )
        }
        ConstExpr::ArithBinOp {
            op,
            mode: arithmetic_mode,
            lhs,
            rhs,
        } => ConstExprId::new(
            db,
            ConstExpr::ArithBinOp {
                op: *op,
                mode: *arithmetic_mode,
                lhs: canonicalize_ty_for_mode(db, *lhs, env, mode),
                rhs: canonicalize_ty_for_mode(db, *rhs, env, mode),
            },
        ),
        ConstExpr::UnOp {
            op,
            mode: arithmetic_mode,
            expr,
        } => ConstExprId::new(
            db,
            ConstExpr::UnOp {
                op: *op,
                mode: *arithmetic_mode,
                expr: canonicalize_ty_for_mode(db, *expr, env, mode),
            },
        ),
        ConstExpr::Cast { expr, to } => ConstExprId::new(
            db,
            ConstExpr::Cast {
                expr: canonicalize_ty_for_mode(db, *expr, env, mode),
                to: canonicalize_ty_for_mode(db, *to, env, mode),
            },
        ),
        ConstExpr::ArrayRepeat { value, len } => ConstExprId::new(
            db,
            ConstExpr::ArrayRepeat {
                value: canonicalize_ty_for_mode(db, *value, env, mode),
                len: canonicalize_ty_for_mode(db, *len, env, mode),
            },
        ),
        ConstExpr::ArrayIndex { array, index } => ConstExprId::new(
            db,
            ConstExpr::ArrayIndex {
                array: canonicalize_ty_for_mode(db, *array, env, mode),
                index: canonicalize_ty_for_mode(db, *index, env, mode),
            },
        ),
        ConstExpr::Field { value, index } => ConstExprId::new(
            db,
            ConstExpr::Field {
                value: canonicalize_ty_for_mode(db, *value, env, mode),
                index: *index,
            },
        ),
        ConstExpr::TraitConst(assoc) => ConstExprId::new(
            db,
            ConstExpr::TraitConst(if let Some(inst) = env.assoc_ty_subst {
                assoc.fold_with(db, &mut AssocTySubst::new(inst))
            } else {
                *assoc
            }),
        ),
        ConstExpr::InherentConst(use_) => ConstExprId::new(
            db,
            ConstExpr::InherentConst(if let Some(inst) = env.assoc_ty_subst {
                use_.fold_with(db, &mut AssocTySubst::new(inst))
            } else {
                *use_
            }),
        ),
    }
}

pub fn evaluate_type_level_const_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    expr: ConstExprId<'db>,
    expected_ty: TyId<'db>,
    env: ConstCanonEnv<'db>,
) -> Option<ConstTyId<'db>> {
    let expr = canonicalize_const_expr_for_mode(db, expr, env, ConstCanonMode::Identity);
    if expr.is_opaque_extern(db) {
        return None;
    }
    let term = ConstTyId::new(db, ConstTyData::Abstract(expr, expected_ty));
    let (origin, owner) = match expr.data(db) {
        ConstExpr::Invocation(invocation) => {
            let owner = invocation.key.owner(db);
            (SemOrigin::Body(owner), Some(owner))
        }
        _ => (SemOrigin::Synthetic, None),
    };
    match force_const_term_value(db, term, CtfeConfig::default(), origin) {
        EvalOutcome::Ready(value) => Some(const_ty_from_sem_const(db, value.value())),
        EvalOutcome::Blocked(_) => None,
        EvalOutcome::Failed(failure) => Some(ConstTyId::invalid(
            db,
            owner.map_or(InvalidCause::Other, |owner| {
                invalid_cause_from_eval_failure(db, owner, failure)
            }),
        )),
    }
}

/// Evaluate a const type as far as its symbolic inputs permit, including
/// deferred expressions that retain a declaration's resolution context.
pub(crate) fn evaluate_type_level_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    expected_ty: Option<TyId<'db>>,
) -> ConstTyId<'db> {
    // An implicit view describes access at the use site, not the const's value.
    let ty = const_ty.ty(db);
    let const_ty = const_ty.with_ty(db, ty.as_view(db).unwrap_or(ty));
    let expected_ty = expected_ty.map(|ty| ty.as_view(db).unwrap_or(ty));
    let evaluated = const_ty.evaluate(db, expected_ty);
    let ConstTyData::Abstract(expr, ty) = evaluated.data(db) else {
        return evaluated;
    };
    let concrete = if let Some(env) = const_canon_env(db, evaluated) {
        evaluate_type_level_const_expr(db, *expr, *ty, env)
    } else {
        evaluate_type_level_int_const_expr(db, *expr, *ty)
    };
    concrete.unwrap_or(evaluated)
}

fn const_ty_is_fully_ground<'db>(db: &'db dyn HirAnalysisDb, const_ty: ConstTyId<'db>) -> bool {
    match const_ty.data(db) {
        ConstTyData::TyVar(..) | ConstTyData::TyParam(..) | ConstTyData::Hole(..) => false,
        ConstTyData::Value(value) => sem_const_is_fully_ground(db, value.value()),
        ConstTyData::Description(value) => sem_const_is_fully_ground(db, *value),
        ConstTyData::Invalid(..) => false,
        ConstTyData::Computation { description, .. } => match description.repr() {
            ConstRepr::Value(value) => sem_const_is_fully_ground(db, value.value()),
            ConstRepr::Term(term) => const_ty_is_fully_ground(db, *term),
            ConstRepr::Deferred(_) => false,
        },
        ConstTyData::Abstract(expr, ty) => {
            ty_is_fully_ground(db, *ty) && const_expr_is_fully_ground(db, *expr)
        }
        ConstTyData::UnEvaluated {
            ty, generic_args, ..
        } => {
            ty.is_some_and(|ty| ty_is_fully_ground(db, ty))
                && generic_args
                    .iter()
                    .copied()
                    .all(|arg| ty_is_fully_ground(db, arg))
        }
    }
}

pub fn concretize_const_ty_if_ground<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    env: ConstCanonEnv<'db>,
) -> Option<ConstTyId<'db>> {
    if !const_ty_is_fully_ground(db, const_ty) {
        return None;
    }

    match const_ty.data(db) {
        ConstTyData::Value(..) | ConstTyData::Description(..) => Some(const_ty),
        ConstTyData::Computation { .. } => Some(const_ty.evaluate(db, Some(const_ty.ty(db)))),
        ConstTyData::UnEvaluated { ty, .. } => {
            let expected_ty = (*ty).unwrap_or_else(|| const_ty.ty(db));
            let evaluated = const_ty.evaluate(db, Some(expected_ty));
            if let ConstTyData::Abstract(expr, expected_ty) = evaluated.data(db) {
                evaluate_type_level_const_expr(db, *expr, *expected_ty, env).or(Some(evaluated))
            } else {
                Some(evaluated)
            }
        }
        ConstTyData::Abstract(expr, expected_ty) => {
            evaluate_type_level_const_expr(db, *expr, *expected_ty, env)
        }
        ConstTyData::TyVar(..)
        | ConstTyData::TyParam(..)
        | ConstTyData::Hole(..)
        | ConstTyData::Invalid(..) => None,
    }
}

fn canonicalize_const_ty_for_display<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    env: ConstCanonEnv<'db>,
) -> Option<ConstTyId<'db>> {
    let ConstTyData::UnEvaluated { ty, .. } = const_ty.data(db) else {
        return None;
    };
    let expected_ty = (*ty).unwrap_or_else(|| const_ty.ty(db));
    let evaluated = const_ty.evaluate(db, Some(expected_ty));
    if evaluated == const_ty || evaluated.ty(db).has_invalid(db) {
        return None;
    }
    Some(
        if let ConstTyData::Abstract(expr, expected_ty) = evaluated.data(db) {
            evaluate_type_level_const_expr(db, *expr, *expected_ty, env).unwrap_or(evaluated)
        } else {
            evaluated
        },
    )
}

pub fn complete_default_const_args_for_identity<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    assumptions: PredicateListId<'db>,
) -> TyId<'db> {
    let (base, args) = ty.decompose_ty_app(db);
    let TyData::TyBase(base_ty) = base.data(db) else {
        return ty;
    };
    let (param_set, trait_self) = match base_ty {
        TyBase::Adt(adt) => match adt.as_generic_param_owner(db) {
            Some(owner) => (collect_generic_params(db, owner), None),
            None => return ty,
        },
        TyBase::Func(func) => match *func {
            CallableDef::Func(def) => (collect_generic_params(db, def.into()), None),
            CallableDef::VariantCtor(_) => return ty,
        },
        _ => return ty,
    };
    let explicit_offset = param_set.offset_to_explicit_params_position(db);
    if args.len() <= explicit_offset {
        return ty;
    }
    let completed_args = param_set.complete_explicit_args(
        db,
        trait_self,
        &args[explicit_offset..],
        assumptions,
        ConstDefaultCompletion::evaluate(None),
        // No application path: `= _` defaults complete as opaque holes here,
        // so there is no structural identity to mint.
        None,
    );
    if completed_args.len() == args.len().saturating_sub(explicit_offset) {
        return ty;
    }
    let mut full_args = args[..explicit_offset].to_vec();
    full_args.extend(completed_args);
    TyId::foldl(db, base, &full_args)
}

pub fn canonicalize_const_ty_for_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    env: ConstCanonEnv<'db>,
    mode: ConstCanonMode,
) -> ConstTyId<'db> {
    let const_ty = if let Some(inst) = env.assoc_ty_subst {
        let folded = TyId::const_ty(db, const_ty).fold_with(db, &mut AssocTySubst::new(inst));
        let TyData::ConstTy(const_ty) = folded.data(db) else {
            return const_ty;
        };
        *const_ty
    } else {
        const_ty
    };
    let env = env.without_assoc_ty_subst();

    let canonicalized = match const_ty.data(db) {
        ConstTyData::TyVar(var, ty) => ConstTyId::new(
            db,
            ConstTyData::TyVar(var.clone(), canonicalize_ty_for_mode(db, *ty, env, mode)),
        ),
        ConstTyData::TyParam(param, ty) => ConstTyId::new(
            db,
            ConstTyData::TyParam(param.clone(), canonicalize_ty_for_mode(db, *ty, env, mode)),
        ),
        ConstTyData::Hole(ty, hole_id) => ConstTyId::new(
            db,
            ConstTyData::Hole(canonicalize_ty_for_mode(db, *ty, env, mode), *hole_id),
        ),
        ConstTyData::Value(value) => const_ty_from_sem_const(
            db,
            canonicalize_sem_const_for_mode(db, value.value(), env, mode),
        ),
        ConstTyData::Description(value) => {
            const_ty_from_sem_const(db, canonicalize_sem_const_for_mode(db, *value, env, mode))
        }
        ConstTyData::Invalid(ty) => ConstTyId::new(
            db,
            ConstTyData::Invalid(canonicalize_ty_for_mode(db, *ty, env, mode)),
        ),
        ConstTyData::Computation {
            description,
            source,
        } => {
            let mut folder = CanonicalizeInvocation {
                env,
                mode: ConstCanonMode::Stored,
            };
            ConstTyId::new(
                db,
                ConstTyData::Computation {
                    description: Box::new(description.as_ref().clone().fold_with(db, &mut folder)),
                    source: source.fold_with(db, &mut folder),
                },
            )
        }
        ConstTyData::Abstract(expr, ty) => ConstTyId::new(
            db,
            ConstTyData::Abstract(
                canonicalize_const_expr_for_mode(db, *expr, env, mode),
                canonicalize_ty_for_mode(db, *ty, env, mode),
            ),
        ),
        ConstTyData::UnEvaluated {
            body,
            ty,
            const_def,
            generic_args,
            preserve_unevaluated,
            defer_validation,
        } => ConstTyId::new(
            db,
            ConstTyData::UnEvaluated {
                body: *body,
                ty: ty.map(|ty| canonicalize_ty_for_mode(db, ty, env, mode)),
                const_def: *const_def,
                generic_args: generic_args
                    .iter()
                    .copied()
                    .map(|arg| canonicalize_ty_for_mode(db, arg, env, mode))
                    .collect(),
                preserve_unevaluated: *preserve_unevaluated,
                defer_validation: *defer_validation,
            },
        ),
    };

    if !matches!(mode, ConstCanonMode::Stored)
        && let ConstTyData::Computation { description, .. } = canonicalized.data(db)
    {
        let evaluated = canonicalized.evaluate(db, Some(description.ty()));
        if evaluated != canonicalized {
            return canonicalize_const_ty_for_mode(db, evaluated, env, mode);
        }
        if let ConstRepr::Term(term) = description.repr() {
            return canonicalize_const_ty_for_mode(db, *term, env, mode);
        }
    }

    match mode {
        ConstCanonMode::Stored => canonicalized,
        ConstCanonMode::Identity => concretize_const_ty_if_ground(db, canonicalized, env)
            .or_else(|| {
                let ConstTyData::UnEvaluated { ty: Some(ty), .. } = canonicalized.data(db) else {
                    return None;
                };
                let evaluated = canonicalized.evaluate(db, Some(*ty));
                (evaluated != canonicalized && evaluated.ty(db).invalid_cause(db).is_none())
                    .then(|| canonicalize_const_ty_for_mode(db, evaluated, env, mode))
            })
            .unwrap_or(canonicalized),
        ConstCanonMode::Display => concretize_const_ty_if_ground(db, canonicalized, env)
            .or_else(|| canonicalize_const_ty_for_display(db, canonicalized, env))
            .unwrap_or(canonicalized),
    }
}

fn canonicalize_sem_const_for_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
    env: ConstCanonEnv<'db>,
    mode: ConstCanonMode,
) -> SemConstId<'db> {
    if let SemConstValue::Description(term) = value.value(db) {
        let term = canonicalize_const_ty_for_mode(db, term, env, mode);
        return sem_const_from_ty(db, TyId::const_ty(db, term))
            .unwrap_or_else(|| SemConstId::new(db, SemConstValue::Description(term)));
    }
    let canonicalize = |ty| canonicalize_ty_for_mode(db, ty, env, mode);
    let children = |children: Box<[SemConstId<'db>]>| {
        children
            .iter()
            .copied()
            .map(|child| canonicalize_sem_const_for_mode(db, child, env, mode))
            .collect::<Vec<_>>()
            .into_boxed_slice()
    };
    let value = match value.value(db) {
        SemConstValue::Unit => SemConstValue::Unit,
        SemConstValue::Scalar { ty, value } => SemConstValue::Scalar {
            ty: canonicalize(ty),
            value,
        },
        SemConstValue::Description(..) => unreachable!(),
        SemConstValue::Tuple { ty, elems } => SemConstValue::Tuple {
            ty: canonicalize(ty),
            elems: children(elems),
        },
        SemConstValue::Struct { ty, fields } => SemConstValue::Struct {
            ty: canonicalize(ty),
            fields: children(fields),
        },
        SemConstValue::Array { ty, elems } => SemConstValue::Array {
            ty: canonicalize(ty),
            elems: children(elems),
        },
        SemConstValue::Enum {
            ty,
            variant,
            fields,
        } => SemConstValue::Enum {
            ty: canonicalize(ty),
            variant,
            fields: children(fields),
        },
    };
    SemConstId::new(db, value)
}

pub fn canonicalize_ty_for_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    env: ConstCanonEnv<'db>,
    mode: ConstCanonMode,
) -> TyId<'db> {
    fn canonicalize_ty_impl<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        env: ConstCanonEnv<'db>,
        mode: ConstCanonMode,
        finalize_self: bool,
    ) -> TyId<'db> {
        let ty = if let Some(inst) = env.assoc_ty_subst {
            ty.fold_with(db, &mut AssocTySubst::new(inst))
        } else {
            ty
        };
        let env = env.without_assoc_ty_subst();

        let mut ty = match ty.data(db) {
            TyData::TyApp(abs, arg) => TyId::app(
                db,
                canonicalize_ty_impl(db, *abs, env, mode, false),
                canonicalize_ty_impl(db, *arg, env, mode, true),
            ),
            TyData::ConstTy(const_ty) => {
                TyId::const_ty(db, canonicalize_const_ty_for_mode(db, *const_ty, env, mode))
            }
            TyData::AssocTy(assoc) => TyId::assoc_ty(
                db,
                canonicalize_trait_inst_for_mode(db, assoc.trait_, env, mode),
                assoc.name,
            ),
            TyData::QualifiedTy(trait_inst) => TyId::qualified_ty(
                db,
                canonicalize_trait_inst_for_mode(db, *trait_inst, env, mode),
            ),
            TyData::TyVar(_)
            | TyData::TyParam(_)
            | TyData::TyBase(_)
            | TyData::Never
            | TyData::Invalid(_) => ty,
        };

        if finalize_self && !matches!(mode, ConstCanonMode::Stored) {
            ty = complete_default_const_args_for_identity(db, ty, env.assumptions);
            ty = normalize_ty(db, ty, env.scope, env.assumptions);
        }

        ty
    }

    canonicalize_ty_impl(db, ty, env, mode, true)
}

pub fn canonicalize_trait_inst_for_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    trait_inst: TraitInstId<'db>,
    env: ConstCanonEnv<'db>,
    mode: ConstCanonMode,
) -> TraitInstId<'db> {
    let trait_inst = if let Some(inst) = env.assoc_ty_subst {
        trait_inst.fold_with(db, &mut AssocTySubst::new(inst))
    } else {
        trait_inst
    };
    let env = env.without_assoc_ty_subst();
    let mut assoc_type_bindings: Vec<_> = trait_inst
        .assoc_type_bindings(db)
        .iter()
        .map(|(name, &ty)| (*name, canonicalize_ty_for_mode(db, ty, env, mode)))
        .collect();
    assoc_type_bindings.sort_by(|(lhs, _), (rhs, _)| lhs.data(db).cmp(rhs.data(db)));
    TraitInstId::new(
        db,
        trait_inst.def(db),
        trait_inst
            .args(db)
            .iter()
            .copied()
            .map(|ty| canonicalize_ty_for_mode(db, ty, env, mode))
            .collect::<Vec<_>>(),
        assoc_type_bindings.into_iter().collect::<IndexMap<_, _>>(),
    )
}

fn const_canon_env<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
) -> Option<ConstCanonEnv<'db>> {
    match const_ty.data(db) {
        ConstTyData::Computation { source, .. } => Some(ConstCanonEnv::new(
            source.impl_env(db).normalization_scope(db),
            source.impl_env(db).assumptions(db),
            None,
        )),
        ConstTyData::UnEvaluated { body, .. } => Some(ConstCanonEnv::new(
            body.scope(),
            assumptions_for_body(db, *body),
            None,
        )),
        ConstTyData::Abstract(expr, _) => match expr.data(db) {
            ConstExpr::Invocation(invocation) => {
                let impl_env = invocation.key.impl_env(db);
                Some(ConstCanonEnv::new(
                    impl_env.normalization_scope(db),
                    impl_env.assumptions(db),
                    None,
                ))
            }
            ConstExpr::TraitConst(assoc) => Some(ConstCanonEnv::new(
                assoc.origin_scope(),
                assoc.assumptions(),
                None,
            )),
            ConstExpr::InherentConst(use_) => Some(ConstCanonEnv::new(
                use_.origin_scope(),
                use_.assumptions(),
                None,
            )),
            _ => None,
        },
        _ => None,
    }
}

pub(crate) fn normalize_const_tys_for_comparison<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> TyId<'db> {
    let TyData::ConstTy(const_ty) = ty.data(db) else {
        return ty;
    };
    if let Some(env) = const_canon_env(db, *const_ty) {
        return canonicalize_ty_for_mode(db, ty, env, ConstCanonMode::Identity);
    }

    match const_ty.data(db) {
        ConstTyData::UnEvaluated {
            ty: Some(expected_ty),
            ..
        } => {
            let normalized = const_ty.evaluate(db, Some(*expected_ty));
            if normalized.ty(db).invalid_cause(db).is_none()
                && matches!(
                    normalized.data(db),
                    ConstTyData::Value(..)
                        | ConstTyData::Description(..)
                        | ConstTyData::Abstract(..)
                )
            {
                if let ConstTyData::Abstract(expr, expected_ty) = normalized.data(db) {
                    evaluate_type_level_int_const_expr(db, *expr, *expected_ty).map_or_else(
                        || TyId::const_ty(db, normalized),
                        |evaluated| TyId::const_ty(db, evaluated),
                    )
                } else {
                    TyId::const_ty(db, normalized)
                }
            } else {
                ty
            }
        }
        ConstTyData::Abstract(expr, expected_ty) => {
            evaluate_type_level_int_const_expr(db, *expr, *expected_ty)
                .map_or(ty, |evaluated| TyId::const_ty(db, evaluated))
        }
        _ => ty,
    }
}

pub(crate) struct ValidatedUnEvaluatedConst<'db> {
    pub const_ty: ConstTyId<'db>,
    pub expected_ty: TyId<'db>,
}

pub(crate) fn retype_hole_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstTyId<'db>> {
    matches!(const_ty.data(db), ConstTyData::Hole(..)).then(|| const_ty.with_ty(db, expected_ty))
}

pub(crate) fn validate_unevaluated_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    expected_ty: Option<TyId<'db>>,
) -> Result<ValidatedUnEvaluatedConst<'db>, InvalidCause<'db>> {
    let ConstTyData::UnEvaluated {
        body,
        ty: const_ty_ty,
        const_def,
        generic_args,
        ..
    } = const_ty.data(db)
    else {
        return Err(InvalidCause::Other);
    };

    let Some(expected_ty) = expected_ty.or(*const_ty_ty) else {
        return Err(InvalidCause::InvalidConstTyExpr { body: *body });
    };
    let check_ty = if generic_args.is_empty() {
        expected_ty
    } else {
        const_ty_ty.unwrap_or(expected_ty)
    };
    let const_ty = const_ty.with_ty(db, expected_ty);

    let (diags, typed_body) = match const_def {
        Some(const_def) => {
            let result = check_const_body(db, *const_def);
            (result.0.clone(), result.1.clone())
        }
        None => {
            let result = check_anon_const_body(db, *body, check_ty);
            (result.0.clone(), result.1.clone())
        }
    };

    if let Some((expected, given)) = diags.iter().find_map(|diag| match diag {
        FuncBodyDiag::Body(BodyDiag::TypeMismatch {
            expected, given, ..
        }) => Some((*expected, *given)),
        _ => None,
    }) {
        if matches!(body.scope().parent_item(db), Some(ItemKind::ImplTrait(_))) {
            return Err(InvalidCause::Other);
        }
        return Err(InvalidCause::ConstTyMismatch { expected, given });
    }

    if let Some((primary, predicate, reason)) = diags.iter().find_map(|diag| match diag {
        FuncBodyDiag::Body(BodyDiag::ConstRequirementNotSatisfied {
            primary,
            predicate,
            reason,
        }) => Some((primary.clone(), predicate.clone(), reason.clone())),
        _ => None,
    }) {
        return Err(InvalidCause::ConstRequirementNotSatisfied {
            primary,
            predicate,
            reason,
        });
    }

    if !diags.is_empty() {
        if let Some(cause) = typed_body
            .body()
            .and_then(|body| typed_body.expr_ty(db, body.expr(db)).invalid_cause(db))
            .or_else(|| typed_body.result_ty().invalid_cause(db))
        {
            return Err(cause);
        }
        return Err(InvalidCause::InvalidConstTyExpr { body: *body });
    }

    if const_def.is_some() {
        let owner = BodyOwner::AnonConstBody {
            body: *body,
            expected: expected_ty,
        };
        if let EvalOutcome::Failed(failure) = eval_body_owner_const(db, owner, generic_args.clone())
        {
            return Err(invalid_cause_from_eval_failure(db, owner, failure));
        }
    }

    check_const_ty(
        db,
        check_ty,
        Some(expected_ty),
        &mut UnificationTable::new(db),
    )?;
    Ok(ValidatedUnEvaluatedConst {
        const_ty,
        expected_ty,
    })
}

#[salsa::interned]
#[derive(Debug)]
pub struct ConstTyId<'db> {
    #[return_ref]
    pub data: ConstTyData<'db>,
}

fn u256_modulus() -> BigUint {
    BigUint::one() << 256usize
}

fn bigint_to_u256_word(value: &BigInt) -> Option<BigUint> {
    let modulus = u256_modulus();
    match value.sign() {
        Sign::Minus => {
            let abs = value.magnitude();
            if abs > &modulus {
                return None;
            }
            if abs.is_zero() {
                Some(BigUint::zero())
            } else {
                Some(&modulus - abs)
            }
        }
        _ => value
            .to_biguint()
            .and_then(|value| (value < modulus).then_some(value)),
    }
}

#[derive(Clone, Copy, Debug)]
enum ConstIntError {
    Overflow,
    DivisionByZero,
    NegativeExponent,
    /// The expression is not a pure integer expression. Callers may fall
    /// through to full CTFE rather than reporting an arithmetic error.
    NotIntExpr,
}

fn const_int_error(fault: PrimitiveFault) -> ConstIntError {
    match fault {
        PrimitiveFault::ArithmeticOverflow => ConstIntError::Overflow,
        PrimitiveFault::DivisionByZero => ConstIntError::DivisionByZero,
        PrimitiveFault::NegativeExponent => ConstIntError::NegativeExponent,
        PrimitiveFault::InvalidPowerExponent
        | PrimitiveFault::OutsideSupportedSubset
        | PrimitiveFault::InvalidCast
        | PrimitiveFault::UnsupportedCast => ConstIntError::NotIntExpr,
    }
}

fn invalid_cause_from_const_int_error<'db>(
    body: Body<'db>,
    expr: ExprId,
    err: ConstIntError,
) -> Option<InvalidCause<'db>> {
    match err {
        ConstIntError::Overflow => Some(InvalidCause::ConstEvalArithmeticOverflow { body, expr }),
        ConstIntError::DivisionByZero => Some(InvalidCause::ConstEvalDivisionByZero { body, expr }),
        ConstIntError::NegativeExponent => {
            Some(InvalidCause::ConstEvalNegativeExponent { body, expr })
        }
        ConstIntError::NotIntExpr => None,
    }
}

fn eval_int_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr: &Expr<'db>,
    expected: Option<TyId<'db>>,
    generic_args: &[TyId<'db>],
) -> Result<BigInt, ConstIntError> {
    match expr {
        Expr::Block(stmts) => {
            let [stmt] = stmts.as_slice() else {
                return Err(ConstIntError::NotIntExpr);
            };
            let Partial::Present(stmt) = stmt.data(db, body) else {
                return Err(ConstIntError::NotIntExpr);
            };
            let Stmt::Expr(expr_id) = stmt else {
                return Err(ConstIntError::NotIntExpr);
            };
            let Partial::Present(inner) = expr_id.data(db, body) else {
                return Err(ConstIntError::NotIntExpr);
            };
            eval_int_expr(db, body, inner, expected, generic_args)
        }
        Expr::Lit(LitKind::Int(value)) => Ok(BigInt::from(value.data(db).clone())),
        Expr::Un(inner, op) => {
            let Partial::Present(inner) = inner.data(db, body) else {
                return Err(ConstIntError::Overflow);
            };
            let value = eval_int_expr(db, body, inner, expected, generic_args)?;
            if matches!(op, UnOp::Minus) && expected.is_none() {
                return Err(ConstIntError::Overflow);
            }
            execute_source_int_unary(
                db,
                expected.unwrap_or_else(|| TyId::u256(db)),
                ArithmeticMode::Checked,
                *op,
                value,
            )
            .map_err(const_int_error)
        }
        Expr::Bin(lhs_id, rhs_id, op) => {
            let Partial::Present(lhs) = lhs_id.data(db, body) else {
                return Err(ConstIntError::Overflow);
            };
            let Partial::Present(rhs) = rhs_id.data(db, body) else {
                return Err(ConstIntError::Overflow);
            };
            let expected = expected.unwrap_or_else(|| TyId::u256(db));
            let lhs = eval_int_expr(db, body, lhs, Some(expected), generic_args)?;
            let rhs = eval_int_expr(db, body, rhs, Some(expected), generic_args)?;
            let BinOp::Arith(op) = op else {
                return Err(ConstIntError::NotIntExpr);
            };
            execute_source_int_binary(db, expected, ArithmeticMode::Checked, *op, lhs, rhs)
                .map_err(const_int_error)
        }
        Expr::Path(path) => {
            if !generic_args.is_empty() {
                return Err(ConstIntError::NotIntExpr);
            }
            let Some(path) = path.to_opt() else {
                return Err(ConstIntError::NotIntExpr);
            };
            let assumptions = assumptions_for_body(db, body);
            let resolved = resolve_path(db, path, body.scope(), assumptions, true)
                .map_err(|_| ConstIntError::NotIntExpr)?;
            let const_ty = match resolved {
                PathRes::Const(const_def, declared_ty) => {
                    let body = const_def
                        .body(db)
                        .to_opt()
                        .ok_or(ConstIntError::NotIntExpr)?;
                    ConstTyId::from_body(db, body, Some(declared_ty), Some(const_def))
                }
                PathRes::TraitConst(_, inst, name) => {
                    let solve_cx =
                        TraitSolveCx::new(db, body.scope()).with_assumptions(assumptions);
                    const_ty_from_trait_const(db, solve_cx, inst, name)
                        .ok_or(ConstIntError::NotIntExpr)?
                }
                PathRes::InherentConst(recv_ty, impl_, name) => {
                    const_ty_from_inherent_const(db, impl_, recv_ty, name)
                        .ok_or(ConstIntError::NotIntExpr)?
                }
                _ => return Err(ConstIntError::NotIntExpr),
            };
            let Some(value) = const_ty.evaluate(db, None).integer_value(db) else {
                return Err(ConstIntError::NotIntExpr);
            };
            let (bits, signed) = int_ty_shape(db, expected.unwrap_or_else(|| TyId::u256(db)))
                .ok_or(ConstIntError::NotIntExpr)?;
            Ok(normalize_int_to_shape(value, bits, signed))
        }
        _ => Err(ConstIntError::NotIntExpr),
    }
}

pub(super) fn try_eval_const_int_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    expr: ExprId,
    expected_ty: TyId<'db>,
) -> Option<BigInt> {
    let Partial::Present(expr) = expr.data(db, body) else {
        return None;
    };
    eval_int_expr(
        db,
        body,
        expr,
        int_ty_shape(db, expected_ty).map(|_| expected_ty),
        &[],
    )
    .ok()
}

#[salsa::tracked(cycle_initial=evaluate_const_ty_cycle_initial, cycle_fn=evaluate_const_ty_cycle_recover)]
pub(crate) fn evaluate_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    expected_ty: Option<TyId<'db>>,
) -> ConstTyId<'db> {
    if let Some(expected_ty) = expected_ty
        && let Some(retyped) = retype_hole_const_ty(db, const_ty, expected_ty)
    {
        return retyped;
    }
    if matches!(const_ty.data(db), ConstTyData::Hole(..)) {
        return const_ty;
    }

    if let ConstTyData::Computation {
        description,
        source,
    } = const_ty.data(db)
    {
        if let ConstRepr::Term(term) = description.repr()
            && let ConstTyData::Abstract(expr, _) = term.data(db)
            && expr.is_opaque_extern(db)
        {
            return const_ty;
        }
        return match force_const_description(
            db,
            description,
            CtfeConfig::default(),
            SemOrigin::Body(source.owner(db)),
        ) {
            EvalOutcome::Ready(value) => {
                const_ty_from_sem_const(db, value.value()).evaluate(db, expected_ty)
            }
            EvalOutcome::Blocked(_) => const_ty,
            EvalOutcome::Failed(failure) => ConstTyId::invalid(
                db,
                invalid_cause_from_eval_failure(db, source.owner(db), failure),
            ),
        };
    }

    if let ConstTyData::Abstract(expr, _) = const_ty.data(db)
        && !expr.is_opaque_extern(db)
        && !matches!(
            expr.data(db),
            ConstExpr::TraitConst(_) | ConstExpr::InherentConst(_)
        )
    {
        match force_const_term_value(db, const_ty, CtfeConfig::default(), SemOrigin::Synthetic) {
            EvalOutcome::Ready(value) => {
                return const_ty_from_sem_const(db, value.value()).evaluate(db, expected_ty);
            }
            EvalOutcome::Blocked(_) => {}
            EvalOutcome::Failed(failure) => {
                let cause = if let ConstExpr::Invocation(invocation) = expr.data(db) {
                    invalid_cause_from_eval_failure(db, invocation.key.owner(db), failure)
                } else {
                    InvalidCause::Other
                };
                return ConstTyId::invalid(db, cause);
            }
        }
    }

    if let ConstTyData::Abstract(expr, ty) = const_ty.data(db)
        && let ConstExpr::InherentConst(use_) = expr.data(db)
        && let Some(resolved) = const_ty_from_inherent_const_use(db, *use_)
    {
        let evaluated = resolved.evaluate(db, expected_ty.or(Some(*ty)));
        if evaluated.ty(db).has_invalid(db) || !selected_const_requires_original_use(db, evaluated)
        {
            return evaluated;
        }
    }

    if let ConstTyData::Abstract(expr, ty) = const_ty.data(db)
        && let ConstExpr::TraitConst(assoc) = expr.data(db)
    {
        if let Some(resolved) = const_ty_from_assoc_const_use(db, *assoc) {
            let evaluated = resolved.evaluate(db, expected_ty.or(Some(*ty)));
            if evaluated.ty(db).has_invalid(db) {
                return evaluated;
            }
            if selected_const_requires_original_use(db, evaluated) {
                return const_ty;
            }
            return evaluated;
        }
        // Unresolvable here (e.g. `Self` is still generic): keep the const
        // abstract, but adopt the use position's integer shape the same way
        // resolved trait consts are normalized into it on evaluation.
        if let Some(expected) = expected_ty
            && expected != *ty
            && int_ty_shape(db, expected).is_some()
            && int_ty_shape(db, *ty).is_some()
        {
            return const_ty.with_ty(db, expected);
        }
        return const_ty;
    }

    let (body, const_ty_ty, generic_args) = match const_ty.data(db) {
        ConstTyData::UnEvaluated {
            body,
            ty,
            generic_args,
            ..
        } => (*body, *ty, generic_args.clone()),
        _ => {
            let const_ty_ty = const_ty.ty(db);
            return match check_const_ty(
                db,
                const_ty_ty,
                expected_ty,
                &mut UnificationTable::new(db),
            ) {
                Ok(_) => const_ty,
                Err(cause) => {
                    let ty = TyId::invalid(db, cause);
                    return const_ty.swap_ty(db, ty);
                }
            };
        }
    };

    let expected_ty = expected_ty.or(const_ty_ty);
    let check_ty = if generic_args.is_empty() {
        expected_ty
    } else {
        const_ty_ty.or(expected_ty)
    };

    let Partial::Present(expr) = body.expr(db).data(db, body) else {
        return ConstTyId::invalid(db, InvalidCause::ParseError);
    };

    let expr = expr.clone();

    if generic_args.is_empty()
        && let Expr::Path(path) = &expr
    {
        let Some(path) = path.to_opt() else {
            return ConstTyId::invalid(db, InvalidCause::ParseError);
        };

        let assumptions = assumptions_for_body(db, body);
        if let Ok(resolved_path) = resolve_path(db, path, body.scope(), assumptions, true) {
            match resolved_path {
                PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                    if let TyData::ConstTy(const_ty) = ty.data(db) {
                        if !generic_args.is_empty()
                            && let ConstTyData::TyParam(param, _) = const_ty.data(db)
                            && let Some(arg) = generic_args.get(param.idx).copied()
                            && let TyData::ConstTy(arg_const) = arg.data(db)
                        {
                            let expected = expected_ty.or(Some(arg_const.ty(db)));
                            return arg_const.evaluate(db, expected);
                        }
                        return const_ty.evaluate(db, expected_ty);
                    }
                }
                PathRes::Const(const_def, ty) => {
                    if let Some(body) = const_def.body(db).to_opt() {
                        let const_ty = ConstTyId::from_body(db, body, Some(ty), Some(const_def));
                        let expected = expected_ty.or(Some(ty));
                        return const_ty.evaluate(db, expected);
                    }
                }
                PathRes::TraitConst(recv_ty, inst, name) => {
                    let mut args = inst.args(db).clone();
                    if let Some(self_arg) = args.first_mut() {
                        *self_arg = recv_ty;
                    }
                    let inst = TraitInstId::new(
                        db,
                        inst.def(db),
                        args,
                        inst.assoc_type_bindings(db).clone(),
                    );

                    let mk_abstract = |expected_ty: TyId<'db>| {
                        let expr = ConstExprId::new(
                            db,
                            ConstExpr::TraitConst(AssocConstUse::new(
                                body.scope(),
                                assumptions,
                                inst,
                                name,
                            )),
                        );
                        ConstTyId::new(db, ConstTyData::Abstract(expr, expected_ty))
                    };

                    let solve_cx =
                        TraitSolveCx::new(db, body.scope()).with_assumptions(assumptions);
                    if let Some(const_ty) = const_ty_from_trait_const(db, solve_cx, inst, name) {
                        let evaluated = const_ty.evaluate(db, expected_ty);
                        if evaluated.ty(db).has_invalid(db) {
                            return evaluated;
                        }
                        if selected_const_requires_original_use(db, evaluated) {
                            return mk_abstract(expected_ty.unwrap_or_else(|| const_ty.ty(db)));
                        }
                        return evaluated;
                    }

                    if let Some(expected_ty) = expected_ty {
                        return mk_abstract(expected_ty);
                    }
                }
                PathRes::InherentConst(recv_ty, impl_, name) => {
                    let mk_abstract = |expected_ty: TyId<'db>| {
                        let use_ = super::assoc_const::InherentConstUse::new(
                            body.scope(),
                            assumptions,
                            impl_,
                            recv_ty,
                            name,
                        );
                        let expr = ConstExprId::new(db, ConstExpr::InherentConst(use_));
                        ConstTyId::new(db, ConstTyData::Abstract(expr, expected_ty))
                    };

                    if let Some(const_ty) = const_ty_from_inherent_const(db, impl_, recv_ty, name) {
                        let evaluated = const_ty.evaluate(db, expected_ty);
                        if evaluated.ty(db).has_invalid(db) {
                            return evaluated;
                        }
                        if selected_const_requires_original_use(db, evaluated) {
                            return mk_abstract(expected_ty.unwrap_or_else(|| const_ty.ty(db)));
                        }
                        return evaluated;
                    }

                    if let Some(expected_ty) = expected_ty {
                        return mk_abstract(expected_ty);
                    }
                }
                PathRes::EnumVariant(variant) if variant.ty.is_unit_variant_only_enum(db) => {
                    let const_ty = const_ty_from_sem_const(
                        db,
                        enum_const(
                            db,
                            variant.ty,
                            VariantIndex(variant.variant.idx),
                            Box::new([]),
                        ),
                    );
                    return const_ty.evaluate(db, expected_ty);
                }
                _ => {}
            }
        }

        // If the path failed to resolve but looks like a path to a value
        // (e.g., a trait associated const like `Type::CONST`), keep it
        // unevaluated and assume the expected type if available, avoiding
        // spurious diagnostics here. Downstream checks will validate usage.
        if path.parent(db).is_some() {
            return ConstTyId::from_body(db, body, expected_ty, None);
        }

        return ConstTyId::invalid(db, InvalidCause::InvalidConstTyExpr { body });
    }

    // Try BigInt-based evaluation for integer arithmetic expressions (checked arithmetic).
    if matches!(
        expr,
        Expr::Block(..) | Expr::Un(..) | Expr::Bin(..) | Expr::Lit(LitKind::Int(..))
    ) {
        let expected_int_ty = expected_ty.filter(|ty| int_ty_shape(db, *ty).is_some());
        match eval_int_expr(db, body, &expr, expected_int_ty, &generic_args) {
            Ok(value) => {
                if let Some(word) = bigint_to_u256_word(&value) {
                    let mut table = UnificationTable::new(db);
                    let ty = table.new_var(TyVarSort::Integral, &Kind::Star);
                    return match check_const_ty(db, ty, expected_ty, &mut table) {
                        Ok(ty) => {
                            const_ty_from_sem_const(db, int_const(db, ty, BigInt::from(word)))
                        }
                        Err(err) => ConstTyId::invalid(db, err),
                    };
                }
            }
            Err(ConstIntError::NotIntExpr) => {
                // Expression contains constructs we can't evaluate with BigInt
                // (e.g. function calls). Fall through to CTFE.
            }
            Err(err) => {
                // Genuine arithmetic error (overflow, division by zero, etc.).
                // For Block/Un/Bin, report error. For plain int literals, fall through to CTFE.
                if matches!(expr, Expr::Block(..) | Expr::Un(..) | Expr::Bin(..))
                    && let Some(cause) =
                        invalid_cause_from_const_int_error(body, body.expr(db), err)
                {
                    return ConstTyId::invalid(db, cause);
                }
            }
        }
    }

    if check_ty.is_none() {
        return ConstTyId::invalid(db, InvalidCause::InvalidConstTyExpr { body });
    }
    let validated = match validate_unevaluated_const_ty(db, const_ty, expected_ty) {
        Ok(validated) => validated,
        Err(InvalidCause::InvalidConstTyExpr { body }) => {
            return ConstTyId::invalid(
                db,
                InvalidCause::ConstEvalUnsupported {
                    body,
                    expr: body.expr(db),
                },
            );
        }
        Err(cause) => return ConstTyId::invalid(db, cause),
    };

    let owner = super::ty_check::BodyOwner::AnonConstBody {
        body,
        expected: validated.expected_ty,
    };
    let key = SemanticInstanceKey::new(
        db,
        owner,
        GenericSubst::new(db, generic_args.clone()),
        EffectProviderSubst::empty(db),
        ImplEnv::empty(db, owner.scope()),
    );
    let request = const_computation_for_instance(db, key, Vec::new());
    let evaluated = match describe_const_computation(db, request, CtfeConfig::default()) {
        EvalOutcome::Ready(description) => match description.repr() {
            ConstRepr::Value(value) => const_ty_from_sem_const(db, value.value()),
            ConstRepr::Term(_) => ConstTyId::new(
                db,
                ConstTyData::Computation {
                    description: Box::new(description),
                    source: key,
                },
            ),
            ConstRepr::Deferred(_) => validated.const_ty,
        },
        EvalOutcome::Blocked(_) => validated.const_ty,
        EvalOutcome::Failed(failure) => {
            return ConstTyId::invalid(db, invalid_cause_from_eval_failure(db, owner, failure));
        }
    };

    let mut table = UnificationTable::new(db);
    match check_const_ty(
        db,
        evaluated.ty(db),
        Some(validated.expected_ty),
        &mut table,
    ) {
        Ok(ty) => evaluated.swap_ty(db, ty),
        Err(cause) => evaluated.swap_ty(db, TyId::invalid(db, cause)),
    }
}

pub(crate) fn invalid_cause_from_ctfe_error<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: crate::analysis::ty::ty_check::BodyOwner<'db>,
    err: CtfeError<'db>,
) -> InvalidCause<'db> {
    let (owner, root_err, origin) = root_ctfe_error(db, owner, &err);
    let Some(body) = owner.body(db) else {
        return InvalidCause::Other;
    };
    let expr = origin_expr_for_const_eval_diag(db, body, origin);
    match root_err {
        CtfeError::AssertionFailed { message, .. } => InvalidCause::ConstEvalAssertionFailed {
            body,
            expr,
            message: message.clone(),
        },
        CtfeError::DivisionByZero { .. } => InvalidCause::ConstEvalDivisionByZero { body, expr },
        CtfeError::OutOfBounds { .. } => InvalidCause::ConstEvalOutOfBounds { body, expr },
        CtfeError::InvalidOperation { message, .. } => InvalidCause::ConstEvalInvalidOperation {
            body,
            expr,
            message: message.clone(),
        },
        CtfeError::InvalidBorrow { .. } => InvalidCause::ConstEvalInvalidBorrow { body, expr },
        CtfeError::InvalidProviderUse { .. } => {
            InvalidCause::ConstEvalInvalidProviderUse { body, expr }
        }
        CtfeError::VariantMismatch { .. } => InvalidCause::ConstEvalVariantMismatch { body, expr },
        CtfeError::UninitializedLocal { .. } => {
            InvalidCause::ConstEvalUninitializedLocal { body, expr }
        }
        CtfeError::ArithmeticOverflow { .. } => {
            InvalidCause::ConstEvalArithmeticOverflow { body, expr }
        }
        CtfeError::NegativeExponent { .. } => {
            InvalidCause::ConstEvalNegativeExponent { body, expr }
        }
        CtfeError::StepLimitExceeded { .. } => {
            InvalidCause::ConstEvalStepLimitExceeded { body, expr }
        }
        CtfeError::RecursionLimitExceeded { .. } => {
            InvalidCause::ConstEvalRecursionLimitExceeded { body, expr }
        }
        CtfeError::RecursiveConst { .. } => InvalidCause::ConstEvalRecursiveConst { body, expr },
        CtfeError::NonConstCall { .. } => InvalidCause::ConstEvalNonConstCall { body, expr },
        CtfeError::InvalidBody { .. } => InvalidCause::Other,
        CtfeError::NotConstEvaluable { .. } => InvalidCause::ConstEvalUnsupported { body, expr },
        CtfeError::CalleeError { .. } => {
            unreachable!("root_ctfe_error must unwrap callee failures")
        }
    }
}

pub(crate) fn invalid_cause_from_eval_failure<'db>(
    db: &'db dyn HirAnalysisDb,
    owner: BodyOwner<'db>,
    failure: EvalFailure<'db>,
) -> InvalidCause<'db> {
    match failure {
        EvalFailure::Ctfe(err) => invalid_cause_from_ctfe_error(db, owner, err),
        EvalFailure::Invariant { origin, message } => {
            let Some(body) = owner.body(db) else {
                return InvalidCause::Other;
            };
            InvalidCause::ConstEvalInvariant {
                body,
                expr: origin_expr_for_const_eval_diag(db, body, origin),
                message,
            }
        }
    }
}

fn root_ctfe_error<'a, 'db>(
    db: &'db dyn HirAnalysisDb,
    owner: crate::analysis::ty::ty_check::BodyOwner<'db>,
    err: &'a CtfeError<'db>,
) -> (
    crate::analysis::ty::ty_check::BodyOwner<'db>,
    &'a CtfeError<'db>,
    SemOrigin<'db>,
) {
    match err {
        CtfeError::CalleeError { callee, source, .. } => {
            root_ctfe_error(db, callee.key(db).owner(db), source)
        }
        CtfeError::NotConstEvaluable { origin }
        | CtfeError::AssertionFailed { origin, .. }
        | CtfeError::InvalidOperation { origin, .. }
        | CtfeError::InvalidBorrow { origin }
        | CtfeError::InvalidProviderUse { origin }
        | CtfeError::NonConstCall { origin }
        | CtfeError::InvalidBody { origin }
        | CtfeError::DivisionByZero { origin }
        | CtfeError::ArithmeticOverflow { origin }
        | CtfeError::NegativeExponent { origin }
        | CtfeError::OutOfBounds { origin }
        | CtfeError::VariantMismatch { origin }
        | CtfeError::UninitializedLocal { origin }
        | CtfeError::StepLimitExceeded { origin }
        | CtfeError::RecursionLimitExceeded { origin }
        | CtfeError::RecursiveConst { origin } => (owner, err, *origin),
    }
}

pub(crate) fn origin_expr_for_const_eval_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    origin: SemOrigin<'db>,
) -> crate::hir_def::ExprId {
    match origin {
        SemOrigin::Expr(expr) => expr,
        SemOrigin::Stmt(stmt) => {
            stmt_primary_expr_for_const_eval_diag(db, body, stmt).unwrap_or_else(|| body.expr(db))
        }
        SemOrigin::Body(_) | SemOrigin::Synthetic => body.expr(db),
    }
}

fn stmt_primary_expr_for_const_eval_diag<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    stmt: crate::hir_def::StmtId,
) -> Option<crate::hir_def::ExprId> {
    match stmt.data(db, body).clone().to_opt()? {
        Stmt::Let(_, _, expr) | Stmt::Return(expr) => expr,
        Stmt::Expr(expr) => Some(expr),
        Stmt::For(_, expr, _, _) => Some(expr),
        Stmt::While(_, _) | Stmt::Continue | Stmt::Break => None,
    }
}

pub(crate) fn const_ty_from_sem_const<'db>(
    db: &'db dyn HirAnalysisDb,
    value: SemConstId<'db>,
) -> ConstTyId<'db> {
    if let SemConstValue::Description(term) = value.value(db) {
        if crate::analysis::semantic::consts::verify_sem_const_description_shape(db, value).is_err()
        {
            return ConstTyId::invalid(db, InvalidCause::Other);
        }
        return term;
    }
    match SConst::from_trusted_source(db, value) {
        SConst::Value(value) => ConstTyId::new(db, ConstTyData::Value(value)),
        SConst::Description(value) | SConst::Evidence(value) => {
            ConstTyId::new(db, ConstTyData::Description(value))
        }
        SConst::Invalid(value) => ConstTyId::new(
            db,
            ConstTyData::Invalid(crate::analysis::semantic::sem_const_ty(db, value)),
        ),
        SConst::Ref(..) => unreachable!("interned constant cannot contain a selected reference"),
    }
}

pub(crate) fn assumptions_for_body<'db>(
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
) -> PredicateListId<'db> {
    let containing_func = match body.scope().parent_item(db) {
        Some(ItemKind::Func(func)) => Some(func),
        Some(ItemKind::Body(parent)) => parent.containing_func(db),
        _ => None,
    };
    if let Some(func) = containing_func {
        return crate::semantic::func_body_assumptions(db, func).extend_all_bounds(db);
    }

    let mut enclosing = body.scope();
    let mut parent_item = enclosing.parent_item(db);
    while let Some(ItemKind::Body(parent)) = parent_item {
        enclosing = parent.scope();
        parent_item = enclosing.parent_item(db);
    }

    match parent_item {
        Some(ItemKind::Trait(trait_)) => {
            PredicateListId::new(db, vec![crate::semantic::trait_self_predicate(db, trait_)])
                .extend_all_bounds(db)
        }
        Some(ItemKind::ImplTrait(impl_trait)) => collect_constraints(db, impl_trait.into())
            .instantiate_identity()
            .extend_all_bounds(db),
        Some(ItemKind::Impl(impl_)) => collect_constraints(db, impl_.into())
            .instantiate_identity()
            .extend_all_bounds(db),
        _ => PredicateListId::empty_list(db),
    }
}

fn evaluate_const_ty_cycle_initial<'db>(
    _db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    _expected_ty: Option<TyId<'db>>,
) -> ConstTyId<'db> {
    const_ty
}

fn evaluate_const_ty_cycle_recover<'db>(
    _db: &'db dyn HirAnalysisDb,
    _value: &ConstTyId<'db>,
    _count: u32,
    _const_ty: ConstTyId<'db>,
    _expected_ty: Option<TyId<'db>>,
) -> salsa::CycleRecoveryAction<ConstTyId<'db>> {
    salsa::CycleRecoveryAction::Iterate
}

pub(crate) fn const_ty_from_assoc_const_use<'db>(
    db: &'db dyn HirAnalysisDb,
    assoc: AssocConstUse<'db>,
) -> Option<ConstTyId<'db>> {
    const_ty_from_trait_const(db, assoc.solve_cx(db), assoc.inst(), assoc.name())
}

/// Whether `start_body`'s value definition can re-enter `start_body` when
/// its const references (module consts and associated consts through their
/// selected impls or defaults) are resolved transitively.
///
/// This detects recursive associated-const definitions on *generic* impls at
/// the definition site: evaluation under the impl's own binder never errors
/// (param-dependent consts legitimately stay symbolic, and the recursive
/// fixpoint in `evaluate_const_ty` recovers with the unevaluated form), so
/// recursion is invisible to the evaluation result. The typed body's
/// registered const refs give the same resolution edges lowering will take;
/// refs whose resolution depends on unknown params (no impl selected) end
/// the walk — those are deferred to instantiation sites.
pub(crate) fn const_body_resolution_reenters<'db>(
    db: &'db dyn HirAnalysisDb,
    start_body: Body<'db>,
    start_expected: TyId<'db>,
    start_args: &[TyId<'db>],
) -> bool {
    use crate::analysis::ty::ty_check::ConstRef;

    let mut visited = FxHashSet::default();
    visited.insert(start_body);
    let mut frontier = vec![(start_body, start_expected, start_args.to_vec())];
    while let Some((body, expected, args)) = frontier.pop() {
        let typed_body = &check_anon_const_body(db, body, expected).1;
        for cref in typed_body.const_refs() {
            let next = match cref {
                ConstRef::Const(const_) => const_
                    .body(db)
                    .to_opt()
                    .map(|next_body| (next_body, const_.ty(db), Vec::new())),
                ConstRef::TraitConst(assoc) => {
                    // The recorded use is in the owning body's binder terms:
                    // `Self::B` in a trait default keeps `Self` as a param.
                    // Instantiate with the args this body is evaluated under
                    // so impl overrides resolve (a default-body cycle only
                    // closes through the concrete impl's override).
                    let assoc = if args.is_empty() {
                        assoc
                    } else {
                        assoc.with_inst(Binder::bind(assoc.inst()).instantiate(db, &args))
                    };
                    const_ty_from_assoc_const_use(db, assoc).and_then(|const_ty| {
                        match const_ty.data(db) {
                            ConstTyData::UnEvaluated {
                                body,
                                ty: Some(ty),
                                generic_args,
                                ..
                            } => Some((*body, *ty, generic_args.clone())),
                            _ => None,
                        }
                    })
                }
                ConstRef::InherentConst(use_) => {
                    let use_ = if args.is_empty() {
                        use_
                    } else {
                        InherentConstUse::new(
                            use_.origin_scope(),
                            use_.assumptions(),
                            use_.impl_(),
                            Binder::bind(use_.receiver_ty()).instantiate(db, &args),
                            use_.name(),
                        )
                    };
                    const_ty_from_inherent_const_use(db, use_).and_then(|const_ty| {
                        match const_ty.data(db) {
                            ConstTyData::UnEvaluated {
                                body,
                                ty: Some(ty),
                                generic_args,
                                ..
                            } => Some((*body, *ty, generic_args.clone())),
                            _ => None,
                        }
                    })
                }
            };
            let Some((next_body, next_expected, next_args)) = next else {
                continue;
            };
            if next_body == start_body {
                return true;
            }
            if next_expected.has_invalid(db) || !visited.insert(next_body) {
                continue;
            }
            frontier.push((next_body, next_expected, next_args));
        }
    }
    false
}

/// Builds the abstract (unevaluated) form of a trait-const use. Evaluation is
/// deferred to the use position so the const can be retyped to the position's
/// expected type (e.g. an integer trait const used as an array length), while
/// the carried `AssocConstUse` keeps the trait goal visible to
/// well-formedness checking.
pub(crate) fn abstract_const_ty_from_assoc_const_use<'db>(
    db: &'db dyn HirAnalysisDb,
    assoc: AssocConstUse<'db>,
    expected_ty: TyId<'db>,
) -> ConstTyId<'db> {
    ConstTyId::new(
        db,
        ConstTyData::Abstract(
            ConstExprId::new(db, ConstExpr::TraitConst(assoc)),
            expected_ty,
        ),
    )
}

/// Evaluates `evaluated` (the result of resolving an associated-const use) and
/// returns it, falling back to an `Abstract` const built from `abstract_expr`
/// when the use can't be evaluated yet (generic receiver). Invalid selected
/// results retain their diagnostic cause. Shared by the trait-const and
/// inherent-const entry points so the two paths can't drift.
fn selected_const_requires_original_use<'db>(
    db: &'db dyn HirAnalysisDb,
    evaluated: ConstTyId<'db>,
) -> bool {
    match evaluated.data(db) {
        ConstTyData::UnEvaluated { .. } => true,
        ConstTyData::Computation { description, .. } => match description.repr() {
            ConstRepr::Term(term) => selected_const_requires_original_use(db, *term),
            ConstRepr::Value(_) => false,
            ConstRepr::Deferred(_) => true,
        },
        ConstTyData::Abstract(expr, _) => !matches!(
            expr.data(db),
            ConstExpr::TraitConst(_) | ConstExpr::InherentConst(_)
        ),
        _ => false,
    }
}

fn const_ty_or_abstract<'db>(
    db: &'db dyn HirAnalysisDb,
    abstract_expr: ConstExpr<'db>,
    evaluated: Option<ConstTyId<'db>>,
    expected_ty: TyId<'db>,
) -> ConstTyId<'db> {
    let to_abstract = |expr: ConstExpr<'db>| {
        ConstTyId::new(
            db,
            ConstTyData::Abstract(ConstExprId::new(db, expr), expected_ty),
        )
    };
    let Some(evaluated) = evaluated else {
        return to_abstract(abstract_expr);
    };
    let evaluated = evaluated.evaluate(db, Some(expected_ty));
    if evaluated.ty(db).has_invalid(db) {
        return evaluated;
    }
    if selected_const_requires_original_use(db, evaluated) {
        return to_abstract(abstract_expr);
    }
    evaluated
}

pub(crate) fn const_ty_or_abstract_from_assoc_const_use<'db>(
    db: &'db dyn HirAnalysisDb,
    assoc: AssocConstUse<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstTyId<'db>> {
    Some(const_ty_or_abstract(
        db,
        ConstExpr::TraitConst(assoc),
        const_ty_from_assoc_const_use(db, assoc),
        expected_ty,
    ))
}

pub(super) fn const_ty_from_trait_const<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    name: IdentId<'db>,
) -> Option<ConstTyId<'db>> {
    let Selection::Unique(resolved) = resolve_trait_impl_instance(db, solve_cx, inst) else {
        return None;
    };
    const_ty_from_resolved_trait_const(db, resolved, name)
}

pub(super) fn const_ty_from_resolved_trait_const<'db>(
    db: &'db dyn HirAnalysisDb,
    resolved: ResolvedImplInstance<'db>,
    name: IdentId<'db>,
) -> Option<ConstTyId<'db>> {
    let inst = resolved.trait_inst();
    let trait_ = inst.def(db);
    let (body, _, generic_args) = selected_assoc_const_body_template(db, resolved, name)?;

    let declared_ty = trait_
        .const_(db, name)
        .and_then(|v| v.ty_binder(db))
        .map(|b| b.instantiate(db, inst.args(db)));

    Some(ConstTyId::from_body_with_generic_args(
        db,
        body,
        declared_ty,
        None,
        generic_args,
    ))
}

/// Recovers the instantiated generic arguments of an inherent `impl` for the
/// given receiver type by unifying the impl self type with `receiver_ty` in
/// `table`. The impl params and self type are instantiated under a single
/// binder so the fresh inference vars are shared.
///
/// This is the single home for the receiver-unification logic shared by
/// expression/pattern type checking and CTFE.
pub(crate) fn unify_inherent_impl_receiver<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    impl_: crate::hir_def::Impl<'db>,
    receiver_ty: TyId<'db>,
) -> Option<Vec<TyId<'db>>> {
    let impl_ty = impl_.admissible_inherent_impl_ty(db)?;
    let mut bound: Vec<TyId<'db>> = collect_generic_params(db, impl_.into()).params(db).to_vec();
    bound.push(impl_ty);

    let mut inst = table.instantiate_with_fresh_vars(super::binder::Binder::bind(bound));
    let inst_ty = inst.pop().unwrap();
    table.unify(inst_ty, receiver_ty).ok()?;
    Some(inst.iter().map(|&ty| ty.fold_with(db, table)).collect())
}

/// The declared type of an inherent const, instantiated for `receiver_ty`
/// within the given unification table (so inference vars stay connected to
/// the caller's inference context).
pub(crate) fn instantiate_inherent_const_decl_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    table: &mut UnificationTable<'db>,
    impl_: crate::hir_def::Impl<'db>,
    receiver_ty: TyId<'db>,
    name: IdentId<'db>,
) -> Option<TyId<'db>> {
    let impl_args = unify_inherent_impl_receiver(db, table, impl_, receiver_ty)?;
    let decl_ty = inherent_const_decl_ty(db, impl_, name)?;
    let decl_ty = super::binder::Binder::bind(decl_ty).instantiate(db, &impl_args);
    Some(table.instantiate_to_term(decl_ty))
}

/// Looks up the HIR body for an associated const defined in an inherent
/// `impl` block, returning both the body and the impl's instantiated generic
/// arguments (recovered by unifying the impl self type with `receiver_ty`).
pub(crate) fn inherent_const_body_and_impl_args<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: crate::hir_def::Impl<'db>,
    receiver_ty: TyId<'db>,
    name: IdentId<'db>,
) -> Option<(Body<'db>, Vec<TyId<'db>>)> {
    let body = impl_.const_(db, name)?.value_body(db)?;

    let mut table = UnificationTable::new(db);
    let impl_args = unify_inherent_impl_receiver(db, &mut table, impl_, receiver_ty)?;
    Some((body, impl_args))
}

/// The declared type of an inherent-impl associated const, still referencing
/// the impl's own generic parameters. Delegates to the const view so the
/// declared-type lowering lives in one place.
pub(crate) fn inherent_const_decl_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: crate::hir_def::Impl<'db>,
    name: IdentId<'db>,
) -> Option<TyId<'db>> {
    impl_.const_(db, name)?.ty(db)
}

/// The declared type of an inherent const, instantiated for the given
/// receiver type.
pub(crate) fn inherent_const_expected_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: crate::hir_def::Impl<'db>,
    receiver_ty: TyId<'db>,
    name: IdentId<'db>,
) -> Option<TyId<'db>> {
    let (_, impl_args) = inherent_const_body_and_impl_args(db, impl_, receiver_ty, name)?;
    inherent_const_decl_ty(db, impl_, name)
        .map(|ty| super::binder::Binder::bind(ty).instantiate(db, &impl_args))
}

pub(crate) fn const_ty_from_inherent_const<'db>(
    db: &'db dyn HirAnalysisDb,
    impl_: crate::hir_def::Impl<'db>,
    receiver_ty: TyId<'db>,
    name: IdentId<'db>,
) -> Option<ConstTyId<'db>> {
    let (body, impl_args) = inherent_const_body_and_impl_args(db, impl_, receiver_ty, name)?;
    let declared_ty = inherent_const_decl_ty(db, impl_, name)
        .map(|ty| super::binder::Binder::bind(ty).instantiate(db, &impl_args));
    Some(ConstTyId::from_body_with_generic_args(
        db,
        body,
        declared_ty,
        None,
        impl_args,
    ))
}

pub(crate) fn const_ty_from_inherent_const_use<'db>(
    db: &'db dyn HirAnalysisDb,
    use_: super::assoc_const::InherentConstUse<'db>,
) -> Option<ConstTyId<'db>> {
    const_ty_from_inherent_const(db, use_.impl_(), use_.receiver_ty(), use_.name())
}

/// Like [`const_ty_or_abstract_from_assoc_const_use`], but for inherent-impl
/// consts: falls back to an abstract const when the receiver isn't concrete
/// enough to evaluate yet (e.g. inside a generic impl).
pub(crate) fn const_ty_or_abstract_from_inherent_const_use<'db>(
    db: &'db dyn HirAnalysisDb,
    use_: super::assoc_const::InherentConstUse<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstTyId<'db>> {
    Some(const_ty_or_abstract(
        db,
        ConstExpr::InherentConst(use_),
        const_ty_from_inherent_const_use(db, use_),
        expected_ty,
    ))
}

// FIXME: When we add type inference, we need to use the inference engine to
// check the type of the expression instead of this function.
pub(crate) fn check_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty_ty: TyId<'db>,
    expected_ty: Option<TyId<'db>>,
    table: &mut UnificationTable<'db>,
) -> Result<TyId<'db>, InvalidCause<'db>> {
    if let Some(cause) = const_ty_ty.invalid_cause(db) {
        return Err(cause);
    }

    if const_ty_ty.has_invalid(db) {
        return Err(InvalidCause::Other);
    }

    let Some(expected_ty) = expected_ty else {
        return Ok(const_ty_ty);
    };

    if table.unify(expected_ty, const_ty_ty).is_ok() {
        Ok(expected_ty)
    } else {
        let invalid = InvalidCause::ConstTyMismatch {
            expected: expected_ty,
            given: const_ty_ty,
        };
        Err(invalid)
    }
}

impl<'db> ConstTyId<'db> {
    /// Construct a type-level integer from a trusted literal payload.
    pub fn integer(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, value: BigInt) -> Self {
        const_ty_from_sem_const(db, int_const(db, ty, value))
    }

    pub fn integer_value(self, db: &'db dyn HirAnalysisDb) -> Option<BigInt> {
        let value = match self.data(db) {
            ConstTyData::Value(value) => value.value(),
            ConstTyData::Description(value) => *value,
            _ => return None,
        };
        match value.value(db) {
            SemConstValue::Scalar {
                value: SemConstScalar::Int { value },
                ..
            } => Some(value),
            _ => None,
        }
    }

    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match self.data(db) {
            ConstTyData::TyVar(_, ty) => *ty,
            ConstTyData::TyParam(_, ty) => *ty,
            ConstTyData::Hole(ty, _) => *ty,
            ConstTyData::Value(value) => crate::analysis::semantic::sem_const_ty(db, value.value()),
            ConstTyData::Description(value) => crate::analysis::semantic::sem_const_ty(db, *value),
            ConstTyData::Invalid(ty) => *ty,
            ConstTyData::Computation { description, .. } => description.ty(),
            ConstTyData::Abstract(_, ty) => *ty,
            ConstTyData::UnEvaluated { ty, .. } => {
                ty.unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
            }
        }
    }

    pub fn pretty_print_with_mode(self, db: &'db dyn HirAnalysisDb, mode: TypePrintMode) -> String {
        if matches!(mode, TypePrintMode::Concrete)
            && let Some(env) = const_canon_env(db, self)
        {
            let concretized =
                canonicalize_const_ty_for_mode(db, self, env, ConstCanonMode::Display);
            if concretized != self {
                return concretized.pretty_print_with_mode(db, TypePrintMode::Symbolic);
            }
        }
        self.pretty_print_symbolic(db)
    }

    pub fn pretty_print_concrete(self, db: &'db dyn HirAnalysisDb) -> String {
        self.pretty_print_with_mode(db, TypePrintMode::Concrete)
    }

    fn pretty_print_symbolic(self, db: &'db dyn HirAnalysisDb) -> String {
        match &self.data(db) {
            ConstTyData::TyVar(var, _) => var.pretty_print(),
            ConstTyData::TyParam(param, ty) => {
                format!("const {}: {}", param.pretty_print(db), ty.pretty_print(db))
            }
            ConstTyData::Hole(..) => "_".to_string(),
            ConstTyData::Value(value) => value.value().pretty_print(db),
            ConstTyData::Description(value) => value.pretty_print(db),
            ConstTyData::Invalid(_) => "<invalid>".to_string(),
            ConstTyData::Computation { description, .. } => match description.repr() {
                ConstRepr::Value(value) => value.value().pretty_print(db),
                ConstRepr::Term(term) => term.pretty_print_symbolic(db),
                ConstRepr::Deferred(_) => "<deferred>".to_string(),
            },
            ConstTyData::Abstract(expr, _) => expr.pretty_print(db),
            ConstTyData::UnEvaluated {
                body,
                ty,
                const_def,
                generic_args,
                ..
            } => {
                if let Some(const_def) = const_def
                    && let Some(name) = const_def.name(db).to_opt()
                {
                    return format!("const {}", name.data(db));
                }

                let expr = body.expr(db);
                if let Some(rendered) = pretty_print_const_body_expr(
                    db,
                    *body,
                    expr,
                    &generic_const_param_display_map(db, *body, generic_args),
                ) {
                    return rendered;
                }

                let fallback = self.evaluate(db, *ty);
                if fallback != self {
                    return fallback.pretty_print(db);
                }

                "const value".into()
            }
        }
    }

    pub(super) fn pretty_print(self, db: &'db dyn HirAnalysisDb) -> String {
        self.pretty_print_with_mode(db, TypePrintMode::Concrete)
    }

    pub fn evaluate(self, db: &'db dyn HirAnalysisDb, expected_ty: Option<TyId<'db>>) -> Self {
        evaluate_const_ty(db, self, expected_ty)
    }

    pub(super) fn from_body(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        ty: Option<TyId<'db>>,
        const_def: Option<Const<'db>>,
    ) -> Self {
        Self::from_body_with_generic_args(db, body, ty, const_def, Vec::new())
    }

    pub(super) fn from_body_with_generic_args(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        ty: Option<TyId<'db>>,
        const_def: Option<Const<'db>>,
        generic_args: Vec<TyId<'db>>,
    ) -> Self {
        Self::from_body_with_generic_args_and_options(
            db,
            body,
            ty,
            const_def,
            generic_args,
            false,
            false,
        )
    }

    pub(super) fn from_body_with_generic_args_and_preservation(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        ty: Option<TyId<'db>>,
        const_def: Option<Const<'db>>,
        generic_args: Vec<TyId<'db>>,
        preserve_unevaluated: bool,
    ) -> Self {
        Self::from_body_with_generic_args_and_options(
            db,
            body,
            ty,
            const_def,
            generic_args,
            preserve_unevaluated,
            false,
        )
    }

    fn from_body_with_generic_args_and_options(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        ty: Option<TyId<'db>>,
        const_def: Option<Const<'db>>,
        generic_args: Vec<TyId<'db>>,
        preserve_unevaluated: bool,
        defer_validation: bool,
    ) -> Self {
        let data = ConstTyData::UnEvaluated {
            body,
            ty,
            const_def,
            generic_args,
            preserve_unevaluated,
            defer_validation,
        };
        Self::new(db, data)
    }

    pub fn from_opt_body(db: &'db dyn HirAnalysisDb, body: Partial<Body<'db>>) -> Self {
        match body {
            Partial::Present(body) => Self::from_body(db, body, None, None),
            Partial::Absent => Self::invalid(db, InvalidCause::ParseError),
        }
    }

    pub(super) fn from_opt_body_with_ty_and_generic_args(
        db: &'db dyn HirAnalysisDb,
        body: Partial<Body<'db>>,
        ty: Option<TyId<'db>>,
        generic_args: Vec<TyId<'db>>,
        preserve_unevaluated: bool,
    ) -> Self {
        match body {
            Partial::Present(body) => Self::from_body_with_generic_args_and_preservation(
                db,
                body,
                ty,
                None,
                generic_args,
                preserve_unevaluated,
            ),
            Partial::Absent => Self::invalid(db, InvalidCause::ParseError),
        }
    }

    pub(super) fn from_opt_body_deferred(
        db: &'db dyn HirAnalysisDb,
        body: Partial<Body<'db>>,
        ty: Option<TyId<'db>>,
        generic_args: Vec<TyId<'db>>,
    ) -> Self {
        match body {
            Partial::Present(body) => Self::from_body_with_generic_args_and_options(
                db,
                body,
                ty,
                None,
                generic_args,
                true,
                true,
            ),
            Partial::Absent => Self::invalid(db, InvalidCause::ParseError),
        }
    }

    pub(super) fn with_ty(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        self.swap_ty(db, ty)
    }

    pub(super) fn invalid(db: &'db dyn HirAnalysisDb, cause: InvalidCause<'db>) -> Self {
        let ty = TyId::invalid(db, cause);
        let data = ConstTyData::Invalid(ty);
        Self::new(db, data)
    }

    pub fn hole(db: &'db dyn HirAnalysisDb) -> Self {
        Self::hole_with_ty(db, TyId::invalid(db, InvalidCause::Other))
    }

    pub fn hole_with_ty(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        Self::hole_with_id(db, ty, HoleId::bound_opaque())
    }

    pub fn hole_with_id(db: &'db dyn HirAnalysisDb, ty: TyId<'db>, hole_id: HoleId<'db>) -> Self {
        Self::new(db, ConstTyData::Hole(ty, hole_id))
    }

    pub fn structural_hole(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        origin: StructuralHoleOrigin<'db>,
        introduced_at: LayoutIntroSite<'db>,
        root: LayoutRootId<'db>,
    ) -> Self {
        Self::hole_with_id(
            db,
            ty,
            HoleId::structural(db, ty, origin, introduced_at, root),
        )
    }

    pub fn bound_callable_hole(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        owner: CallableLayoutOwner<'db>,
        origin: CallableInputLayoutHoleOrigin,
        ordinal: usize,
    ) -> Self {
        Self::hole_with_id(db, ty, HoleId::bound_callable(owner, origin, ordinal))
    }

    pub(crate) fn swap_ty(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        let data = match self.data(db) {
            ConstTyData::TyVar(var, _) => ConstTyData::TyVar(var.clone(), ty),
            ConstTyData::TyParam(param, _) => ConstTyData::TyParam(param.clone(), ty),
            ConstTyData::Hole(_, hole_id) => ConstTyData::Hole(
                ty,
                match hole_id {
                    HoleId::Structural(hole_id) => {
                        HoleId::Structural(StructuralHoleId::with_intro(
                            db,
                            ty,
                            hole_id.root(db),
                            hole_id.origin(db),
                            hole_id.introduced_at(db).clone(),
                        ))
                    }
                    HoleId::Bound(hole_id) => HoleId::Bound(*hole_id),
                },
            ),
            ConstTyData::Value(value) => {
                if ty.invalid_cause(db).is_some() {
                    return Self::new(db, ConstTyData::Invalid(ty));
                }
                match crate::analysis::semantic::retype_verified_sem_const(db, value.value(), ty)
                    .and_then(|value| VerifiedConstValueId::from_complete_execution(db, value).ok())
                {
                    Some(value) => ConstTyData::Value(value),
                    None => ConstTyData::Invalid(TyId::invalid(
                        db,
                        InvalidCause::ConstTyMismatch {
                            expected: ty,
                            given: self.ty(db),
                        },
                    )),
                }
            }
            ConstTyData::Description(value) => {
                if ty.invalid_cause(db).is_some() {
                    return Self::new(db, ConstTyData::Invalid(ty));
                }
                return crate::analysis::semantic::consts::retype_sem_const_description(
                    db, *value, ty,
                )
                .map(|value| const_ty_from_sem_const(db, value))
                .unwrap_or_else(|| {
                    Self::invalid(
                        db,
                        InvalidCause::ConstTyMismatch {
                            expected: ty,
                            given: self.ty(db),
                        },
                    )
                });
            }
            ConstTyData::Computation {
                description,
                source,
            } => {
                if ty.invalid_cause(db).is_some() {
                    return Self::new(db, ConstTyData::Invalid(ty));
                }
                let Some(description) = description.as_ref().clone().with_ty(db, ty) else {
                    return Self::invalid(
                        db,
                        InvalidCause::ConstTyMismatch {
                            expected: ty,
                            given: self.ty(db),
                        },
                    );
                };
                ConstTyData::Computation {
                    description: Box::new(description),
                    source: *source,
                }
            }
            ConstTyData::Invalid(_) => ConstTyData::Invalid(ty),
            ConstTyData::Abstract(expr, _) => ConstTyData::Abstract(*expr, ty),
            ConstTyData::UnEvaluated {
                body,
                const_def,
                generic_args,
                preserve_unevaluated,
                defer_validation,
                ..
            } => ConstTyData::UnEvaluated {
                body: *body,
                ty: Some(ty),
                const_def: *const_def,
                generic_args: generic_args.clone(),
                preserve_unevaluated: *preserve_unevaluated,
                defer_validation: *defer_validation,
            },
        };

        Self::new(db, data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstTyData<'db> {
    TyVar(TyVar<'db>, TyId<'db>),
    TyParam(TyParam<'db>, TyId<'db>),
    Hole(TyId<'db>, HoleId<'db>),
    Value(VerifiedConstValueId<'db>),
    Description(SemConstId<'db>),
    Invalid(TyId<'db>),
    Abstract(ConstExprId<'db>, TyId<'db>),
    /// A stored source occurrence retains the common description and diagnostic
    /// context. Identity comparison projects its canonical term without making
    /// the original occurrence lose its required evaluation.
    Computation {
        description: Box<ConstDesc<'db>>,
        source: SemanticInstanceKey<'db>,
    },
    UnEvaluated {
        body: Body<'db>,
        ty: Option<TyId<'db>>,
        const_def: Option<Const<'db>>,
        generic_args: Vec<TyId<'db>>,
        preserve_unevaluated: bool,
        defer_validation: bool,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analysis::semantic::{bool_const, int_const, sem_const_from_ty, tuple_const},
        analysis::ty::ty_def::PrimTy,
        test_db::HirAnalysisTestDb,
    };

    struct ReplaceConst<'db> {
        from: TyId<'db>,
        to: TyId<'db>,
    }

    impl<'db> TyFolder<'db> for ReplaceConst<'db> {
        fn fold_ty(&mut self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> TyId<'db> {
            if ty == self.from {
                self.to
            } else {
                ty.super_fold_with(db, self)
            }
        }
    }

    #[test]
    fn signed_constant_type_round_trip_preserves_value() {
        let db = HirAnalysisTestDb::default();
        let ty = TyId::new(&db, TyData::TyBase(TyBase::Prim(PrimTy::I8)));
        let value = int_const(&db, ty, BigInt::from(-1));
        let frozen = const_ty_from_sem_const(&db, value);
        let thawed = sem_const_from_ty(&db, TyId::const_ty(&db, frozen));
        assert_eq!(thawed, Some(value));
    }

    #[test]
    fn dependent_constant_type_round_trip_preserves_term_and_nested_description() {
        let db = HirAnalysisTestDb::default();
        let ty = TyId::u8(&db);
        let hole = ConstTyId::new(
            &db,
            ConstTyData::Hole(ty, HoleId::Bound(BoundHoleId::Opaque)),
        );
        let dependent = sem_const_from_ty(&db, TyId::const_ty(&db, hole)).unwrap();
        assert_eq!(const_ty_from_sem_const(&db, dependent), hole);

        let tuple_ty = TyId::tuple_with_elems(&db, &[ty, ty]);
        let description = tuple_const(
            &db,
            tuple_ty,
            vec![int_const(&db, ty, BigInt::from(7)), dependent].into_boxed_slice(),
        );
        let stored = const_ty_from_sem_const(&db, description);
        assert!(
            matches!(stored.data(&db), ConstTyData::Description(value) if *value == description)
        );
        assert_eq!(
            sem_const_from_ty(&db, TyId::const_ty(&db, stored)),
            Some(description)
        );
        assert_eq!(stored.swap_ty(&db, tuple_ty), stored);
        let array_ty = TyId::array_with_len(&db, ty, 2);
        assert!(matches!(
            stored.swap_ty(&db, array_ty).data(&db),
            ConstTyData::Invalid(..)
        ));

        let malformed = tuple_const(
            &db,
            tuple_ty,
            vec![bool_const(&db, true), dependent].into_boxed_slice(),
        );
        assert!(matches!(
            const_ty_from_sem_const(&db, malformed).data(&db),
            ConstTyData::Invalid(..)
        ));
    }

    #[test]
    fn specializing_nested_description_produces_verified_value() {
        let db = HirAnalysisTestDb::default();
        let ty = TyId::u8(&db);
        let hole = ConstTyId::hole_with_ty(&db, ty);
        let hole_ty = TyId::const_ty(&db, hole);
        let dependent = sem_const_from_ty(&db, hole_ty).unwrap();
        let first = int_const(&db, ty, BigInt::from(7));
        let tuple_ty = TyId::tuple_with_elems(&db, &[ty, ty]);
        let symbolic = const_ty_from_sem_const(
            &db,
            tuple_const(&db, tuple_ty, vec![first, dependent].into_boxed_slice()),
        );
        let replacement = ConstTyId::integer(&db, ty, BigInt::from(9));
        let folded = TyId::const_ty(&db, symbolic).fold_with(
            &db,
            &mut ReplaceConst {
                from: hole_ty,
                to: TyId::const_ty(&db, replacement),
            },
        );
        let TyData::ConstTy(folded) = folded.data(&db) else {
            panic!("specialization must preserve a constant type");
        };
        assert!(matches!(folded.data(&db), ConstTyData::Value(..)));
        assert_eq!(
            sem_const_from_ty(&db, TyId::const_ty(&db, *folded)),
            Some(tuple_const(
                &db,
                tuple_ty,
                vec![first, int_const(&db, ty, BigInt::from(9))].into_boxed_slice(),
            ))
        );
    }
}
