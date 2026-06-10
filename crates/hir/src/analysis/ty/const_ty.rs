use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, ToPrimitive, Zero};

use crate::core::hir_def::{
    BinOp, Body, Const, EnumVariant, Expr, ExprId, Func, GenericArgListId, GenericParamOwner,
    IdentId, IntegerId, LitKind, Partial, PathId, Stmt, TypeAlias as HirTypeAlias,
    TypeId as HirTypeId,
};
use salsa::Update;

use super::const_expr::{ConstExpr, ConstExprId, pretty_print_un_op};
use super::{
    assoc_const::AssocConstUse,
    diagnostics::{BodyDiag, FuncBodyDiag},
    fold::{AssocTySubst, TyFoldable},
    normalize::normalize_ty,
    trait_def::TraitInstId,
    trait_resolution::{
        GoalSatisfiability, TraitSolveCx, constraint::collect_constraints, is_goal_satisfiable,
    },
    ty_check::{check_anon_const_body, check_const_body},
    ty_def::{InvalidCause, TyId, TyParam, TyVar},
    ty_lower::{ConstDefaultCompletion, collect_generic_params},
    unify::UnificationTable,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    semantic::{
        CtfeError, SemConstId, SemConstValue, SemOrigin, VariantIndex, eval_body_owner_const,
        eval_body_owner_const_with_args, int_ty_shape, normalize_int_to_shape, sem_const_from_ty,
    },
    ty::trait_resolution::PredicateListId,
    ty::ty_def::{Kind, PrimTy, TyBase, TyData, TyVarSort},
};
use crate::hir_def::{CallableDef, ItemKind, scope_graph::ScopeId};
use common::indexmap::IndexMap;
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LayoutHoleArgSite<'db> {
    Path(PathId<'db>),
    GenericArgList(GenericArgListId<'db>),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum CallableInputLayoutHoleOrigin {
    Receiver,
    ValueParam(usize),
    Effect(usize),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HoleId<'db> {
    Structural(StructuralHoleId<'db>),
    Bound(BoundHoleId<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BoundHoleId<'db> {
    Opaque,
    CallableInput {
        func: Func<'db>,
        origin: CallableInputLayoutHoleOrigin,
        ordinal: usize,
    },
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

/// A site a hole's containing type was lowered at lexically (syntactic
/// nesting and template membership).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum LexSite<'db> {
    HirType(HirTypeId<'db>),
    TypeComponent { ty: HirTypeId<'db>, slot: usize },
    RootPath(PathId<'db>),
    GenericArgList(GenericArgListId<'db>),
    AliasTemplate(HirTypeAlias<'db>),
}

/// A context a hole's containing type was instantiated/applied through
/// (alias uses, generic-arg positions, callable inputs).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum InstSite<'db> {
    CallableInput {
        func: Func<'db>,
        origin: CallableInputLayoutHoleOrigin,
    },
    TypeComponent {
        ty: HirTypeId<'db>,
        slot: usize,
    },
    RootPath(PathId<'db>),
    GenericArgList(GenericArgListId<'db>),
}

/// One link of a hole's provenance chain. The `Lex`/`Inst` tag keeps the
/// merged chain injective with respect to the lexical and instantiation
/// histories it encodes: sites that exist in both kinds (`TypeComponent`,
/// `RootPath`, `GenericArgList`) stay distinguishable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Update)]
pub enum ProvenanceSite<'db> {
    Lex(LexSite<'db>),
    Inst(InstSite<'db>),
}

/// Interned cons-list of provenance sites, newest site at the head.
///
/// HIR ids (`TypeId`, `PathId`, `GenericArgListId`) are content-interned, so
/// a hole's creation site alone does not identify a syntactic occurrence:
/// `(Slot<_>, Slot<_>)` shares one child HIR type and `(M, M)` one alias
/// path. The provenance chain accumulates the lowering/instantiation
/// contexts a hole's containing type passed through, giving each occurrence
/// a distinct identity. Two holes silently merging means aliased storage
/// slots, so pushes err on the side of distinctness.
#[salsa::interned]
#[derive(Debug)]
pub struct ProvenanceId<'db> {
    pub parent: Option<ProvenanceId<'db>>,
    pub site: ProvenanceSite<'db>,
}

impl<'db> ProvenanceId<'db> {
    pub(crate) fn root(db: &'db dyn HirAnalysisDb, site: ProvenanceSite<'db>) -> Self {
        Self::new(db, None, site)
    }

    pub(crate) fn push(self, db: &'db dyn HirAnalysisDb, site: ProvenanceSite<'db>) -> Self {
        Self::new(db, Some(self), site)
    }

    pub(crate) fn contains(
        self,
        db: &'db dyn HirAnalysisDb,
        mut pred: impl FnMut(ProvenanceSite<'db>) -> bool,
    ) -> bool {
        let mut current = Some(self);
        while let Some(link) = current {
            if pred(link.site(db)) {
                return true;
            }
            current = link.parent(db);
        }
        false
    }
}

#[salsa::interned]
#[derive(Debug)]
pub struct StructuralHoleId<'db> {
    pub expected_ty: TyId<'db>,
    pub origin: StructuralHoleOrigin<'db>,
    pub provenance: ProvenanceId<'db>,
}

impl<'db> StructuralHoleId<'db> {
    pub(crate) fn push_provenance(
        self,
        db: &'db dyn HirAnalysisDb,
        site: ProvenanceSite<'db>,
    ) -> Self {
        Self::new(
            db,
            self.expected_ty(db),
            self.origin(db),
            self.provenance(db).push(db, site),
        )
    }
}

impl<'db> HoleId<'db> {
    pub(crate) fn bound_callable(
        func: Func<'db>,
        origin: CallableInputLayoutHoleOrigin,
        ordinal: usize,
    ) -> Self {
        Self::Bound(BoundHoleId::CallableInput {
            func,
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
        provenance: ProvenanceId<'db>,
    ) -> Self {
        Self::Structural(StructuralHoleId::new(db, expected_ty, origin, provenance))
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

pub(crate) fn evaluate_abstract_int_const_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    expr: ConstExprId<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstTyId<'db>> {
    evaluate_int_const_expr_impl(db, expr, expected_ty, false)
}

pub fn evaluate_type_level_int_const_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    expr: ConstExprId<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstTyId<'db>> {
    evaluate_int_const_expr_impl(db, expr, expected_ty, true)
}

fn evaluate_int_const_expr_impl<'db>(
    db: &'db dyn HirAnalysisDb,
    expr: ConstExprId<'db>,
    expected_ty: TyId<'db>,
    allow_numeric_calls: bool,
) -> Option<ConstTyId<'db>> {
    fn numeric_call_kind(name: &str) -> Option<&str> {
        match name {
            "add" | "sub" | "mul" | "div" | "rem" | "pow" | "shl" | "shr" | "bitand" | "bitor"
            | "bitxor" => Some(name),
            _ => {
                let op = name
                    .strip_prefix("__checked_")
                    .or_else(|| name.strip_prefix("__"))?;
                [
                    "_u8", "_u16", "_u32", "_u64", "_u128", "_u256", "_usize", "_i8", "_i16",
                    "_i32", "_i64", "_i128", "_i256", "_isize", "_bool",
                ]
                .iter()
                .find_map(|suffix| op.strip_suffix(suffix))
            }
        }
    }

    fn eval_int_value<'db>(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        expected_ty: TyId<'db>,
        allow_numeric_calls: bool,
    ) -> Option<BigInt> {
        let TyData::ConstTy(const_ty) = ty.data(db) else {
            return None;
        };
        match const_ty.data(db) {
            ConstTyData::Evaluated(EvaluatedConstTy::LitInt(int_id), _) => {
                let (bits, signed) = int_ty_shape(db, expected_ty)?;
                let raw = BigInt::from_bytes_be(Sign::Plus, &int_id.data(db).to_bytes_be());
                Some(normalize_int_to_shape(raw, bits, signed))
            }
            ConstTyData::Abstract(expr, ty) => eval_expr(db, *expr, *ty, allow_numeric_calls),
            _ => None,
        }
    }

    fn eval_expr<'db>(
        db: &'db dyn HirAnalysisDb,
        expr: ConstExprId<'db>,
        expected_ty: TyId<'db>,
        allow_numeric_calls: bool,
    ) -> Option<BigInt> {
        let (bits, signed) = int_ty_shape(db, expected_ty)?;
        let normalize = |value| normalize_int_to_shape(value, bits, signed);
        match expr.data(db) {
            ConstExpr::ArithBinOp { op, lhs, rhs } => {
                let lhs = eval_int_value(db, *lhs, expected_ty, allow_numeric_calls)?;
                let rhs = eval_int_value(db, *rhs, expected_ty, allow_numeric_calls)?;
                Some(match op {
                    crate::hir_def::ArithBinOp::Add => normalize(lhs + rhs),
                    crate::hir_def::ArithBinOp::Sub => normalize(lhs - rhs),
                    crate::hir_def::ArithBinOp::Mul => normalize(lhs * rhs),
                    crate::hir_def::ArithBinOp::Div => {
                        if rhs.is_zero() {
                            return None;
                        }
                        normalize(lhs / rhs)
                    }
                    crate::hir_def::ArithBinOp::Rem => {
                        if rhs.is_zero() {
                            return None;
                        }
                        normalize(lhs % rhs)
                    }
                    crate::hir_def::ArithBinOp::Pow => {
                        if rhs.sign() == Sign::Minus {
                            return None;
                        }
                        let exp = rhs.to_u32()?;
                        normalize(lhs.pow(exp))
                    }
                    _ => return None,
                })
            }
            ConstExpr::UnOp { op, expr } => {
                let value = eval_int_value(db, *expr, expected_ty, allow_numeric_calls)?;
                Some(match op {
                    crate::hir_def::UnOp::Minus => normalize(-value),
                    crate::hir_def::UnOp::Plus => value,
                    _ => return None,
                })
            }
            ConstExpr::Cast { expr, to } => {
                let value = eval_int_value(db, *expr, expected_ty, allow_numeric_calls)?;
                let (bits, signed) = int_ty_shape(db, *to)?;
                Some(normalize_int_to_shape(value, bits, signed))
            }
            ConstExpr::ExternConstFnCall { func, args, .. }
            | ConstExpr::UserConstFnCall { func, args, .. }
                if allow_numeric_calls =>
            {
                let op = numeric_call_kind(func.name(db).to_opt()?.data(db))?;
                let args = args
                    .iter()
                    .map(|arg| eval_int_value(db, *arg, expected_ty, true))
                    .collect::<Option<Vec<_>>>()?;
                Some(match (op, args.as_slice()) {
                    ("add", [lhs, rhs]) => normalize(lhs.clone() + rhs),
                    ("sub", [lhs, rhs]) => normalize(lhs.clone() - rhs),
                    ("mul", [lhs, rhs]) => normalize(lhs.clone() * rhs),
                    ("div", [lhs, rhs]) => {
                        if rhs.is_zero() {
                            return None;
                        }
                        normalize(lhs.clone() / rhs)
                    }
                    ("rem", [lhs, rhs]) => {
                        if rhs.is_zero() {
                            return None;
                        }
                        normalize(lhs.clone() % rhs)
                    }
                    ("pow", [lhs, rhs]) => {
                        if rhs.sign() == Sign::Minus {
                            return None;
                        }
                        normalize(lhs.clone().pow(rhs.to_u32()?))
                    }
                    ("shl", [lhs, rhs]) => normalize(lhs.clone() << rhs.to_usize()?),
                    ("shr", [lhs, rhs]) => normalize(lhs.clone() >> rhs.to_usize()?),
                    ("bitand", [lhs, rhs]) => normalize(lhs & rhs),
                    ("bitor", [lhs, rhs]) => normalize(lhs | rhs),
                    ("bitxor", [lhs, rhs]) => normalize(lhs ^ rhs),
                    _ => return None,
                })
            }
            ConstExpr::ExternConstFnCall { .. } | ConstExpr::UserConstFnCall { .. } => None,
            _ => None,
        }
    }

    let value = eval_expr(db, expr, expected_ty, allow_numeric_calls)?;
    let (bits, _) = int_ty_shape(db, expected_ty)?;
    let encoded = normalize_int_to_shape(value, bits, false);
    let (_, bytes) = encoded.to_bytes_be();
    Some(ConstTyId::new(
        db,
        ConstTyData::Evaluated(
            EvaluatedConstTy::LitInt(IntegerId::new(db, BigUint::from_bytes_be(&bytes))),
            expected_ty,
        ),
    ))
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

fn evaluated_const_ty_is_fully_ground<'db>(
    db: &'db dyn HirAnalysisDb,
    value: &EvaluatedConstTy<'db>,
) -> bool {
    match value {
        EvaluatedConstTy::Tuple(elems)
        | EvaluatedConstTy::Array(elems)
        | EvaluatedConstTy::Record(elems) => elems
            .iter()
            .copied()
            .all(|elem| ty_is_fully_ground(db, elem)),
        EvaluatedConstTy::LitInt(..)
        | EvaluatedConstTy::LitBool(..)
        | EvaluatedConstTy::Unit
        | EvaluatedConstTy::Bytes(..)
        | EvaluatedConstTy::EnumVariant(..) => true,
        EvaluatedConstTy::Invalid => false,
    }
}

fn const_expr_is_fully_ground<'db>(db: &'db dyn HirAnalysisDb, expr: ConstExprId<'db>) -> bool {
    match expr.data(db) {
        ConstExpr::ExternConstFnCall {
            generic_args, args, ..
        }
        | ConstExpr::UserConstFnCall {
            generic_args, args, ..
        } => generic_args
            .iter()
            .chain(args.iter())
            .copied()
            .all(|arg| ty_is_fully_ground(db, arg)),
        ConstExpr::ArithBinOp { lhs, rhs, .. } => {
            ty_is_fully_ground(db, *lhs) && ty_is_fully_ground(db, *rhs)
        }
        ConstExpr::UnOp { expr, .. } | ConstExpr::Cast { expr, .. } => {
            ty_is_fully_ground(db, *expr)
        }
        ConstExpr::TraitConst(assoc) => trait_inst_is_fully_ground(db, assoc.inst()),
        ConstExpr::LocalBinding(_) => false,
    }
}

fn canonicalize_const_expr_for_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    expr: ConstExprId<'db>,
    env: ConstCanonEnv<'db>,
    mode: ConstCanonMode,
) -> ConstExprId<'db> {
    match expr.data(db) {
        ConstExpr::ExternConstFnCall {
            func,
            generic_args,
            args,
        } => ConstExprId::new(
            db,
            ConstExpr::ExternConstFnCall {
                func: *func,
                generic_args: generic_args
                    .iter()
                    .copied()
                    .map(|arg| canonicalize_ty_for_mode(db, arg, env, mode))
                    .collect(),
                args: args
                    .iter()
                    .copied()
                    .map(|arg| canonicalize_ty_for_mode(db, arg, env, mode))
                    .collect(),
            },
        ),
        ConstExpr::UserConstFnCall {
            func,
            generic_args,
            args,
        } => ConstExprId::new(
            db,
            ConstExpr::UserConstFnCall {
                func: *func,
                generic_args: generic_args
                    .iter()
                    .copied()
                    .map(|arg| canonicalize_ty_for_mode(db, arg, env, mode))
                    .collect(),
                args: args
                    .iter()
                    .copied()
                    .map(|arg| canonicalize_ty_for_mode(db, arg, env, mode))
                    .collect(),
            },
        ),
        ConstExpr::ArithBinOp { op, lhs, rhs } => ConstExprId::new(
            db,
            ConstExpr::ArithBinOp {
                op: *op,
                lhs: canonicalize_ty_for_mode(db, *lhs, env, mode),
                rhs: canonicalize_ty_for_mode(db, *rhs, env, mode),
            },
        ),
        ConstExpr::UnOp { op, expr } => ConstExprId::new(
            db,
            ConstExpr::UnOp {
                op: *op,
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
        ConstExpr::TraitConst(assoc) => ConstExprId::new(
            db,
            ConstExpr::TraitConst(if let Some(inst) = env.assoc_ty_subst {
                assoc.fold_with(db, &mut AssocTySubst::new(inst))
            } else {
                *assoc
            }),
        ),
        ConstExpr::LocalBinding(binding) => ConstExprId::new(db, ConstExpr::LocalBinding(*binding)),
    }
}

pub fn evaluate_type_level_const_expr<'db>(
    db: &'db dyn HirAnalysisDb,
    expr: ConstExprId<'db>,
    expected_ty: TyId<'db>,
    env: ConstCanonEnv<'db>,
) -> Option<ConstTyId<'db>> {
    let expr = canonicalize_const_expr_for_mode(db, expr, env, ConstCanonMode::Identity);
    evaluate_type_level_int_const_expr(db, expr, expected_ty).or_else(|| {
        if !const_expr_is_fully_ground(db, expr) {
            return None;
        }

        match expr.data(db) {
            ConstExpr::UserConstFnCall {
                func,
                generic_args,
                args,
            } => {
                let args = args
                    .iter()
                    .copied()
                    .map(|arg| sem_const_from_ty(db, arg))
                    .collect::<Option<Vec<_>>>()?;
                match eval_body_owner_const_with_args(
                    db,
                    crate::analysis::ty::ty_check::BodyOwner::Func(*func),
                    generic_args.clone(),
                    args,
                ) {
                    Ok(value) => {
                        Some(const_ty_from_sem_const(db, value).evaluate(db, Some(expected_ty)))
                    }
                    Err(err) => Some(ConstTyId::invalid(
                        db,
                        invalid_cause_from_ctfe_error(
                            db,
                            crate::analysis::ty::ty_check::BodyOwner::Func(*func),
                            err,
                        ),
                    )),
                }
            }
            ConstExpr::TraitConst(assoc) => const_ty_from_assoc_const_use(db, *assoc)
                .map(|const_ty| const_ty.evaluate(db, Some(expected_ty))),
            ConstExpr::ExternConstFnCall { .. }
            | ConstExpr::ArithBinOp { .. }
            | ConstExpr::UnOp { .. }
            | ConstExpr::Cast { .. }
            | ConstExpr::LocalBinding(_) => None,
        }
    })
}

fn const_ty_is_fully_ground<'db>(db: &'db dyn HirAnalysisDb, const_ty: ConstTyId<'db>) -> bool {
    match const_ty.data(db) {
        ConstTyData::TyVar(..) | ConstTyData::TyParam(..) | ConstTyData::Hole(..) => false,
        ConstTyData::Evaluated(value, ty) => {
            ty_is_fully_ground(db, *ty) && evaluated_const_ty_is_fully_ground(db, value)
        }
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
        ConstTyData::Evaluated(..) => Some(const_ty),
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
        ConstTyData::TyVar(..) | ConstTyData::TyParam(..) | ConstTyData::Hole(..) => None,
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
        ConstTyData::Evaluated(value, ty) => ConstTyId::new(
            db,
            ConstTyData::Evaluated(
                canonicalize_evaluated_const_ty_for_mode(db, value, env, mode),
                canonicalize_ty_for_mode(db, *ty, env, mode),
            ),
        ),
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
            },
        ),
    };

    match mode {
        ConstCanonMode::Stored => canonicalized,
        ConstCanonMode::Identity => {
            concretize_const_ty_if_ground(db, canonicalized, env).unwrap_or(canonicalized)
        }
        ConstCanonMode::Display => concretize_const_ty_if_ground(db, canonicalized, env)
            .or_else(|| canonicalize_const_ty_for_display(db, canonicalized, env))
            .unwrap_or(canonicalized),
    }
}

fn canonicalize_evaluated_const_ty_for_mode<'db>(
    db: &'db dyn HirAnalysisDb,
    value: &EvaluatedConstTy<'db>,
    env: ConstCanonEnv<'db>,
    mode: ConstCanonMode,
) -> EvaluatedConstTy<'db> {
    match value {
        EvaluatedConstTy::Tuple(elems) => EvaluatedConstTy::Tuple(
            elems
                .iter()
                .copied()
                .map(|elem| canonicalize_ty_for_mode(db, elem, env, mode))
                .collect(),
        ),
        EvaluatedConstTy::Array(elems) => EvaluatedConstTy::Array(
            elems
                .iter()
                .copied()
                .map(|elem| canonicalize_ty_for_mode(db, elem, env, mode))
                .collect(),
        ),
        EvaluatedConstTy::Record(fields) => EvaluatedConstTy::Record(
            fields
                .iter()
                .copied()
                .map(|field| canonicalize_ty_for_mode(db, field, env, mode))
                .collect(),
        ),
        _ => value.clone(),
    }
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

fn display_const_canon_env<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
) -> Option<ConstCanonEnv<'db>> {
    match const_ty.data(db) {
        ConstTyData::UnEvaluated { body, .. } => Some(ConstCanonEnv::new(
            body.scope(),
            assumptions_for_body(db, *body),
            None,
        )),
        ConstTyData::Abstract(expr, _) => match expr.data(db) {
            ConstExpr::UserConstFnCall { func, .. } | ConstExpr::ExternConstFnCall { func, .. } => {
                Some(ConstCanonEnv::new(
                    func.scope(),
                    PredicateListId::empty_list(db),
                    None,
                ))
            }
            ConstExpr::TraitConst(assoc) => Some(ConstCanonEnv::new(
                assoc.origin_scope(),
                assoc.assumptions(),
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
    if let Some(env) = display_const_canon_env(db, *const_ty) {
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
                    ConstTyData::Evaluated(..) | ConstTyData::Abstract(..)
                )
            {
                if let ConstTyData::Abstract(expr, expected_ty) = normalized.data(db) {
                    evaluate_abstract_int_const_expr(db, *expr, *expected_ty).map_or_else(
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
            evaluate_abstract_int_const_expr(db, *expr, *expected_ty)
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

    if const_def.is_some()
        && eval_body_owner_const(
            db,
            super::ty_check::BodyOwner::AnonConstBody {
                body: *body,
                expected: expected_ty,
            },
            generic_args.clone(),
        )
        .is_err()
    {
        return Err(InvalidCause::Other);
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

    if let ConstTyData::Abstract(expr, ty) = const_ty.data(db)
        && let ConstExpr::TraitConst(assoc) = expr.data(db)
    {
        if let Some(resolved) = const_ty_from_assoc_const_use(db, *assoc) {
            let evaluated = resolved.evaluate(db, expected_ty.or(Some(*ty)));
            if evaluated.ty(db).has_invalid(db) {
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

    let (body, const_ty_ty, generic_args, const_def) = match const_ty.data(db) {
        ConstTyData::UnEvaluated {
            body,
            ty,
            generic_args,
            const_def,
            ..
        } => (*body, *ty, generic_args.clone(), *const_def),
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
        let data = ConstTyData::Evaluated(
            EvaluatedConstTy::Invalid,
            TyId::invalid(db, InvalidCause::ParseError),
        );
        return ConstTyId::new(db, data);
    };

    let expr = expr.clone();

    #[derive(Clone, Copy, Debug)]
    struct CheckedIntTy {
        bits: u16,
        signed: bool,
    }

    fn checked_int_ty_from_ty<'db>(
        db: &'db dyn HirAnalysisDb,
        expected: Option<TyId<'db>>,
    ) -> Option<CheckedIntTy> {
        let expected = expected?;
        let base_ty = expected.base_ty(db);
        let TyData::TyBase(TyBase::Prim(prim)) = base_ty.data(db) else {
            return None;
        };
        Some(match prim {
            // unsigned
            PrimTy::U8 => CheckedIntTy {
                bits: 8,
                signed: false,
            },
            PrimTy::U16 => CheckedIntTy {
                bits: 16,
                signed: false,
            },
            PrimTy::U32 => CheckedIntTy {
                bits: 32,
                signed: false,
            },
            PrimTy::U64 => CheckedIntTy {
                bits: 64,
                signed: false,
            },
            PrimTy::U128 => CheckedIntTy {
                bits: 128,
                signed: false,
            },
            PrimTy::U256 | PrimTy::Usize => CheckedIntTy {
                bits: 256,
                signed: false,
            },
            // signed
            PrimTy::I8 => CheckedIntTy {
                bits: 8,
                signed: true,
            },
            PrimTy::I16 => CheckedIntTy {
                bits: 16,
                signed: true,
            },
            PrimTy::I32 => CheckedIntTy {
                bits: 32,
                signed: true,
            },
            PrimTy::I64 => CheckedIntTy {
                bits: 64,
                signed: true,
            },
            PrimTy::I128 => CheckedIntTy {
                bits: 128,
                signed: true,
            },
            PrimTy::I256 | PrimTy::Isize => CheckedIntTy {
                bits: 256,
                signed: true,
            },
            _ => return None,
        })
    }

    fn u256_modulus() -> BigUint {
        BigUint::one() << 256usize
    }

    fn signed_bounds(ty: CheckedIntTy) -> (BigInt, BigInt) {
        debug_assert!(ty.signed);
        let half = BigInt::one() << ((ty.bits - 1) as usize);
        let min = -half.clone();
        let max = half - BigInt::one();
        (min, max)
    }

    fn unsigned_max(ty: CheckedIntTy) -> BigInt {
        debug_assert!(!ty.signed);
        (BigInt::one() << (ty.bits as usize)) - BigInt::one()
    }

    fn in_range(value: &BigInt, ty: CheckedIntTy) -> bool {
        if ty.signed {
            let (min, max) = signed_bounds(ty);
            value >= &min && value <= &max
        } else {
            value >= &BigInt::zero() && value <= &unsigned_max(ty)
        }
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
            _ => value.to_biguint().and_then(|v| (v < modulus).then_some(v)),
        }
    }

    fn u256_word_to_bigint(word: &BigUint, ty: CheckedIntTy) -> BigInt {
        if !ty.signed {
            return BigInt::from(word.clone());
        }

        let bits = ty.bits as usize;
        let mask = if bits == 256 {
            (BigUint::one() << 256usize) - BigUint::one()
        } else {
            (BigUint::one() << (ty.bits as usize)) - BigUint::one()
        };
        let value_bits = word & mask;
        let sign_bit = BigUint::one() << ((ty.bits - 1) as usize);
        if (value_bits.clone() & sign_bit).is_zero() {
            BigInt::from(value_bits)
        } else {
            BigInt::from(value_bits) - (BigInt::one() << (ty.bits as usize))
        }
    }

    #[derive(Clone, Copy, Debug)]
    enum ConstIntError {
        Overflow,
        DivisionByZero,
        NegativeExponent,
        /// The expression is not a pure integer expression (e.g. contains
        /// function calls). Fall through to CTFE instead of reporting an error.
        NotIntExpr,
    }

    fn invalid_cause_from_const_int_error<'db>(
        body: Body<'db>,
        expr: ExprId,
        err: ConstIntError,
    ) -> Option<InvalidCause<'db>> {
        match err {
            ConstIntError::Overflow => {
                Some(InvalidCause::ConstEvalArithmeticOverflow { body, expr })
            }
            ConstIntError::DivisionByZero => {
                Some(InvalidCause::ConstEvalDivisionByZero { body, expr })
            }
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
        expected: Option<CheckedIntTy>,
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

            Expr::Lit(LitKind::Int(i)) => Ok(BigInt::from(i.data(db).clone())),

            Expr::Un(inner, op) => {
                let Partial::Present(inner) = inner.data(db, body) else {
                    return Err(ConstIntError::Overflow);
                };
                let value = eval_int_expr(db, body, inner, expected, generic_args)?;
                match op {
                    crate::core::hir_def::expr::UnOp::Minus => {
                        let Some(expected) = expected else {
                            return Err(ConstIntError::Overflow);
                        };
                        let neg = -value;
                        if !in_range(&neg, expected) {
                            return Err(ConstIntError::Overflow);
                        }
                        Ok(neg)
                    }
                    crate::core::hir_def::expr::UnOp::Plus => Ok(value),
                    _ => Err(ConstIntError::NotIntExpr),
                }
            }

            Expr::Bin(lhs_id, rhs_id, op) => {
                let Partial::Present(lhs) = lhs_id.data(db, body) else {
                    return Err(ConstIntError::Overflow);
                };
                let Partial::Present(rhs) = rhs_id.data(db, body) else {
                    return Err(ConstIntError::Overflow);
                };
                let expected = expected.unwrap_or(CheckedIntTy {
                    bits: 256,
                    signed: false,
                });

                let lhs = eval_int_expr(db, body, lhs, Some(expected), generic_args)?;
                let rhs = eval_int_expr(db, body, rhs, Some(expected), generic_args)?;

                match op {
                    crate::core::hir_def::expr::BinOp::Arith(op) => match op {
                        crate::core::hir_def::expr::ArithBinOp::Add => {
                            let result = lhs + rhs;
                            if !in_range(&result, expected) {
                                Err(ConstIntError::Overflow)
                            } else {
                                Ok(result)
                            }
                        }
                        crate::core::hir_def::expr::ArithBinOp::Sub => {
                            let result = lhs - rhs;
                            if !in_range(&result, expected) {
                                Err(ConstIntError::Overflow)
                            } else {
                                Ok(result)
                            }
                        }
                        crate::core::hir_def::expr::ArithBinOp::Mul => {
                            let result = lhs * rhs;
                            if !in_range(&result, expected) {
                                Err(ConstIntError::Overflow)
                            } else {
                                Ok(result)
                            }
                        }
                        crate::core::hir_def::expr::ArithBinOp::Div => {
                            if rhs.is_zero() {
                                return Err(ConstIntError::DivisionByZero);
                            }
                            if expected.signed {
                                let (min, _) = signed_bounds(expected);
                                if lhs == min && rhs == -BigInt::one() {
                                    return Err(ConstIntError::Overflow);
                                }
                            }
                            let result = lhs / rhs;
                            if !in_range(&result, expected) {
                                Err(ConstIntError::Overflow)
                            } else {
                                Ok(result)
                            }
                        }
                        crate::core::hir_def::expr::ArithBinOp::Rem => {
                            if rhs.is_zero() {
                                return Err(ConstIntError::DivisionByZero);
                            }
                            let result = lhs % rhs;
                            if !in_range(&result, expected) {
                                Err(ConstIntError::Overflow)
                            } else {
                                Ok(result)
                            }
                        }
                        crate::core::hir_def::expr::ArithBinOp::Pow => {
                            if rhs.sign() == Sign::Minus {
                                return Err(ConstIntError::NegativeExponent);
                            }
                            let Some(exp) = rhs.to_biguint() else {
                                return Err(ConstIntError::NegativeExponent);
                            };
                            let mut acc = BigInt::one();
                            let mut base = lhs;
                            let mut exp = exp;
                            while !exp.is_zero() {
                                if (&exp & BigUint::one()) == BigUint::one() {
                                    acc *= base.clone();
                                    if !in_range(&acc, expected) {
                                        return Err(ConstIntError::Overflow);
                                    }
                                }
                                exp >>= 1usize;
                                if exp.is_zero() {
                                    break;
                                }
                                base = base.clone() * base;
                                if !in_range(&base, expected) {
                                    return Err(ConstIntError::Overflow);
                                }
                            }
                            Ok(acc)
                        }
                        _ => Err(ConstIntError::NotIntExpr),
                    },
                    _ => Err(ConstIntError::NotIntExpr),
                }
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
                    PathRes::TraitConst(_recv_ty, inst, name) => {
                        let solve_cx =
                            TraitSolveCx::new(db, body.scope()).with_assumptions(assumptions);
                        const_ty_from_trait_const(db, solve_cx, inst, name)
                            .ok_or(ConstIntError::NotIntExpr)?
                    }
                    _ => return Err(ConstIntError::NotIntExpr),
                };

                let evaluated = const_ty.evaluate(db, None);
                match evaluated.data(db) {
                    ConstTyData::Evaluated(EvaluatedConstTy::LitInt(i), _) => {
                        let word = i.data(db);
                        let expected_for_interpretation = expected.unwrap_or(CheckedIntTy {
                            bits: 256,
                            signed: false,
                        });
                        Ok(u256_word_to_bigint(word, expected_for_interpretation))
                    }
                    _ => Err(ConstIntError::NotIntExpr),
                }
            }

            _ => Err(ConstIntError::NotIntExpr),
        }
    }

    if generic_args.is_empty()
        && let Expr::Path(path) = &expr
    {
        let Some(path) = path.to_opt() else {
            return ConstTyId::new(
                db,
                ConstTyData::Evaluated(
                    EvaluatedConstTy::Invalid,
                    TyId::invalid(db, InvalidCause::ParseError),
                ),
            );
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
                            return mk_abstract(expected_ty.unwrap_or_else(|| const_ty.ty(db)));
                        }
                        return evaluated;
                    }

                    if let Some(expected_ty) = expected_ty {
                        return mk_abstract(expected_ty);
                    }
                }
                PathRes::EnumVariant(variant) if variant.ty.is_unit_variant_only_enum(db) => {
                    let evaluated = EvaluatedConstTy::EnumVariant(variant.variant);
                    let const_ty =
                        ConstTyId::new(db, ConstTyData::Evaluated(evaluated, variant.ty));
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

        return ConstTyId::new(
            db,
            ConstTyData::Evaluated(
                EvaluatedConstTy::Invalid,
                TyId::invalid(db, InvalidCause::InvalidConstTyExpr { body }),
            ),
        );
    }

    // Try BigInt-based evaluation for integer arithmetic expressions (checked arithmetic).
    if matches!(
        expr,
        Expr::Block(..) | Expr::Un(..) | Expr::Bin(..) | Expr::Lit(LitKind::Int(..))
    ) {
        let expected_int_ty = expected_ty.and_then(|ty| checked_int_ty_from_ty(db, Some(ty)));
        match eval_int_expr(db, body, &expr, expected_int_ty, &generic_args) {
            Ok(value) => {
                if let Some(word) = bigint_to_u256_word(&value) {
                    let mut table = UnificationTable::new(db);
                    let resolved = EvaluatedConstTy::LitInt(IntegerId::new(db, word));
                    let ty = table.new_var(TyVarSort::Integral, &Kind::Star);
                    let data = match check_const_ty(db, ty, expected_ty, &mut table) {
                        Ok(ty) => ConstTyData::Evaluated(resolved, ty),
                        Err(err) => ConstTyData::Evaluated(resolved, TyId::invalid(db, err)),
                    };
                    return ConstTyId::new(db, data);
                }
            }
            Err(ConstIntError::NotIntExpr) => {
                // Expression contains constructs we can't evaluate with BigInt
                // (e.g. function calls). Fall through to CTFE.
            }
            Err(err) => {
                // Genuine arithmetic error (overflow, division by zero, etc.).
                // For Block/Un/Bin, report error. For plain int literals, fall through to CTFE.
                if const_def.is_some() {
                    return ConstTyId::invalid(db, InvalidCause::Other);
                }
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

    let evaluated = match eval_body_owner_const(
        db,
        super::ty_check::BodyOwner::AnonConstBody {
            body,
            expected: validated.expected_ty,
        },
        generic_args,
    )
    .map(|value| const_ty_from_sem_const(db, value))
    {
        Ok(value) => value,
        Err(CtfeError::NotConstEvaluable { .. }) => validated.const_ty,
        Err(err) => {
            if const_def.is_some() {
                return ConstTyId::invalid(db, InvalidCause::Other);
            }
            return ConstTyId::invalid(
                db,
                invalid_cause_from_ctfe_error(
                    db,
                    super::ty_check::BodyOwner::AnonConstBody {
                        body,
                        expected: validated.expected_ty,
                    },
                    err,
                ),
            );
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
        CtfeError::NotConstEvaluable { .. }
        | CtfeError::InvalidOperation { .. }
        | CtfeError::InvalidBorrow { .. }
        | CtfeError::InvalidProviderUse { .. }
        | CtfeError::OutOfBounds { .. }
        | CtfeError::VariantMismatch { .. }
        | CtfeError::UninitializedLocal { .. }
        | CtfeError::CalleeError { .. } => InvalidCause::ConstEvalUnsupported { body, expr },
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

fn origin_expr_for_const_eval_diag<'db>(
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
    let ty = crate::analysis::semantic::sem_const_ty(db, value);
    let evaluated = match value.value(db) {
        SemConstValue::Unit => EvaluatedConstTy::Unit,
        SemConstValue::Scalar { value, .. } => match value {
            crate::analysis::semantic::SemConstScalar::Bool(flag) => {
                EvaluatedConstTy::LitBool(flag)
            }
            crate::analysis::semantic::SemConstScalar::Int { value } => {
                let int = int_ty_shape(db, ty).map_or(value.clone(), |(bits, _)| {
                    normalize_int_to_shape(value.clone(), bits, false)
                });
                let (_, bytes) = int.to_bytes_be();
                EvaluatedConstTy::LitInt(IntegerId::new(db, BigUint::from_bytes_be(&bytes)))
            }
            crate::analysis::semantic::SemConstScalar::Bytes(bytes) => {
                EvaluatedConstTy::Bytes(bytes)
            }
        },
        SemConstValue::Tuple { elems, .. } => EvaluatedConstTy::Tuple(
            elems
                .iter()
                .copied()
                .map(|elem| TyId::const_ty(db, const_ty_from_sem_const(db, elem)))
                .collect(),
        ),
        SemConstValue::Struct { fields, .. } => EvaluatedConstTy::Record(
            fields
                .iter()
                .copied()
                .map(|field| TyId::const_ty(db, const_ty_from_sem_const(db, field)))
                .collect(),
        ),
        SemConstValue::Array { elems, .. } => EvaluatedConstTy::Array(
            elems
                .iter()
                .copied()
                .map(|elem| TyId::const_ty(db, const_ty_from_sem_const(db, elem)))
                .collect(),
        ),
        SemConstValue::TypeLevel { const_ty, .. } => {
            let TyData::ConstTy(const_ty) = const_ty.data(db) else {
                return ConstTyId::invalid(db, InvalidCause::Other);
            };
            return *const_ty;
        }
        SemConstValue::Enum {
            variant, fields, ..
        } => enum_const_ty_from_sem_const(db, ty, variant, fields.as_ref()),
    };
    ConstTyId::new(db, ConstTyData::Evaluated(evaluated, ty))
}

fn enum_const_ty_from_sem_const<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
    variant: VariantIndex,
    fields: &[SemConstId<'db>],
) -> EvaluatedConstTy<'db> {
    if !fields.is_empty() {
        return EvaluatedConstTy::Invalid;
    }

    let Some(enum_) = ty.as_enum(db) else {
        return EvaluatedConstTy::Invalid;
    };
    let Some(variant) = enum_.variants(db).nth(variant.0 as usize) else {
        return EvaluatedConstTy::Invalid;
    };
    EvaluatedConstTy::EnumVariant(crate::hir_def::EnumVariant::new(variant.owner, variant.idx))
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
/// its const references (module consts and trait consts through their
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
) -> bool {
    use crate::analysis::ty::ty_check::ConstRef;

    let mut visited = FxHashSet::default();
    visited.insert(start_body);
    let mut frontier = vec![(start_body, start_expected)];
    while let Some((body, expected)) = frontier.pop() {
        let typed_body = &check_anon_const_body(db, body, expected).1;
        for cref in typed_body.const_refs() {
            let next =
                match cref {
                    ConstRef::Const(const_) => const_
                        .body(db)
                        .to_opt()
                        .map(|next_body| (next_body, const_.ty(db))),
                    ConstRef::TraitConst(assoc) => const_ty_from_assoc_const_use(db, assoc)
                        .and_then(|const_ty| match const_ty.data(db) {
                            ConstTyData::UnEvaluated {
                                body, ty: Some(ty), ..
                            } => Some((*body, *ty)),
                            _ => None,
                        }),
                };
            let Some((next_body, next_expected)) = next else {
                continue;
            };
            if next_body == start_body {
                return true;
            }
            if next_expected.has_invalid(db) || !visited.insert(next_body) {
                continue;
            }
            frontier.push((next_body, next_expected));
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

pub(crate) fn const_ty_or_abstract_from_assoc_const_use<'db>(
    db: &'db dyn HirAnalysisDb,
    assoc: AssocConstUse<'db>,
    expected_ty: TyId<'db>,
) -> Option<ConstTyId<'db>> {
    let Some(evaluated) = const_ty_from_assoc_const_use(db, assoc) else {
        return Some(abstract_const_ty_from_assoc_const_use(
            db,
            assoc,
            expected_ty,
        ));
    };
    let evaluated = evaluated.evaluate(db, Some(expected_ty));
    if evaluated.ty(db).has_invalid(db) {
        return Some(abstract_const_ty_from_assoc_const_use(
            db,
            assoc,
            expected_ty,
        ));
    }
    Some(evaluated)
}

pub(super) fn const_ty_from_trait_const<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    name: IdentId<'db>,
) -> Option<ConstTyId<'db>> {
    let trait_ = inst.def(db);
    let selected = crate::analysis::ty::trait_def::assoc_const_body_and_impl_args_for_trait_inst(
        db, solve_cx, inst, name,
    );
    let (body, generic_args) = if let Some(selected) = selected {
        selected
    } else if matches!(
        is_goal_satisfiable(db, solve_cx, inst),
        GoalSatisfiability::Satisfied(_)
    ) {
        trait_
            .const_(db, name)
            .and_then(|c| c.default_body(db))
            .map(|body| (body, inst.args(db).clone()))?
    } else {
        return None;
    };

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
    pub fn ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match self.data(db) {
            ConstTyData::TyVar(_, ty) => *ty,
            ConstTyData::TyParam(_, ty) => *ty,
            ConstTyData::Hole(ty, _) => *ty,
            ConstTyData::Evaluated(_, ty) => *ty,
            ConstTyData::Abstract(_, ty) => *ty,
            ConstTyData::UnEvaluated { ty, .. } => {
                ty.unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
            }
        }
    }

    pub fn pretty_print_with_mode(self, db: &'db dyn HirAnalysisDb, mode: TypePrintMode) -> String {
        if matches!(mode, TypePrintMode::Concrete)
            && let Some(env) = display_const_canon_env(db, self)
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
            ConstTyData::Evaluated(resolved, _) => resolved.pretty_print(db),
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
        Self::from_body_with_generic_args_and_preservation(
            db,
            body,
            ty,
            const_def,
            generic_args,
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
        let data = ConstTyData::UnEvaluated {
            body,
            ty,
            const_def,
            generic_args,
            preserve_unevaluated,
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

    pub(super) fn with_ty(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        self.swap_ty(db, ty)
    }

    pub(super) fn invalid(db: &'db dyn HirAnalysisDb, cause: InvalidCause<'db>) -> Self {
        let resolved = EvaluatedConstTy::Invalid;
        let ty = TyId::invalid(db, cause);
        let data = ConstTyData::Evaluated(resolved, ty);
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
        provenance: ProvenanceId<'db>,
    ) -> Self {
        Self::hole_with_id(db, ty, HoleId::structural(db, ty, origin, provenance))
    }

    pub fn bound_callable_hole(
        db: &'db dyn HirAnalysisDb,
        ty: TyId<'db>,
        func: Func<'db>,
        origin: CallableInputLayoutHoleOrigin,
        ordinal: usize,
    ) -> Self {
        Self::hole_with_id(db, ty, HoleId::bound_callable(func, origin, ordinal))
    }

    fn swap_ty(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        let data = match self.data(db) {
            ConstTyData::TyVar(var, _) => ConstTyData::TyVar(var.clone(), ty),
            ConstTyData::TyParam(param, _) => ConstTyData::TyParam(param.clone(), ty),
            ConstTyData::Hole(_, hole_id) => ConstTyData::Hole(
                ty,
                match hole_id {
                    HoleId::Structural(hole_id) => HoleId::Structural(StructuralHoleId::new(
                        db,
                        ty,
                        hole_id.origin(db),
                        hole_id.provenance(db),
                    )),
                    HoleId::Bound(hole_id) => HoleId::Bound(*hole_id),
                },
            ),
            ConstTyData::Evaluated(evaluated, _) => ConstTyData::Evaluated(evaluated.clone(), ty),
            ConstTyData::Abstract(expr, _) => ConstTyData::Abstract(*expr, ty),
            ConstTyData::UnEvaluated {
                body,
                const_def,
                generic_args,
                preserve_unevaluated,
                ..
            } => ConstTyData::UnEvaluated {
                body: *body,
                ty: Some(ty),
                const_def: *const_def,
                generic_args: generic_args.clone(),
                preserve_unevaluated: *preserve_unevaluated,
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
    Evaluated(EvaluatedConstTy<'db>, TyId<'db>),
    Abstract(ConstExprId<'db>, TyId<'db>),
    UnEvaluated {
        body: Body<'db>,
        ty: Option<TyId<'db>>,
        const_def: Option<Const<'db>>,
        generic_args: Vec<TyId<'db>>,
        preserve_unevaluated: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EvaluatedConstTy<'db> {
    LitInt(IntegerId<'db>),
    LitBool(bool),
    Unit,
    Tuple(Vec<TyId<'db>>),
    Array(Vec<TyId<'db>>),
    Bytes(Vec<u8>),
    Record(Vec<TyId<'db>>),
    EnumVariant(EnumVariant<'db>),
    Invalid,
}

impl EvaluatedConstTy<'_> {
    pub fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        match self {
            EvaluatedConstTy::LitInt(val) => {
                format!("{}", val.data(db))
            }
            EvaluatedConstTy::LitBool(val) => format!("{val}"),
            EvaluatedConstTy::Unit => "()".to_string(),
            EvaluatedConstTy::Tuple(elems) => {
                let elems = elems
                    .iter()
                    .map(|elem| elem.pretty_print(db).as_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({elems})")
            }
            EvaluatedConstTy::Array(elems) => {
                let elems = elems
                    .iter()
                    .map(|elem| elem.pretty_print(db).as_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{elems}]")
            }
            EvaluatedConstTy::Bytes(bytes) => {
                let bytes = bytes
                    .iter()
                    .map(|b| b.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{bytes}]")
            }
            EvaluatedConstTy::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|field| field.pretty_print(db).as_str())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{fields}}}")
            }
            EvaluatedConstTy::EnumVariant(variant) => {
                let enum_name = variant
                    .enum_
                    .name(db)
                    .to_opt()
                    .map(|n| n.data(db).to_string())
                    .unwrap_or_else(|| "<unknown>".to_string());
                let variant_name = variant.name(db).unwrap_or("<unknown>");
                format!("{enum_name}::{variant_name}")
            }
            EvaluatedConstTy::Invalid => "<invalid>".to_string(),
        }
    }
}
