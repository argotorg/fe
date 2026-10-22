//! Builtin implementations of the core callable traits for closures and
//! function items.
//!
//! A callable value implements `Fn<A>` and `FnMut<A>` of `core::functional`,
//! where `A` is the shape of its own parameter list, without HIR `impl`
//! items: `Out` is its result and `E` the effects a call runs, those a
//! closure's body uses or a function's own. A closure's body only reads its
//! captures and a function item holds nothing, so the `FnMut` implementation
//! is the `Fn` one. The trait solver, normalizer, method selection and callee
//! resolution derive the relationship from here.

use crate::hir_def::ModeMarker;
use common::indexmap::IndexMap;
use cranelift_entity::EntityRef;
use salsa::SalsaValue;

use super::{
    binder::Binder,
    corelib::resolve_core_trait,
    effects::rows::RowComponent,
    shape::Shape,
    trait_def::{AssocBinding, ImplementorId, ImplementorOrigin, TraitInstId},
    ty_check::{BodyOwner, EffectParamSite, infer_body},
    ty_def::{ClosureDef, ClosureTy, TyBase, TyData, TyId},
};
use crate::{
    analysis::HirAnalysisDb,
    core::semantic::EffectRequirement,
    hir_def::{
        Body, CallableDef, Cond, CondId, Expr, ExprId, Func, IdentId, Partial, Stmt, StmtId, Trait,
        params::FuncParamMode, scope_graph::ScopeId,
    },
};

/// A core callable trait, `Fn` or `FnMut`: whether its receiver is
/// `mut self`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct CallableTrait {
    pub(crate) mut_receiver: bool,
}

impl CallableTrait {
    /// The callable trait `trait_` is, if it is `Fn` or `FnMut` of
    /// `core::functional`.
    pub(crate) fn of<'db>(db: &'db dyn HirAnalysisDb, trait_: Trait<'db>) -> Option<Self> {
        let this = match trait_.name(db).to_opt()?.data(db).as_str() {
            "Fn" => Self {
                mut_receiver: false,
            },
            "FnMut" => Self { mut_receiver: true },
            _ => return None,
        };
        (this.trait_(db, trait_.scope()) == Some(trait_)).then_some(this)
    }

    fn trait_<'db>(self, db: &'db dyn HirAnalysisDb, scope: ScopeId<'db>) -> Option<Trait<'db>> {
        let name = if self.mut_receiver { "FnMut" } else { "Fn" };
        resolve_core_trait(db, scope, &["functional", name])
    }

    /// The trait's method: `call`, or `call_mut` for a `mut` receiver.
    pub(crate) fn method(self) -> &'static str {
        if self.mut_receiver {
            "call_mut"
        } else {
            "call"
        }
    }
}

/// The parameters an instance of a callable trait takes: those its shape
/// argument lists, unless that is no parameter list yet.
pub(crate) fn callable_params<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
) -> Option<Vec<Shape<'db>>> {
    inst.args(db).get(1)?.as_params_shape(db)
}

/// A type whose values implement the callable traits of their parameter
/// list: a closure, or a function item. A variant constructor, an
/// `unsafe fn` and a projection are not callable values: a variant is made
/// rather than called, an `unsafe fn` may only be called in an `unsafe`
/// context, and a projection grants accesses rather than returning a value.
/// Nor is a function with a spread.
#[derive(Debug, Clone, Copy)]
pub(crate) enum CallableValue<'db> {
    Closure(ClosureTy<'db>),
    /// A function item with its generic arguments.
    Func(Func<'db>, &'db [TyId<'db>]),
}

impl<'db> CallableValue<'db> {
    pub(crate) fn of(db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Option<Self> {
        if let Some(closure) = ty.as_closure(db) {
            return Some(Self::Closure(closure));
        }
        let (base, args) = ty.decompose_ty_app(db);
        let TyData::TyBase(TyBase::Func(CallableDef::Func(func))) = base.data(db) else {
            return None;
        };
        (!func.is_unsafe(db)
            && !func.is_projection(db)
            && !func.is_raw_place(db)
            && !func.is_maybe_mut(db)
            && CallableDef::Func(*func).spread(db).is_none())
        .then_some(Self::Func(*func, args))
    }

    /// The shape of each of the value's parameters.
    fn params(self, db: &'db dyn HirAnalysisDb) -> Vec<Shape<'db>> {
        match self {
            Self::Closure(closure) => closure.params(db).clone(),
            Self::Func(func, args) => CallableDef::Func(func)
                .param_shapes(db, None)
                .0
                .into_iter()
                .map(|shape| shape.instantiate(db, args))
                .collect(),
        }
    }

    fn ret_ty(self, db: &'db dyn HirAnalysisDb) -> TyId<'db> {
        match self {
            Self::Closure(closure) => closure.ret_ty(db),
            Self::Func(func, args) => CallableDef::Func(func).ret_ty(db).instantiate(db, args),
        }
    }

    fn scope(self, db: &'db dyn HirAnalysisDb) -> ScopeId<'db> {
        match self {
            Self::Closure(closure) => closure.def(db).body.scope(),
            Self::Func(func, _) => func.scope(),
        }
    }

    /// The effects a call runs, the callable trait's row `E`: those a
    /// closure's body uses, or a function's own effects.
    pub(crate) fn effects(self, db: &'db dyn HirAnalysisDb) -> Vec<RowComponent<'db>> {
        match self {
            Self::Closure(closure) => closure.effects(db).clone(),
            // A `mut?` function is no callable value.
            Self::Func(func, args) => func
                .effect_requirements(db, None)
                .iter()
                .map(|requirement| RowComponent {
                    name: requirement.binding_name,
                    key: Binder::bind(func.into(), requirement.key.clone()).instantiate(db, args),
                    is_mut: requirement.is_mut,
                    key_syntax: requirement.binding_ty,
                })
                .collect(),
        }
    }

    /// The instance of `trait_` the value implements, if `trait_` is a
    /// callable trait: the shape of its parameter list, and its result as
    /// `Out`.
    fn callable_inst(
        self,
        db: &'db dyn HirAnalysisDb,
        self_ty: TyId<'db>,
        trait_: Trait<'db>,
    ) -> Option<TraitInstId<'db>> {
        CallableTrait::of(db, trait_)?;
        Some(TraitInstId::new(
            db,
            trait_,
            vec![self_ty, TyId::params_shape(db, &self.params(db))],
            IndexMap::from_iter([(AssocBinding::of(out_ident(db)), self.ret_ty(db))]),
            ModeMarker::None,
        ))
    }
}

/// The instance of `goal`'s callable trait a function item, `goal`'s self
/// type, implements: its own shape, which its type does not print.
pub(crate) fn func_item_callable_inst<'db>(
    db: &'db dyn HirAnalysisDb,
    goal: TraitInstId<'db>,
) -> Option<TraitInstId<'db>> {
    let self_ty = goal.self_ty(db);
    let value @ CallableValue::Func(..) = CallableValue::of(db, self_ty)? else {
        return None;
    };
    value.callable_inst(db, self_ty, goal.def(db))
}

/// Both callable trait instances a value of type `ty` implements, `Fn` and
/// `FnMut`, if it is a callable value.
pub(crate) fn callable_insts<'db>(
    db: &'db dyn HirAnalysisDb,
    ty: TyId<'db>,
) -> Vec<TraitInstId<'db>> {
    let Some(value) = CallableValue::of(db, ty) else {
        return Vec::new();
    };
    [false, true]
        .into_iter()
        .filter_map(|mut_receiver| {
            let trait_ = CallableTrait { mut_receiver }.trait_(db, value.scope(db))?;
            value.callable_inst(db, ty, trait_)
        })
        .collect()
}

/// The builtin implementor of `goal` when its self type is a callable value
/// and its trait a callable trait. The solver unifies the shapes.
pub(crate) fn callable_implementor<'db>(
    db: &'db dyn HirAnalysisDb,
    goal: TraitInstId<'db>,
) -> Option<ImplementorId<'db>> {
    let self_ty = goal.self_ty(db);
    let value = CallableValue::of(db, self_ty)?;
    let inst = value.callable_inst(db, self_ty, goal.def(db))?;
    Some(ImplementorId::new(
        db,
        TraitInstId::new_simple(db, inst.def(db), inst.args(db).clone()),
        Vec::new(),
        IndexMap::from_iter([(out_ident(db), value.ret_ty(db))]),
        ImplementorOrigin::Callable,
    ))
}

/// `Out` of the callable trait `trait_` at the shape `shape` for the
/// callable value `self_ty`, unless the shape lists parameters of other
/// modes than the value's.
pub(crate) fn callable_out_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    self_ty: TyId<'db>,
    trait_: Trait<'db>,
    shape: TyId<'db>,
    name: IdentId<'db>,
) -> Option<TyId<'db>> {
    let value = CallableValue::of(db, self_ty)?;
    CallableTrait::of(db, trait_)?;
    let params = value.params(db);
    let same_modes = shape.as_params_shape(db).is_none_or(|listed| {
        listed.len() == params.len()
            && listed
                .iter()
                .zip(&params)
                .all(|(listed, param)| listed.same_modes(db, param))
    });
    (name == out_ident(db) && same_modes).then(|| value.ret_ty(db))
}

pub(crate) fn out_ident<'db>(db: &'db dyn HirAnalysisDb) -> IdentId<'db> {
    IdentId::new(db, "Out".to_string())
}

/// The innermost closure whose body each expression and statement of a body
/// is part of; `None` for the item body itself. A closure expression is part
/// of the region it appears in.
#[derive(Debug, Clone, PartialEq, Eq, SalsaValue)]
pub(crate) struct ClosureRegions {
    exprs: Vec<Option<ExprId>>,
    stmts: Vec<Option<ExprId>>,
}

impl ClosureRegions {
    pub(crate) fn expr(&self, expr: ExprId) -> Option<ExprId> {
        self.exprs[expr.index()]
    }

    pub(crate) fn stmt(&self, stmt: StmtId) -> Option<ExprId> {
        self.stmts[stmt.index()]
    }
}

#[salsa::tracked]
pub(crate) fn closure_regions<'db>(db: &'db dyn HirAnalysisDb, body: Body<'db>) -> ClosureRegions {
    let mut walk = RegionWalk {
        db,
        body,
        regions: ClosureRegions {
            exprs: vec![None; body.exprs(db).len()],
            stmts: vec![None; body.stmts(db).len()],
        },
    };
    walk.expr(body.expr(db), None);
    walk.regions
}

struct RegionWalk<'db> {
    db: &'db dyn HirAnalysisDb,
    body: Body<'db>,
    regions: ClosureRegions,
}

impl<'db> RegionWalk<'db> {
    fn expr(&mut self, expr: ExprId, region: Option<ExprId>) {
        self.regions.exprs[expr.index()] = region;
        let Partial::Present(data) = expr.data(self.db, self.body) else {
            return;
        };
        match data {
            Expr::Closure { body, .. } => self.expr(*body, Some(expr)),
            Expr::Lit(_) | Expr::Path(_) | Expr::UnsupportedMacroCall => {}
            Expr::Block(stmts, _) => stmts.iter().for_each(|&stmt| self.stmt(stmt, region)),
            Expr::Bin(lhs, rhs, _) | Expr::Assign(lhs, rhs) | Expr::AugAssign(lhs, rhs, _) => {
                self.expr(*lhs, region);
                self.expr(*rhs, region);
            }
            Expr::Un(inner, _)
            | Expr::Cast(inner, _)
            | Expr::Try(inner)
            | Expr::Field(inner, _)
            | Expr::ArrayRep(inner, _) => self.expr(*inner, region),
            Expr::Call(callee, args) => {
                self.expr(*callee, region);
                args.iter().for_each(|arg| self.expr(arg.expr, region));
            }
            Expr::MethodCall(receiver, _, _, args) => {
                self.expr(*receiver, region);
                args.iter().for_each(|arg| self.expr(arg.expr, region));
            }
            Expr::Assert(args) => args.iter().for_each(|arg| self.expr(arg.expr, region)),
            Expr::RecordInit(_, fields) => fields
                .iter()
                .for_each(|field| self.expr(field.expr, region)),
            Expr::Tuple(elems) | Expr::Array(elems) | Expr::ItemQuote(elems) => {
                elems.iter().for_each(|&elem| self.expr(elem, region))
            }
            Expr::If(cond, then, else_) => {
                self.cond(*cond, region);
                self.expr(*then, region);
                if let Some(else_) = else_ {
                    self.expr(*else_, region);
                }
            }
            Expr::Match(scrutinee, arms) => {
                self.expr(*scrutinee, region);
                if let Partial::Present(arms) = arms {
                    arms.iter().for_each(|arm| self.expr(arm.body, region));
                }
            }
            Expr::With(bindings, body) => {
                bindings
                    .iter()
                    .for_each(|binding| self.expr(binding.value, region));
                self.expr(*body, region);
            }
        }
    }

    fn stmt(&mut self, stmt: StmtId, region: Option<ExprId>) {
        self.regions.stmts[stmt.index()] = region;
        let Partial::Present(data) = stmt.data(self.db, self.body) else {
            return;
        };
        match data {
            Stmt::Let(_, _, init, else_) => {
                for expr in init.iter().chain(else_) {
                    self.expr(*expr, region);
                }
            }
            Stmt::For(_, iterable, body, _) => {
                self.expr(*iterable, region);
                self.expr(*body, region);
            }
            Stmt::While(cond, body) => {
                self.cond(*cond, region);
                self.expr(*body, region);
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.expr(*expr, region);
                }
            }
            Stmt::Yield(expr) | Stmt::Expr(expr) => self.expr(*expr, region),
            Stmt::Continue | Stmt::Break => {}
        }
    }

    fn cond(&mut self, cond: CondId, region: Option<ExprId>) {
        let Partial::Present(data) = cond.data(self.db, self.body) else {
            return;
        };
        match data {
            Cond::Expr(expr) | Cond::Let(_, expr) => self.expr(*expr, region),
            Cond::Bin(lhs, rhs, _) => {
                self.cond(*lhs, region);
                self.cond(*rhs, region);
            }
        }
    }
}

/// The closure whose body a call of `func`, a method of the callable trait
/// instance `inst`, runs, as typed in its parent's generic body, with the
/// receiver mode of that body.
pub(crate) fn callee_closure<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    inst: TraitInstId<'db>,
) -> Option<(ClosureTy<'db>, FuncParamMode)> {
    let closure = inst.self_ty(db).as_closure(db)?;
    let callable = CallableTrait::of(db, inst.def(db))?;
    if func.name(db).to_opt()?.data(db) != callable.method() {
        return None;
    }
    let receiver = if callable.mut_receiver {
        FuncParamMode::Mut
    } else {
        FuncParamMode::View
    };
    Some((closure, receiver))
}

/// The function item whose function a call of `func`, a method of the
/// callable trait instance `inst`, runs.
pub(crate) fn callee_function_item<'db>(
    db: &'db dyn HirAnalysisDb,
    func: Func<'db>,
    inst: TraitInstId<'db>,
) -> Option<TyId<'db>> {
    let item = inst.self_ty(db);
    let CallableValue::Func(..) = CallableValue::of(db, item)? else {
        return None;
    };
    let callable = CallableTrait::of(db, inst.def(db))?;
    (func.name(db).to_opt()?.data(db) == callable.method()).then_some(item)
}

/// `closure` as its parent's typed body has it, generic over the parent's
/// parameters: the type a closure body's owner names, whose instances take
/// the parent's arguments from the closure type they are called on.
pub(crate) fn closure_template_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    closure: ClosureTy<'db>,
) -> ClosureTy<'db> {
    parent_closure_ty(db, closure.def(db)).unwrap_or(closure)
}

fn parent_closure_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    def: ClosureDef<'db>,
) -> Option<ClosureTy<'db>> {
    let parent = BodyOwner::closure_parent(db, def)?;
    infer_body(db, parent)
        .1
        .expr_ty(db, def.expr)
        .as_closure(db)
}

/// The effect requirements of the body of the closure at `def`: its row,
/// numbered in order.
pub(crate) fn closure_effect_requirements<'db>(
    db: &'db dyn HirAnalysisDb,
    def: ClosureDef<'db>,
) -> Vec<EffectRequirement<'db>> {
    let Some(closure) = parent_closure_ty(db, def) else {
        return Vec::new();
    };
    closure
        .effects(db)
        .iter()
        .enumerate()
        .map(|(idx, component)| EffectRequirement {
            binding_name: component.name,
            key: component.key.clone(),
            is_mut: component.is_mut,
            binding_site: EffectParamSite::Closure(def),
            binding_idx: idx as u32,
            binding_ty: component.key_syntax,
        })
        .collect()
}
