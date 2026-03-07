use crate::core::hir_def::{
    Body, Const, EnumVariant, Expr, IdentId, IntegerId, LitKind, Partial, Stmt,
};
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, Zero};

use super::const_expr::{ConstExpr, ConstExprId};
use super::{
    ctfe::{CtfeConfig, CtfeInterpreter, instantiate_typed_body},
    diagnostics::{BodyDiag, FuncBodyDiag},
    trait_def::TraitInstId,
    trait_resolution::{
        TraitSolveCx,
        constraint::{collect_constraints, collect_func_def_constraints},
    },
    ty_check::{check_anon_const_body, check_const_body},
    ty_def::{InvalidCause, TyId, TyParam, TyVar},
    unify::UnificationTable,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    ty::trait_resolution::PredicateListId,
    ty::ty_def::{Kind, PrimTy, TyBase, TyData, TyVarSort},
};
use crate::hir_def::ItemKind;
use common::indexmap::IndexMap;

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
    if let ConstTyData::Hole(ty) = const_ty.data(db) {
        if let Some(expected_ty) = expected_ty {
            return const_ty.swap_ty(db, expected_ty);
        }
        let _ = ty;
        return const_ty;
    }

    if let ConstTyData::Abstract(expr, ty) = const_ty.data(db)
        && let ConstExpr::TraitConst { inst, name } = expr.data(db)
        && let Some(resolved) = const_ty_from_trait_const(
            db,
            TraitSolveCx::new(db, inst.def(db).top_mod(db).scope()),
            *inst,
            *name,
        )
    {
        let evaluated = resolved.evaluate(db, expected_ty.or(Some(*ty)));
        if matches!(
            evaluated.ty(db).invalid_cause(db),
            Some(InvalidCause::ConstEvalUnsupported { .. })
        ) && expected_ty.is_some()
        {
            return const_ty;
        }
        return evaluated;
    }

    let (body, const_ty_ty, const_def, generic_args) = match const_ty.data(db) {
        ConstTyData::UnEvaluated {
            body,
            ty,
            const_def,
            generic_args,
        } => (*body, *ty, *const_def, generic_args.clone()),
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

    fn eval_int_expr<'db>(
        db: &'db dyn HirAnalysisDb,
        body: Body<'db>,
        expr: &Expr<'db>,
        expected: Option<CheckedIntTy>,
    ) -> Result<BigInt, ConstIntError> {
        match expr {
            Expr::Block(stmts) => {
                let Some(last) = stmts.last() else {
                    return Err(ConstIntError::Overflow);
                };
                let Partial::Present(stmt) = last.data(db, body) else {
                    return Err(ConstIntError::Overflow);
                };
                let Stmt::Expr(expr_id) = stmt else {
                    return Err(ConstIntError::Overflow);
                };
                let Partial::Present(inner) = expr_id.data(db, body) else {
                    return Err(ConstIntError::Overflow);
                };
                eval_int_expr(db, body, inner, expected)
            }

            Expr::Lit(LitKind::Int(i)) => Ok(BigInt::from(i.data(db).clone())),

            Expr::Un(inner, op) => {
                let Partial::Present(inner) = inner.data(db, body) else {
                    return Err(ConstIntError::Overflow);
                };
                let value = eval_int_expr(db, body, inner, expected)?;
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

                let lhs = eval_int_expr(db, body, lhs, Some(expected))?;
                let rhs = eval_int_expr(db, body, rhs, Some(expected))?;

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
                let Some(path) = path.to_opt() else {
                    return Err(ConstIntError::NotIntExpr);
                };
                let assumptions = PredicateListId::empty_list(db);
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
                        let solve_cx = TraitSolveCx::new(db, inst.def(db).top_mod(db).scope());
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

    if let Expr::Path(path) = &expr {
        let Some(path) = path.to_opt() else {
            return ConstTyId::new(
                db,
                ConstTyData::Evaluated(
                    EvaluatedConstTy::Invalid,
                    TyId::invalid(db, InvalidCause::ParseError),
                ),
            );
        };

        let containing_func = match body.scope().parent_item(db) {
            Some(ItemKind::Func(func)) => Some(func),
            Some(ItemKind::Body(parent)) => parent.containing_func(db),
            _ => None,
        };
        let assumptions = if let Some(func) = containing_func {
            let mut preds =
                collect_func_def_constraints(db, func.into(), true).instantiate_identity();
            // Methods inside a trait implicitly assume `Self: Trait` in their bodies.
            if let Some(ItemKind::Trait(trait_)) = func.scope().parent_item(db) {
                let self_pred =
                    TraitInstId::new(db, trait_, trait_.params(db).to_vec(), IndexMap::new());
                let mut merged = preds.list(db).to_vec();
                merged.push(self_pred);
                preds = PredicateListId::new(db, merged);
            }
            preds.extend_all_bounds(db)
        } else {
            // Walk up through nested body scopes to find an enclosing item (trait/impl).
            let mut enclosing = body.scope();
            let mut parent_item = enclosing.parent_item(db);
            while let Some(ItemKind::Body(parent)) = parent_item {
                enclosing = parent.scope();
                parent_item = enclosing.parent_item(db);
            }

            match parent_item {
                Some(ItemKind::Trait(trait_)) => {
                    let self_pred =
                        TraitInstId::new(db, trait_, trait_.params(db).to_vec(), IndexMap::new());
                    PredicateListId::new(db, vec![self_pred]).extend_all_bounds(db)
                }
                Some(ItemKind::ImplTrait(impl_trait)) => collect_constraints(db, impl_trait.into())
                    .instantiate_identity()
                    .extend_all_bounds(db),
                Some(ItemKind::Impl(impl_)) => collect_constraints(db, impl_.into())
                    .instantiate_identity()
                    .extend_all_bounds(db),
                _ => PredicateListId::empty_list(db),
            }
        };
        if let Ok(resolved_path) = resolve_path(db, path, body.scope(), assumptions, true) {
            match resolved_path {
                PathRes::Ty(ty) | PathRes::TyAlias(_, ty) => {
                    if let TyData::ConstTy(const_ty) = ty.data(db) {
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
                        let expr = ConstExprId::new(db, ConstExpr::TraitConst { inst, name });
                        ConstTyId::new(db, ConstTyData::Abstract(expr, expected_ty))
                    };

                    let solve_cx =
                        TraitSolveCx::new(db, body.scope()).with_assumptions(assumptions);
                    if let Some(const_ty) = const_ty_from_trait_const(db, solve_cx, inst, name) {
                        let evaluated = const_ty.evaluate(db, expected_ty);
                        if matches!(
                            evaluated.ty(db).invalid_cause(db),
                            Some(InvalidCause::ConstEvalUnsupported { .. })
                        ) && let Some(expected_ty) = expected_ty
                        {
                            return mk_abstract(expected_ty);
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
        match eval_int_expr(db, body, &expr, expected_int_ty) {
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
            Err(_) => {
                // Genuine arithmetic error (overflow, division by zero, etc.).
                // For Block/Un/Bin, report error. For plain int literals, fall through to CTFE.
                if matches!(expr, Expr::Block(..) | Expr::Un(..) | Expr::Bin(..)) {
                    return ConstTyId::new(
                        db,
                        ConstTyData::Evaluated(
                            EvaluatedConstTy::Invalid,
                            TyId::invalid(db, InvalidCause::InvalidConstTyExpr { body }),
                        ),
                    );
                }
            }
        }
    }

    let Some(check_ty) = check_ty else {
        return ConstTyId::invalid(db, InvalidCause::InvalidConstTyExpr { body });
    };
    let expected_ty = expected_ty.expect("check_ty implies expected_ty is present");

    let (diags, typed_body) = match const_def {
        Some(const_def) => {
            let result = check_const_body(db, const_def);
            (result.0.clone(), result.1.clone())
        }
        None => {
            let result = check_anon_const_body(db, body, check_ty);
            (result.0.clone(), result.1.clone())
        }
    };

    if let Some((expected, given)) = diags.iter().find_map(|diag| match diag {
        FuncBodyDiag::Body(BodyDiag::TypeMismatch {
            expected, given, ..
        }) => Some((*expected, *given)),
        _ => None,
    }) {
        // If this const body belongs to a trait impl associated const, prefer
        // reporting the mismatch at the const definition during impl checks.
        if matches!(body.scope().parent_item(db), Some(ItemKind::ImplTrait(_))) {
            return ConstTyId::invalid(db, InvalidCause::Other);
        }

        return ConstTyId::invalid(db, InvalidCause::ConstTyMismatch { expected, given });
    }

    if !diags.is_empty() {
        return ConstTyId::invalid(
            db,
            InvalidCause::ConstEvalUnsupported {
                body,
                expr: body.expr(db),
            },
        );
    }

    let mut interp = CtfeInterpreter::new(db, CtfeConfig::default());
    let typed_body = if generic_args.is_empty() {
        typed_body
    } else {
        instantiate_typed_body(db, typed_body.clone(), &generic_args)
    };
    let evaluated = interp
        .eval_expr_in_body(body, typed_body, generic_args, body.expr(db))
        .unwrap_or_else(|cause| ConstTyId::invalid(db, cause));

    let mut table = UnificationTable::new(db);
    match check_const_ty(db, evaluated.ty(db), Some(expected_ty), &mut table) {
        Ok(ty) => evaluated.swap_ty(db, ty),
        Err(cause) => evaluated.swap_ty(db, TyId::invalid(db, cause)),
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

pub(super) fn const_ty_from_trait_const<'db>(
    db: &'db dyn HirAnalysisDb,
    solve_cx: TraitSolveCx<'db>,
    inst: TraitInstId<'db>,
    name: IdentId<'db>,
) -> Option<ConstTyId<'db>> {
    let trait_ = inst.def(db);
    let (body, generic_args) =
        crate::analysis::ty::trait_def::assoc_const_body_and_impl_args_for_trait_inst(
            db, solve_cx, inst, name,
        )
        .or_else(|| {
            trait_
                .const_(db, name)
                .and_then(|c| c.default_body(db))
                .map(|body| (body, inst.args(db).clone()))
        })?;

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
fn check_const_ty<'db>(
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
            ConstTyData::Hole(ty) => *ty,
            ConstTyData::Evaluated(_, ty) => *ty,
            ConstTyData::Abstract(_, ty) => *ty,
            ConstTyData::UnEvaluated { ty, .. } => {
                ty.unwrap_or_else(|| TyId::invalid(db, InvalidCause::Other))
            }
        }
    }

    pub(super) fn pretty_print(self, db: &dyn HirAnalysisDb) -> String {
        match &self.data(db) {
            ConstTyData::TyVar(var, _) => var.pretty_print(),
            ConstTyData::TyParam(param, ty) => {
                format!("const {}: {}", param.pretty_print(db), ty.pretty_print(db))
            }
            ConstTyData::Hole(_) => "_".to_string(),
            ConstTyData::Evaluated(resolved, _) => resolved.pretty_print(db),
            ConstTyData::Abstract(expr, _) => expr.pretty_print(db),
            ConstTyData::UnEvaluated {
                body, const_def, ..
            } => {
                if let Some(const_def) = const_def
                    && let Some(name) = const_def.name(db).to_opt()
                {
                    return format!("const {}", name.data(db));
                }

                let expr = body.expr(db);
                let Partial::Present(expr) = expr.data(db, *body) else {
                    return "const value".into();
                };

                match expr {
                    Expr::Lit(LitKind::Bool(value)) => format!("{value}"),
                    Expr::Lit(LitKind::Int(int)) => format!("{}", int.data(db)),
                    Expr::Lit(LitKind::String(string)) => format!("\"{}\"", string.data(db)),
                    Expr::Path(path) if path.is_present() => path.unwrap().pretty_print(db),
                    Expr::Block(stmts) if stmts.len() == 1 => {
                        let Partial::Present(Stmt::Expr(tail_expr)) = stmts[0].data(db, *body)
                        else {
                            return "const value".into();
                        };
                        let Partial::Present(expr) = tail_expr.data(db, *body) else {
                            return "const value".into();
                        };

                        match expr {
                            Expr::Lit(LitKind::Bool(value)) => format!("{value}"),
                            Expr::Lit(LitKind::Int(int)) => format!("{}", int.data(db)),
                            Expr::Lit(LitKind::String(string)) => {
                                format!("\"{}\"", string.data(db))
                            }
                            Expr::Path(path) if path.is_present() => path.unwrap().pretty_print(db),
                            _ => "const value".into(),
                        }
                    }
                    _ => "const value".into(),
                }
            }
        }
    }

    pub(super) fn evaluate(
        self,
        db: &'db dyn HirAnalysisDb,
        expected_ty: Option<TyId<'db>>,
    ) -> Self {
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
        let data = ConstTyData::UnEvaluated {
            body,
            ty,
            const_def,
            generic_args,
        };
        Self::new(db, data)
    }

    pub fn from_opt_body(db: &'db dyn HirAnalysisDb, body: Partial<Body<'db>>) -> Self {
        match body {
            Partial::Present(body) => Self::from_body(db, body, None, None),
            Partial::Absent => Self::invalid(db, InvalidCause::ParseError),
        }
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
        Self::new(db, ConstTyData::Hole(ty))
    }

    fn swap_ty(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        let data = match self.data(db) {
            ConstTyData::TyVar(var, _) => ConstTyData::TyVar(var.clone(), ty),
            ConstTyData::TyParam(param, _) => ConstTyData::TyParam(param.clone(), ty),
            ConstTyData::Hole(_) => ConstTyData::Hole(ty),
            ConstTyData::Evaluated(evaluated, _) => ConstTyData::Evaluated(evaluated.clone(), ty),
            ConstTyData::Abstract(expr, _) => ConstTyData::Abstract(*expr, ty),
            ConstTyData::UnEvaluated {
                body,
                const_def,
                generic_args,
                ..
            } => ConstTyData::UnEvaluated {
                body: *body,
                ty: Some(ty),
                const_def: *const_def,
                generic_args: generic_args.clone(),
            },
        };

        Self::new(db, data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstTyData<'db> {
    TyVar(TyVar<'db>, TyId<'db>),
    TyParam(TyParam<'db>, TyId<'db>),
    Hole(TyId<'db>),
    Evaluated(EvaluatedConstTy<'db>, TyId<'db>),
    Abstract(ConstExprId<'db>, TyId<'db>),
    UnEvaluated {
        body: Body<'db>,
        ty: Option<TyId<'db>>,
        const_def: Option<Const<'db>>,
        generic_args: Vec<TyId<'db>>,
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
