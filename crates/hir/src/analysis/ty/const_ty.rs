use crate::core::hir_def::{Body, Const, Expr, IdentId, IntegerId, LitKind, Partial, Stmt};
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{One, Zero};

use super::{
    trait_def::TraitInstId,
    ty_def::{InvalidCause, TyId, TyParam, TyVar},
    unify::UnificationTable,
};
use crate::analysis::{
    HirAnalysisDb,
    name_resolution::{PathRes, resolve_path},
    ty::ty_def::{Kind, PrimTy, TyBase, TyData, TyVarSort},
    ty::{trait_def::assoc_const_body_for_trait_inst, trait_resolution::PredicateListId},
};

#[salsa::interned]
#[derive(Debug)]
pub struct ConstTyId<'db> {
    #[return_ref]
    pub data: ConstTyData<'db>,
}

#[salsa::tracked]
pub(crate) fn evaluate_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty: ConstTyId<'db>,
    expected_ty: Option<TyId<'db>>,
) -> ConstTyId<'db> {
    let (body, const_ty_ty, _const_def) = match const_ty.data(db) {
        ConstTyData::UnEvaluated {
            body,
            ty,
            const_def,
        } => (*body, *ty, *const_def),
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
                    _ => Err(ConstIntError::Overflow),
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
                        _ => Err(ConstIntError::Overflow),
                    },
                    _ => Err(ConstIntError::Overflow),
                }
            }

            Expr::Path(path) => {
                let Some(path) = path.to_opt() else {
                    return Err(ConstIntError::Overflow);
                };
                let assumptions = PredicateListId::empty_list(db);
                let resolved = resolve_path(db, path, body.scope(), assumptions, true)
                    .map_err(|_| ConstIntError::Overflow)?;

                let const_ty = match resolved {
                    PathRes::Const(const_def, declared_ty) => {
                        let body = const_def.body(db).to_opt().ok_or(ConstIntError::Overflow)?;
                        ConstTyId::from_body(db, body, Some(declared_ty), Some(const_def))
                    }
                    PathRes::TraitConst(_recv_ty, inst, name) => {
                        const_ty_from_trait_const(db, inst, name).ok_or(ConstIntError::Overflow)?
                    }
                    _ => return Err(ConstIntError::Overflow),
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
                    _ => Err(ConstIntError::Overflow),
                }
            }

            _ => Err(ConstIntError::Overflow),
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

        let assumptions = PredicateListId::empty_list(db);
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
                PathRes::TraitConst(_recv_ty, inst, name) => {
                    if let Some(const_ty) = const_ty_from_trait_const(db, inst, name) {
                        return const_ty.evaluate(db, expected_ty);
                    }
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

    let mut table = UnificationTable::new(db);
    let (resolved, ty) = match expr {
        Expr::Lit(LitKind::Bool(b)) => (
            EvaluatedConstTy::LitBool(b),
            TyId::new(db, TyData::TyBase(TyBase::bool())),
        ),

        Expr::Lit(LitKind::Int(i)) => (
            EvaluatedConstTy::LitInt(i),
            table.new_var(TyVarSort::Integral, &Kind::Star),
        ),

        Expr::Block(..) | Expr::Un(..) | Expr::Bin(..) => {
            let expected_int_ty = checked_int_ty_from_ty(db, expected_ty);
            match eval_int_expr(db, body, &expr, expected_int_ty) {
                Ok(value) => {
                    let Some(word) = bigint_to_u256_word(&value) else {
                        return ConstTyId::new(
                            db,
                            ConstTyData::Evaluated(
                                EvaluatedConstTy::Invalid,
                                TyId::invalid(db, InvalidCause::InvalidConstTyExpr { body }),
                            ),
                        );
                    };
                    (
                        EvaluatedConstTy::LitInt(IntegerId::new(db, word)),
                        table.new_var(TyVarSort::Integral, &Kind::Star),
                    )
                }
                Err(_) => {
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

        _ => {
            return ConstTyId::new(
                db,
                ConstTyData::Evaluated(
                    EvaluatedConstTy::Invalid,
                    TyId::invalid(db, InvalidCause::InvalidConstTyExpr { body }),
                ),
            );
        }
    };

    let data = match check_const_ty(db, ty, expected_ty, &mut table) {
        Ok(ty) => ConstTyData::Evaluated(resolved, ty),
        Err(err) => ConstTyData::Evaluated(resolved, TyId::invalid(db, err)),
    };

    ConstTyId::new(db, data)
}

pub(super) fn const_ty_from_trait_const<'db>(
    db: &'db dyn HirAnalysisDb,
    inst: TraitInstId<'db>,
    name: IdentId<'db>,
) -> Option<ConstTyId<'db>> {
    let body = assoc_const_body_for_trait_inst(db, inst, name).or_else(|| {
        let trait_ = inst.def(db);
        trait_.const_(db, name).and_then(|c| c.default_body(db))
    })?;

    let declared_ty = inst
        .def(db)
        .const_(db, name)
        .and_then(|v| v.ty_binder(db))
        .map(|b| b.instantiate(db, inst.args(db)));

    Some(ConstTyId::from_body(db, body, declared_ty, None))
}

// FIXME: When we add type inference, we need to use the inference engine to
// check the type of the expression instead of this function.
fn check_const_ty<'db>(
    db: &'db dyn HirAnalysisDb,
    const_ty_ty: TyId<'db>,
    expected_ty: Option<TyId<'db>>,
    table: &mut UnificationTable<'db>,
) -> Result<TyId<'db>, InvalidCause<'db>> {
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
            ConstTyData::Evaluated(_, ty) => *ty,
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
            ConstTyData::Evaluated(resolved, _) => resolved.pretty_print(db),
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
                    Expr::Lit(LitKind::Bool(value)) => format!("const {}", value),
                    Expr::Lit(LitKind::Int(int)) => format!("const {}", int.data(db)),
                    Expr::Lit(LitKind::String(string)) => format!("const \"{}\"", string.data(db)),
                    Expr::Path(path) if path.is_present() => {
                        format!("const {}", path.unwrap().pretty_print(db))
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
        let data = ConstTyData::UnEvaluated {
            body,
            ty,
            const_def,
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

    fn swap_ty(self, db: &'db dyn HirAnalysisDb, ty: TyId<'db>) -> Self {
        let data = match self.data(db) {
            ConstTyData::TyVar(var, _) => ConstTyData::TyVar(var.clone(), ty),
            ConstTyData::TyParam(param, _) => ConstTyData::TyParam(param.clone(), ty),
            ConstTyData::Evaluated(evaluated, _) => ConstTyData::Evaluated(evaluated.clone(), ty),
            ConstTyData::UnEvaluated {
                body, const_def, ..
            } => ConstTyData::UnEvaluated {
                body: *body,
                ty: Some(ty),
                const_def: *const_def,
            },
        };

        Self::new(db, data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstTyData<'db> {
    TyVar(TyVar<'db>, TyId<'db>),
    TyParam(TyParam<'db>, TyId<'db>),
    Evaluated(EvaluatedConstTy<'db>, TyId<'db>),
    UnEvaluated {
        body: Body<'db>,
        ty: Option<TyId<'db>>,
        const_def: Option<Const<'db>>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum EvaluatedConstTy<'db> {
    LitInt(IntegerId<'db>),
    LitBool(bool),
    Invalid,
}

impl EvaluatedConstTy<'_> {
    pub fn pretty_print(&self, db: &dyn HirAnalysisDb) -> String {
        match self {
            EvaluatedConstTy::LitInt(val) => {
                format!("{}", val.data(db))
            }
            EvaluatedConstTy::LitBool(val) => format!("{val}"),
            EvaluatedConstTy::Invalid => "<invalid>".to_string(),
        }
    }
}
