use std::fmt;

use hir::analysis::{
    semantic::{SemConstId, SemConstScalar, SemConstValue},
    ty::ty_def::TyId,
};

use super::{GenerationError, GenerationErrorKind};
use crate::DriverDataBase;

const MAX_DEPTH: usize = 32;
const MAX_NODES: usize = 4096;

#[derive(Debug, PartialEq, Eq)]
pub(super) enum ValueType {
    Bool,
    U256,
    Tuple(Vec<Self>),
    Array(Box<Self>, usize),
}

impl fmt::Display for ValueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => f.write_str("bool"),
            Self::U256 => f.write_str("u256"),
            Self::Tuple(elems) => {
                f.write_str("(")?;
                for (index, elem) in elems.iter().enumerate() {
                    if index > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{elem}")?;
                }
                if elems.len() == 1 {
                    f.write_str(",")?;
                }
                f.write_str(")")
            }
            Self::Array(elem, len) => write!(f, "[{elem}; {len}]"),
        }
    }
}

fn error(kind: GenerationErrorKind, message: impl Into<String>) -> GenerationError {
    GenerationError::new(kind, message)
}

fn protocol(message: impl Into<String>) -> GenerationError {
    error(GenerationErrorKind::Protocol, message)
}

fn limit(message: impl Into<String>) -> GenerationError {
    error(GenerationErrorKind::Limit, message)
}

struct NodeBudget {
    used: usize,
}

impl NodeBudget {
    fn new() -> Self {
        Self { used: 0 }
    }

    fn enter(&mut self, depth: usize) -> Result<(), GenerationError> {
        if depth > MAX_DEPTH {
            return Err(limit("generated value recursion depth limit exceeded"));
        }
        self.add(1)
    }

    fn add(&mut self, count: usize) -> Result<(), GenerationError> {
        self.used = self
            .used
            .checked_add(count)
            .filter(|used| *used <= MAX_NODES)
            .ok_or_else(|| limit("generated value node limit exceeded"))?;
        Ok(())
    }
}

fn read_type_inner<'db>(
    db: &'db DriverDataBase,
    ty: TyId<'db>,
    depth: usize,
    budget: &mut NodeBudget,
) -> Result<ValueType, GenerationError> {
    budget.enter(depth)?;
    if ty == TyId::bool(db) {
        return Ok(ValueType::Bool);
    }
    if ty == TyId::u256(db) {
        return Ok(ValueType::U256);
    }
    if ty.is_tuple(db) {
        let args = ty.generic_args(db);
        if args.len() > MAX_NODES.saturating_sub(budget.used) {
            return Err(limit("generated value node limit exceeded"));
        }
        let mut elems = Vec::with_capacity(args.len());
        for elem in args {
            elems.push(read_type_inner(db, *elem, depth + 1, budget)?);
        }
        return Ok(ValueType::Tuple(elems));
    }
    if ty.is_array(db) {
        let args = ty.generic_args(db);
        let Some(&elem_ty) = args.first().filter(|_| args.len() == 2) else {
            return Err(protocol("provider value has an invalid fixed-array type"));
        };
        let len = ty
            .array_len(db)
            .ok_or_else(|| protocol("provider value requires a fixed array length"))?;
        let elem = read_type_inner(db, elem_ty, depth + 1, budget)?;
        return Ok(ValueType::Array(Box::new(elem), len));
    }
    Err(protocol(format!(
        "provider value type `{}` is not supported",
        ty.pretty_print(db)
    )))
}

/// Reads a normalized value type into database-independent structural data.
pub(super) fn read_type(db: &DriverDataBase, ty: TyId<'_>) -> Result<ValueType, GenerationError> {
    let shape = read_type_inner(db, ty, 1, &mut NodeBudget::new())?;
    if expanded_nodes(&shape) > MAX_NODES {
        return Err(limit("generated value node limit exceeded"));
    }
    Ok(shape)
}

// Saturation records excessive inner expansions without rejecting them early:
// an enclosing zero-length array has no element values to materialize. Its
// element type is still fully validated by read_type_inner.
fn expanded_nodes(ty: &ValueType) -> usize {
    let children = match ty {
        ValueType::Bool | ValueType::U256 => 0,
        ValueType::Tuple(elems) => elems
            .iter()
            .fold(0usize, |sum, elem| sum.saturating_add(expanded_nodes(elem))),
        ValueType::Array(elem, len) => expanded_nodes(elem).saturating_mul(*len),
    };
    children.saturating_add(1)
}

struct Renderer {
    output: String,
    max_bytes: usize,
    nodes: NodeBudget,
}

impl Renderer {
    fn new(max_bytes: usize) -> Self {
        Self {
            output: String::new(),
            max_bytes,
            // Type validation and value traversal have independent limits.
            nodes: NodeBudget::new(),
        }
    }

    fn push(&mut self, text: &str) -> Result<(), GenerationError> {
        self.output
            .len()
            .checked_add(text.len())
            .filter(|len| *len <= self.max_bytes)
            .ok_or_else(|| limit("generated source byte limit exceeded"))?;
        self.output.push_str(text);
        Ok(())
    }

    fn check_type(
        &self,
        db: &DriverDataBase,
        actual: TyId<'_>,
        expected: &ValueType,
    ) -> Result<(), GenerationError> {
        let actual = read_type(db, actual)?;
        if &actual != expected {
            return Err(protocol(format!(
                "provider value type `{actual}` does not match expected `{expected}`"
            )));
        }
        Ok(())
    }

    fn value(
        &mut self,
        db: &DriverDataBase,
        value: SemConstId<'_>,
        expected: &ValueType,
        depth: usize,
    ) -> Result<(), GenerationError> {
        self.nodes.enter(depth)?;
        match (expected, value.value(db)) {
            (
                ValueType::Bool,
                SemConstValue::Scalar {
                    ty,
                    value: SemConstScalar::Bool(value),
                },
            ) => {
                self.check_type(db, ty, expected)?;
                self.push(if value { "true" } else { "false" })
            }
            (
                ValueType::U256,
                SemConstValue::Scalar {
                    ty,
                    value: SemConstScalar::Int { value },
                },
            ) if value.bits() <= 256 && !value.to_string().starts_with('-') => {
                self.check_type(db, ty, expected)?;
                self.push(&value.to_string())
            }
            (ValueType::Tuple(elems), SemConstValue::Unit) if elems.is_empty() => self.push("()"),
            (ValueType::Tuple(expected_elems), SemConstValue::Tuple { ty, elems }) => {
                self.check_type(db, ty, expected)?;
                if elems.len() != expected_elems.len() {
                    return Err(protocol("provider tuple value has the wrong arity"));
                }
                self.push("(")?;
                for (index, (elem, expected_elem)) in elems.iter().zip(expected_elems).enumerate() {
                    if index > 0 {
                        self.push(", ")?;
                    }
                    self.value(db, *elem, expected_elem, depth + 1)?;
                }
                if elems.len() == 1 {
                    self.push(",")?;
                }
                self.push(")")
            }
            (ValueType::Array(expected_elem, expected_len), SemConstValue::Array { ty, elems }) => {
                self.check_type(db, ty, expected)?;
                if elems.len() != *expected_len {
                    return Err(protocol("provider array value has the wrong length"));
                }
                self.push("[")?;
                for (index, elem) in elems.iter().enumerate() {
                    if index > 0 {
                        self.push(", ")?;
                    }
                    self.value(db, *elem, expected_elem, depth + 1)?;
                }
                self.push("]")
            }
            _ => Err(protocol(format!(
                "provider value does not match expected type `{expected}`"
            ))),
        }
    }
}

/// Renders a checked semantic constant as a bounded canonical Fe function body.
pub(super) fn body(
    db: &DriverDataBase,
    value: SemConstId<'_>,
    expected: &ValueType,
    max_bytes: usize,
) -> Result<String, GenerationError> {
    let mut renderer = Renderer::new(max_bytes);
    renderer.push("{ ")?;
    renderer.value(db, value, expected, 1)?;
    renderer.push(" }")?;
    Ok(renderer.output)
}
