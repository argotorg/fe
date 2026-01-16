//! MIR pretty-printing utilities.

use std::collections::HashSet;

use cranelift_entity::EntityRef;
use hir::analysis::HirAnalysisDb;
use hir::hir_def::expr::{ArithBinOp, BinOp, CompBinOp, LogicalBinOp, UnOp};
use hir::projection::{IndexSource, Projection};

use crate::ir::{AddressSpaceKind, IntrinsicOp, Place, Rvalue, TerminatingCall};
use crate::{
    CallOrigin, LocalId, MirBody, MirFunction, MirInst, MirModule, Terminator, ValueId, ValueOrigin,
};

/// Format an entire MIR module as a human-readable string.
pub fn format_module(db: &dyn HirAnalysisDb, module: &MirModule<'_>) -> String {
    let mut out = String::new();
    for func in &module.functions {
        out.push_str(&format_function(db, func));
        out.push('\n');
    }
    out
}

/// Format a single MIR function.
pub fn format_function(db: &dyn HirAnalysisDb, func: &MirFunction<'_>) -> String {
    let mut out = String::new();

    // Function signature with parameters
    let params: Vec<String> = func
        .body
        .param_locals
        .iter()
        .chain(func.body.effect_param_locals.iter())
        .map(|local| format_local_decl(db, &func.body, *local))
        .collect();
    let params_str = params.join(", ");
    let return_ty = func.ret_ty.pretty_print(db);
    out.push_str(&format!(
        "fn {}({}) -> {}:\n",
        func.symbol_name, params_str, return_ty
    ));

    let mut defined_locals: HashSet<LocalId> = func
        .body
        .param_locals
        .iter()
        .chain(func.body.effect_param_locals.iter())
        .copied()
        .collect();

    // Basic blocks
    for (idx, block) in func.body.blocks.iter().enumerate() {
        out.push_str(&format!("  bb{}:\n", idx));
        for inst in &block.insts {
            out.push_str(&format!(
                "    {}\n",
                format_inst_with_local_types(db, &func.body, inst, &mut defined_locals)
            ));
        }
        out.push_str(&format!(
            "    {}\n",
            format_terminator(&func.body, &block.terminator)
        ));
    }

    out
}

/// Format a local variable declaration.
fn format_local_decl(db: &dyn HirAnalysisDb, body: &MirBody<'_>, local: LocalId) -> String {
    let data = body.local(local);
    let ty = data.ty.pretty_print(db);
    format!("v{}: {}", local.0, ty)
}

/// Format a single MIR instruction.
pub fn format_inst(_db: &dyn HirAnalysisDb, body: &MirBody<'_>, inst: &MirInst<'_>) -> String {
    match inst {
        MirInst::Assign { dest, rvalue, .. } => {
            let rendered = format_rvalue(body, rvalue);
            if let Some(dest) = dest {
                format!("{} = {}", format_local(*dest), rendered)
            } else {
                format!("eval {rendered}")
            }
        }
        MirInst::BindValue { value } => format!("bind {}", format_value(body, *value)),
        MirInst::Store { place, value } => {
            format!(
                "store {} = {}",
                format_place(body, place),
                format_value(body, *value)
            )
        }
        MirInst::InitAggregate { place, inits } => {
            let inits: Vec<String> = inits
                .iter()
                .map(|(path, value)| {
                    let proj = format_projection_path(body, path.iter());
                    format!("{} = {}", proj, format_value(body, *value))
                })
                .collect();
            format!(
                "init {} {{ {} }}",
                format_place(body, place),
                inits.join(", ")
            )
        }
        MirInst::SetDiscriminant { place, variant } => {
            format!("set_discr {} = {}", format_place(body, place), variant.idx)
        }
    }
}

/// Format a MIR terminator.
pub fn format_terminator(body: &MirBody<'_>, term: &Terminator<'_>) -> String {
    match term {
        Terminator::Return(Some(val)) => format!("ret {}", format_value(body, *val)),
        Terminator::Return(None) => "ret".into(),
        Terminator::TerminatingCall(call) => match call {
            TerminatingCall::Call(call) => {
                let rendered = format_call(body, call);
                format!("terminate {rendered}")
            }
            TerminatingCall::Intrinsic { op, args } => {
                let args: Vec<String> = args.iter().map(|arg| format_value(body, *arg)).collect();
                format!("terminate {}({})", format_intrinsic(*op), args.join(", "))
            }
        },
        Terminator::Goto { target } => format!("jmp bb{}", target.index()),
        Terminator::Branch {
            cond,
            then_bb,
            else_bb,
        } => format!(
            "br {} bb{} bb{}",
            format_value(body, *cond),
            then_bb.index(),
            else_bb.index()
        ),
        Terminator::Switch {
            discr,
            targets,
            default,
        } => {
            let arms: Vec<String> = targets
                .iter()
                .map(|t| format!("{} => bb{}", t.value, t.block.index()))
                .collect();
            format!(
                "switch {} [{}] else bb{}",
                format_value(body, *discr),
                arms.join(", "),
                default.index()
            )
        }
        Terminator::Unreachable => "unreachable".into(),
    }
}

fn format_call(body: &MirBody<'_>, call: &CallOrigin<'_>) -> String {
    let name = call.resolved_name.as_deref().unwrap_or("<unresolved>");
    let args: Vec<String> = call
        .args
        .iter()
        .chain(call.effect_args.iter())
        .map(|arg| format_value(body, *arg))
        .collect();
    format!("{}({})", name, args.join(", "))
}

fn format_rvalue(body: &MirBody<'_>, rvalue: &Rvalue<'_>) -> String {
    match rvalue {
        Rvalue::ZeroInit => "0".into(),
        Rvalue::Value(value) => format_value(body, *value),
        Rvalue::Call(call) => format_call(body, call),
        Rvalue::Intrinsic { op, args } => {
            let args: Vec<String> = args.iter().map(|arg| format_value(body, *arg)).collect();
            format!("{}({})", format_intrinsic(*op), args.join(", "))
        }
        Rvalue::Load { place } => format!("load {}", format_place(body, place)),
        Rvalue::Alloc { address_space } => {
            let space = match address_space {
                AddressSpaceKind::Memory => "mem",
                AddressSpaceKind::Calldata => "calldata",
                AddressSpaceKind::Storage => "stor",
                AddressSpaceKind::TransientStorage => "tstor",
            };
            format!("alloc {space}")
        }
    }
}

fn format_value(body: &MirBody<'_>, val: ValueId) -> String {
    let mut stack = HashSet::new();
    format_value_inner(body, val, &mut stack, 0)
}

fn format_value_id(val: ValueId) -> String {
    format!("v{}", val.0)
}

fn format_value_inner(
    body: &MirBody<'_>,
    val: ValueId,
    stack: &mut HashSet<ValueId>,
    depth: usize,
) -> String {
    const MAX_DEPTH: usize = 12;
    if depth >= MAX_DEPTH {
        return format_value_id(val);
    }
    if !stack.insert(val) {
        return format_value_id(val);
    }

    let rendered = match &body.value(val).origin {
        ValueOrigin::Expr(expr) => format!("expr{}", expr.index()),
        ValueOrigin::ControlFlowResult { expr } => format!("cf_result(expr{})", expr.index()),
        ValueOrigin::Unit => "()".into(),
        ValueOrigin::Unary { op, inner } => {
            let inner = format_value_inner(body, *inner, stack, depth + 1);
            match op {
                UnOp::Plus => format!("(+{inner})"),
                UnOp::Minus => format!("(-{inner})"),
                UnOp::Not => format!("(!{inner})"),
                UnOp::BitNot => format!("(~{inner})"),
            }
        }
        ValueOrigin::Binary { op, lhs, rhs } => {
            let lhs = format_value_inner(body, *lhs, stack, depth + 1);
            let rhs = format_value_inner(body, *rhs, stack, depth + 1);
            match op {
                BinOp::Index => format!("({lhs}[{rhs}])"),
                _ => format!("({lhs} {} {rhs})", format_bin_op(*op)),
            }
        }
        ValueOrigin::Synthetic(value) => match value {
            crate::ir::SyntheticValue::Int(int) => int.to_string(),
            crate::ir::SyntheticValue::Bool(flag) => {
                if *flag {
                    "true".into()
                } else {
                    "false".into()
                }
            }
            crate::ir::SyntheticValue::Bytes(bytes) => format_bytes(bytes),
        },
        ValueOrigin::Local(local) => format_local(*local),
        ValueOrigin::FuncItem(root) => format!(
            "func_item({})",
            root.symbol.as_deref().unwrap_or("<unresolved>")
        ),
        ValueOrigin::FieldPtr(field_ptr) => {
            let base = format_value_inner(body, field_ptr.base, stack, depth + 1);
            if field_ptr.offset_bytes == 0 {
                base
            } else {
                format!("({base} + {})", field_ptr.offset_bytes)
            }
        }
        ValueOrigin::PlaceRef(place) => format!("&{}", format_place(body, place)),
        ValueOrigin::TransparentCast { value } => {
            format_value_inner(body, *value, stack, depth + 1)
        }
    };

    stack.remove(&val);
    rendered
}

fn format_local(local: LocalId) -> String {
    format!("v{}", local.0)
}

fn format_place(body: &MirBody<'_>, place: &Place<'_>) -> String {
    let space = match body.place_address_space(place) {
        AddressSpaceKind::Memory => "mem",
        AddressSpaceKind::Calldata => "calldata",
        AddressSpaceKind::Storage => "stor",
        AddressSpaceKind::TransientStorage => "tstor",
    };
    let base = format_value(body, place.base);
    let proj = format_projection_path(body, place.projection.iter());
    if proj.is_empty() {
        format!("{}[{}]", space, base)
    } else {
        format!("{}[{}]{}", space, base, proj)
    }
}

fn format_inst_with_local_types(
    db: &dyn HirAnalysisDb,
    body: &MirBody<'_>,
    inst: &MirInst<'_>,
    defined_locals: &mut HashSet<LocalId>,
) -> String {
    match inst {
        MirInst::Assign { dest, rvalue, .. } => {
            let rendered = format_rvalue(body, rvalue);
            if let Some(dest) = dest {
                let local = *dest;
                let local_name = format_local(local);
                if defined_locals.insert(local) {
                    let ty = body.local(local).ty.pretty_print(db);
                    format!("{local_name}: {ty} = {rendered}")
                } else {
                    format!("{local_name} = {rendered}")
                }
            } else {
                format!("eval {rendered}")
            }
        }
        _ => format_inst(db, body, inst),
    }
}

fn format_projection_path<'a>(
    body: &MirBody<'_>,
    projections: impl Iterator<
        Item = &'a Projection<
            hir::analysis::ty::ty_def::TyId<'a>,
            hir::hir_def::EnumVariant<'a>,
            ValueId,
        >,
    >,
) -> String {
    let mut out = String::new();
    for proj in projections {
        match proj {
            Projection::Field(idx) => out.push_str(&format!(".{}", idx)),
            Projection::VariantField {
                variant, field_idx, ..
            } => out.push_str(&format!(".{}[{}]", variant.idx, field_idx)),
            Projection::Discriminant => out.push_str(".discr"),
            Projection::Index(IndexSource::Constant(idx)) => out.push_str(&format!("[{}]", idx)),
            Projection::Index(IndexSource::Dynamic(val)) => {
                out.push_str(&format!("[{}]", format_value(body, *val)))
            }
            Projection::Deref => out.push_str(".*"),
        }
    }
    out
}

fn format_bin_op(op: BinOp) -> &'static str {
    match op {
        BinOp::Arith(arith) => match arith {
            ArithBinOp::Add => "+",
            ArithBinOp::Sub => "-",
            ArithBinOp::Mul => "*",
            ArithBinOp::Div => "/",
            ArithBinOp::Rem => "%",
            ArithBinOp::Pow => "**",
            ArithBinOp::LShift => "<<",
            ArithBinOp::RShift => ">>",
            ArithBinOp::BitAnd => "&",
            ArithBinOp::BitOr => "|",
            ArithBinOp::BitXor => "^",
            ArithBinOp::Range => "..",
        },
        BinOp::Comp(comp) => match comp {
            CompBinOp::Eq => "==",
            CompBinOp::NotEq => "!=",
            CompBinOp::Lt => "<",
            CompBinOp::LtEq => "<=",
            CompBinOp::Gt => ">",
            CompBinOp::GtEq => ">=",
        },
        BinOp::Logical(logical) => match logical {
            LogicalBinOp::And => "&&",
            LogicalBinOp::Or => "||",
        },
        BinOp::Index => "[]",
    }
}

fn format_bytes(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(2 + bytes.len() * 2);
    out.push_str("0x");
    for byte in bytes {
        out.push_str(&format!("{:02x}", byte));
    }
    out
}

fn format_intrinsic(op: IntrinsicOp) -> &'static str {
    match op {
        IntrinsicOp::Mload => "mload",
        IntrinsicOp::Calldataload => "calldataload",
        IntrinsicOp::Calldatacopy => "calldatacopy",
        IntrinsicOp::Calldatasize => "calldatasize",
        IntrinsicOp::Returndatacopy => "returndatacopy",
        IntrinsicOp::Returndatasize => "returndatasize",
        IntrinsicOp::AddrOf => "addr_of",
        IntrinsicOp::Mstore => "mstore",
        IntrinsicOp::Mstore8 => "mstore8",
        IntrinsicOp::Sload => "sload",
        IntrinsicOp::Sstore => "sstore",
        IntrinsicOp::ReturnData => "return_data",
        IntrinsicOp::Codecopy => "codecopy",
        IntrinsicOp::Codesize => "codesize",
        IntrinsicOp::CodeRegionOffset => "code_region_offset",
        IntrinsicOp::CodeRegionLen => "code_region_len",
        IntrinsicOp::Keccak => "keccak256",
        IntrinsicOp::Revert => "revert",
        IntrinsicOp::Caller => "caller",
    }
}
