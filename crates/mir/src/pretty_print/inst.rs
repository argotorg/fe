use std::fmt::{self, Write};

use crate::{
    db::MirDb,
    ir::{function::BodyDataStore, inst::InstKind, InstId},
};

use super::PrettyPrint;

impl PrettyPrint for InstId {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        store: &BodyDataStore,
        w: &mut W,
    ) -> fmt::Result {
        if let Some(result) = store.inst_result(*self) {
            result.pretty_print(db, store, w)?;
            write!(w, ": ")?;

            let result_ty = result.ty(db, store);
            result_ty.pretty_print(db, store, w)?;
            write!(w, " = ")?;
        }

        match &store.inst_data(*self).kind {
            InstKind::Declare { local } => {
                write!(w, "let ")?;
                local.pretty_print(db, store, w)?;
                write!(w, ": ")?;
                store.value_ty(*local).pretty_print(db, store, w)
            }

            InstKind::Unary { op, value } => {
                write!(w, "{op}")?;
                value.pretty_print(db, store, w)
            }

            InstKind::Binary { op, lhs, rhs } => {
                lhs.pretty_print(db, store, w)?;
                write!(w, " {op} ")?;
                rhs.pretty_print(db, store, w)
            }

            InstKind::Cast { value, to, .. } => {
                value.pretty_print(db, store, w)?;
                write!(w, " as ")?;
                to.pretty_print(db, store, w)
            }

            InstKind::AggregateConstruct { ty, args } => {
                ty.pretty_print(db, store, w)?;
                write!(w, "{{")?;
                if args.is_empty() {
                    return write!(w, "}}");
                }

                let arg_len = args.len();
                for (arg_idx, arg) in args.iter().enumerate().take(arg_len - 1) {
                    write!(w, "<{arg_idx}>: ")?;
                    arg.pretty_print(db, store, w)?;
                    write!(w, ", ")?;
                }
                let arg = args[arg_len - 1];
                write!(w, "<{}>: ", arg_len - 1)?;
                arg.pretty_print(db, store, w)?;
                write!(w, "}}")
            }

            InstKind::Bind { src } => {
                write!(w, "bind ")?;
                src.pretty_print(db, store, w)
            }

            InstKind::MemCopy { src } => {
                write!(w, "memcopy ")?;
                src.pretty_print(db, store, w)
            }

            InstKind::Load { src } => {
                write!(w, "load ")?;
                src.pretty_print(db, store, w)
            }

            InstKind::AggregateAccess { value, indices } => {
                value.pretty_print(db, store, w)?;
                for index in indices {
                    write!(w, ".<")?;
                    index.pretty_print(db, store, w)?;
                    write!(w, ">")?
                }
                Ok(())
            }

            InstKind::MapAccess { value, key } => {
                value.pretty_print(db, store, w)?;
                write!(w, "{{")?;
                key.pretty_print(db, store, w)?;
                write!(w, "}}")
            }

            InstKind::Call {
                func,
                args,
                call_type,
            } => {
                let name = func.debug_name(db);
                write!(w, "{name}@{call_type}(")?;
                args.as_slice().pretty_print(db, store, w)?;
                write!(w, ")")
            }

            InstKind::Jump { dest } => {
                write!(w, "jump BB{}", dest.index())
            }

            InstKind::Branch { cond, then, else_ } => {
                write!(w, "branch ")?;
                cond.pretty_print(db, store, w)?;
                write!(w, " then: BB{} else: BB{}", then.index(), else_.index())
            }

            InstKind::Switch {
                disc,
                table,
                default,
            } => {
                write!(w, "switch ")?;
                disc.pretty_print(db, store, w)?;
                for (value, block) in table.iter() {
                    write!(w, " ")?;
                    value.pretty_print(db, store, w)?;
                    write!(w, ": BB{}", block.index())?;
                }

                if let Some(default) = default {
                    write!(w, " default: BB{}", default.index())
                } else {
                    Ok(())
                }
            }

            InstKind::Revert { arg } => {
                write!(w, "revert ")?;
                if let Some(arg) = arg {
                    arg.pretty_print(db, store, w)?;
                }
                Ok(())
            }

            InstKind::Emit { arg } => {
                write!(w, "emit ")?;
                arg.pretty_print(db, store, w)
            }

            InstKind::Return { arg } => {
                if let Some(arg) = arg {
                    write!(w, "return ")?;
                    arg.pretty_print(db, store, w)
                } else {
                    write!(w, "return")
                }
            }

            InstKind::Keccak256 { arg } => {
                write!(w, "keccak256 ")?;
                arg.pretty_print(db, store, w)
            }

            InstKind::AbiEncode { arg } => {
                write!(w, "abi_encode ")?;
                arg.pretty_print(db, store, w)
            }

            InstKind::Nop => {
                write!(w, "nop")
            }

            InstKind::Create { value, contract } => {
                write!(w, "create ")?;
                let contract_name = contract.name(db.upcast());
                write!(w, "{contract_name} ")?;
                value.pretty_print(db, store, w)
            }

            InstKind::Create2 {
                value,
                salt,
                contract,
            } => {
                write!(w, "create2 ")?;
                let contract_name = contract.name(db.upcast());
                write!(w, "{contract_name} ")?;
                value.pretty_print(db, store, w)?;
                write!(w, " ")?;
                salt.pretty_print(db, store, w)
            }

            InstKind::YulIntrinsic { op, args } => {
                write!(w, "{op}(")?;
                args.as_slice().pretty_print(db, store, w)?;
                write!(w, ")")
            }
        }
    }
}
