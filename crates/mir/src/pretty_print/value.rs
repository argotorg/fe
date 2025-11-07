use std::fmt::{self, Write};

use crate::{
    db::MirDb,
    ir::{
        constant::ConstantValue, function::BodyDataStore, value::AssignableValue, Value, ValueId,
    },
};

use super::PrettyPrint;

impl PrettyPrint for ValueId {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        store: &BodyDataStore,
        w: &mut W,
    ) -> fmt::Result {
        match store.value_data(*self) {
            Value::Temporary { .. } | Value::Local(_) => write!(w, "_{}", self.index()),
            Value::Immediate { imm, .. } => write!(w, "{imm}"),
            Value::Constant { constant, .. } => {
                let const_value = constant.data(db);
                write!(w, "const ")?;
                match &const_value.value {
                    ConstantValue::Immediate(num) => write!(w, "{num}"),
                    ConstantValue::Str(s) => write!(w, r#""{s}""#),
                    ConstantValue::Bool(b) => write!(w, "{b}"),
                }
            }
            Value::Unit { .. } => write!(w, "()"),
        }
    }
}

impl PrettyPrint for &[ValueId] {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        store: &BodyDataStore,
        w: &mut W,
    ) -> fmt::Result {
        if self.is_empty() {
            return Ok(());
        }

        let arg_len = self.len();
        for arg in self.iter().take(arg_len - 1) {
            arg.pretty_print(db, store, w)?;
            write!(w, ", ")?;
        }
        let arg = self[arg_len - 1];
        arg.pretty_print(db, store, w)
    }
}

impl PrettyPrint for AssignableValue {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        store: &BodyDataStore,
        w: &mut W,
    ) -> fmt::Result {
        match self {
            Self::Value(value) => value.pretty_print(db, store, w),
            Self::Aggregate { lhs, idx } => {
                lhs.pretty_print(db, store, w)?;
                write!(w, ".<")?;
                idx.pretty_print(db, store, w)?;
                write!(w, ">")
            }

            Self::Map { lhs, key } => {
                lhs.pretty_print(db, store, w)?;
                write!(w, "{{")?;
                key.pretty_print(db, store, w)?;
                write!(w, "}}")
            }
        }
    }
}
