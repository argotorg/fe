use fe_common::diagnostics::Label;
use fe_common::Span;

use crate::context::{AnalyzerContext, DiagnosticVoucher};
use crate::display::Displayable;
use crate::errors::BinaryOperationError;
use crate::namespace::types::TypeId;
use crate::AnalyzerDb;
use std::fmt::Display;

fn type_label(db: &dyn AnalyzerDb, span: Span, typ: TypeId) -> Label {
    Label::primary(span, format!("this has type `{}`", typ.display(db)))
}

pub fn add_bin_operations_errors(
    context: &dyn AnalyzerContext,
    op: &dyn Display,
    lspan: Span,
    ltype: TypeId,
    rspan: Span,
    rtype: TypeId,
    error: BinaryOperationError,
) -> DiagnosticVoucher {
    let db = context.db();
    let ltype = ltype.deref(db);
    let rtype = rtype.deref(db);

    match error {
        BinaryOperationError::NotEqualAndUnsigned => context.fancy_error(
            &format!("`{}` operand types must be equal and unsigned", op),
            vec![type_label(db, lspan, ltype), type_label(db, rspan, rtype)],
            vec![],
        ),
        BinaryOperationError::RightIsSigned => context.fancy_error(
            &format!(
                "The right hand side of the `{}` operation must be unsigned",
                op
            ),
            vec![Label::primary(
                rspan,
                format!("this has signed type `{}`", rtype.display(db)),
            )],
            vec![],
        ),
        BinaryOperationError::RightTooLarge => context.fancy_error(
            &format!("incompatible `{}` operand types", op),
            vec![type_label(db, lspan, ltype), type_label(db, rspan, rtype)],
            vec![format!(
                "The type of the right hand side cannot be larger than the left (`{}`)",
                ltype.display(db)
            )],
        ),
        BinaryOperationError::TypesNotCompatible => context.fancy_error(
            &format!("`{}` operand types are not compatible", op),
            vec![type_label(db, lspan, ltype), type_label(db, rspan, rtype)],
            vec![],
        ),
        BinaryOperationError::TypesNotNumeric => context.fancy_error(
            &format!("`{}` operands must be numeric", op),
            vec![type_label(db, lspan, ltype), type_label(db, rspan, rtype)],
            vec![],
        ),
    }
}
