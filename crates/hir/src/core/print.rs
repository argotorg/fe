//! HIR-to-source pretty printing.
//!
//! # Panics
//!
//! All pretty_print methods will panic if they encounter `Partial::Absent` nodes,
//! as the HIR is expected to be complete for pretty printing.

use crate::HirDb;
use crate::hir_def::scope_graph::ScopeId;
use crate::hir_def::*;

// ============================================================================
// Helper Functions
// ============================================================================

/// Returns an indentation string for the given level.
fn indent_str(level: usize) -> String {
    "    ".repeat(level)
}

/// Unwraps a Partial<T>, panicking with context if Absent.
fn unwrap_partial<T>(partial: Partial<T>, context: &str) -> T {
    match partial {
        Partial::Present(v) => v,
        Partial::Absent => panic!("HIR pretty_print: missing required node at {}", context),
    }
}

/// Unwraps a Partial<T> reference, panicking with context if Absent.
fn unwrap_partial_ref<'a, T>(partial: &'a Partial<T>, context: &str) -> &'a T {
    match partial {
        Partial::Present(v) => v,
        Partial::Absent => panic!("HIR pretty_print: missing required node at {}", context),
    }
}

/// Indents each line of text and appends to result.
fn indent_lines(result: &mut String, text: &str, indent_level: usize) {
    for line in text.lines() {
        result.push_str(&indent_str(indent_level));
        result.push_str(line);
        result.push('\n');
    }
}

/// Indents each line of text and returns the indented string.
fn indent_text(text: &str, indent_level: usize) -> String {
    text.lines()
        .map(|line| format!("{}{}", indent_str(indent_level), line))
        .collect::<Vec<_>>()
        .join("\n")
}

fn write_attrs<'db>(
    result: &mut String,
    attrs: AttrListId<'db>,
    db: &dyn HirDb,
    indent_level: usize,
) {
    let attrs_str = attrs.pretty_print(db);
    if !attrs_str.is_empty() {
        result.push_str(&indent_text(&attrs_str, indent_level));
        result.push('\n');
    }
}

/// Formats a list of bounds separated by " + ".
fn format_bounds<'db>(bounds: &[TypeBound<'db>], db: &dyn HirDb) -> String {
    bounds
        .iter()
        .map(|b| b.pretty_print(db))
        .collect::<Vec<_>>()
        .join(" + ")
}

/// Indents all lines after the first line by the given indent level.
/// Useful for inline blocks where the first line follows other content.
fn indent_continuation(text: &str, indent_level: usize) -> String {
    let mut lines = text.lines();
    let mut result = String::new();

    if let Some(first) = lines.next() {
        result.push_str(first);
    }

    for line in lines {
        result.push('\n');
        result.push_str(&indent_str(indent_level));
        result.push_str(line);
    }

    result
}

fn format_field_def<'db>(field: &FieldDef<'db>, db: &dyn HirDb, indent_level: usize) -> String {
    let mut result = String::new();
    write_attrs(&mut result, field.attributes, db, indent_level);

    let vis = field.vis.pretty_print();
    let name = unwrap_partial(field.name, "FieldDef::name");
    let ty = unwrap_partial(field.type_ref, "FieldDef::type_ref");
    result.push_str(&indent_text(
        &format!("{}{}: {}", vis, name.data(db), ty.pretty_print(db)),
        indent_level,
    ));

    result
}

// ============================================================================
// Visibility & Modifiers
// ============================================================================

impl Visibility {
    /// Returns the visibility keyword or empty string.
    pub fn pretty_print(self) -> &'static str {
        match self {
            Visibility::Public => "pub ",
            Visibility::Private => "",
        }
    }
}

impl ItemModifier {
    /// Returns the modifier keywords.
    pub fn pretty_print(self) -> &'static str {
        match self {
            ItemModifier::Pub => "pub ",
            ItemModifier::Unsafe => "unsafe ",
            ItemModifier::PubAndUnsafe => "pub unsafe ",
            ItemModifier::None => "",
        }
    }
}

// ============================================================================
// Literals
// ============================================================================

impl<'db> LitKind<'db> {
    /// Pretty-prints a literal value.
    pub fn pretty_print(self, db: &dyn HirDb) -> String {
        match self {
            LitKind::Int(i) => i.data(db).to_string(),
            LitKind::String(s) => format!("\"{}\"", s.data(db)),
            LitKind::Bool(b) => if b { "true" } else { "false" }.to_string(),
        }
    }
}

// ============================================================================
// Attributes
// ============================================================================

impl<'db> AttrListId<'db> {
    /// Pretty-prints all attributes, each on its own line.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        self.data(db)
            .iter()
            .map(|attr| attr.pretty_print(db))
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Pretty-prints attributes with a trailing newline if non-empty.
    pub fn pretty_print_with_newline(self, db: &'db dyn HirDb) -> String {
        let s = self.pretty_print(db);
        if s.is_empty() { s } else { format!("{}\n", s) }
    }
}

impl<'db> Attr<'db> {
    /// Pretty-prints a single attribute.
    pub fn pretty_print(&self, db: &'db dyn HirDb) -> String {
        match self {
            Attr::Normal(normal) => normal.pretty_print(db),
            Attr::DocComment(doc) => format!("/// {}", doc.text.data(db)),
        }
    }
}

impl<'db> NormalAttr<'db> {
    /// Pretty-prints a normal attribute like `#[foo(bar = "baz")]`.
    pub fn pretty_print(&self, db: &'db dyn HirDb) -> String {
        let path = unwrap_partial(self.path, "Attr::path").pretty_print(db);
        if self.args.is_empty() {
            format!("#[{}]", path)
        } else {
            let args = self
                .args
                .iter()
                .map(|arg| arg.pretty_print(db))
                .collect::<Vec<_>>()
                .join(", ");
            format!("#[{}({})]", path, args)
        }
    }
}

impl<'db> AttrArg<'db> {
    /// Pretty-prints an attribute argument like `key` or `key = value`.
    pub fn pretty_print(&self, db: &'db dyn HirDb) -> String {
        let key = unwrap_partial(self.key, "AttrArg::key").pretty_print(db);
        match &self.value {
            Some(value) => format!("{} = {}", key, value.pretty_print(db)),
            None => key,
        }
    }
}

impl<'db> AttrArgValue<'db> {
    /// Pretty-prints an attribute argument value.
    pub fn pretty_print(&self, db: &'db dyn HirDb) -> String {
        match self {
            AttrArgValue::Ident(ident) => ident.data(db).to_string(),
            AttrArgValue::Lit(lit) => lit.pretty_print(db),
        }
    }
}

// ============================================================================
// Generic Parameters
// ============================================================================

impl<'db> GenericParamListId<'db> {
    /// Pretty-prints generic parameter list like `<T, U: Trait>`.
    pub fn pretty_print_params(self, db: &'db dyn HirDb) -> String {
        let params = self.data(db);
        if params.is_empty() {
            return String::new();
        }

        let params_str = params
            .iter()
            .map(|p| p.pretty_print(db))
            .collect::<Vec<_>>()
            .join(", ");
        format!("<{}>", params_str)
    }
}

impl<'db> GenericParam<'db> {
    /// Pretty-prints a generic parameter.
    pub fn pretty_print(&self, db: &'db dyn HirDb) -> String {
        match self {
            GenericParam::Type(ty) => ty.pretty_print(db),
            GenericParam::Const(c) => c.pretty_print(db),
        }
    }
}

impl<'db> TypeGenericParam<'db> {
    /// Pretty-prints a type generic parameter like `T: Trait`.
    pub fn pretty_print(&self, db: &dyn HirDb) -> String {
        let name = unwrap_partial(self.name, "TypeGenericParam::name")
            .data(db)
            .to_string();

        let mut result = name;

        if !self.bounds.is_empty() {
            result.push_str(": ");
            result.push_str(&format_bounds(&self.bounds, db));
        }

        if let Some(default) = self.default_ty {
            result.push_str(" = ");
            result.push_str(&default.pretty_print(db));
        }

        result
    }
}

impl<'db> ConstGenericParam<'db> {
    /// Pretty-prints a const generic parameter like `const N: usize`.
    pub fn pretty_print(&self, db: &'db dyn HirDb) -> String {
        let name = unwrap_partial(self.name, "ConstGenericParam::name")
            .data(db)
            .to_string();
        let ty = unwrap_partial(self.ty, "ConstGenericParam::ty").pretty_print(db);
        format!("const {}: {}", name, ty)
    }
}

impl<'db> TypeBound<'db> {
    /// Pretty-prints a type bound.
    pub fn pretty_print(&self, db: &dyn HirDb) -> String {
        match self {
            TypeBound::Trait(trait_ref) => trait_ref.pretty_print(db),
            TypeBound::Kind(kind) => {
                let kind = unwrap_partial_ref(kind, "TypeBound::Kind");
                kind.pretty_print()
            }
        }
    }
}

impl KindBound {
    /// Pretty-prints a kind bound.
    pub fn pretty_print(&self) -> String {
        match self {
            KindBound::Mono => "*".to_string(),
            KindBound::Abs(lhs, rhs) => {
                let lhs = unwrap_partial_ref(lhs, "KindBound::Abs lhs").pretty_print();
                let rhs = unwrap_partial_ref(rhs, "KindBound::Abs rhs").pretty_print();
                format!("{} -> {}", lhs, rhs)
            }
        }
    }
}

// ============================================================================
// Where Clause
// ============================================================================

impl<'db> WhereClauseId<'db> {
    /// Pretty-prints a where clause.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let predicates = self.data(db);
        if predicates.is_empty() {
            return String::new();
        }

        let preds = predicates
            .iter()
            .map(|p| p.pretty_print(db))
            .collect::<Vec<_>>()
            .join(", ");
        format!(" where {}", preds)
    }
}

impl<'db> WherePredicate<'db> {
    /// Pretty-prints a where predicate like `T: Trait`.
    pub fn pretty_print(&self, db: &dyn HirDb) -> String {
        let ty = unwrap_partial(self.ty, "WherePredicate::ty").pretty_print(db);
        format!("{}: {}", ty, format_bounds(&self.bounds, db))
    }
}

// ============================================================================
// Function Parameters
// ============================================================================

impl<'db> FuncParamListId<'db> {
    /// Pretty-prints function parameters like `(x: u32, y: i64)`.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let params = self.data(db);
        let params_str = params
            .iter()
            .map(|p| p.pretty_print(db))
            .collect::<Vec<_>>()
            .join(", ");
        format!("({})", params_str)
    }
}

impl<'db> FuncParam<'db> {
    /// Pretty-prints a function parameter.
    pub fn pretty_print(&self, db: &'db dyn HirDb) -> String {
        let mut result = String::new();
        let name = unwrap_partial(self.name, "FuncParam::name");

        // Mutability comes first
        if self.is_mut {
            result.push_str("mut ");
        }

        // Handle label if different from name
        if let Some(label) = &self.label {
            // Only print label separately if different from name
            if *label != name {
                result.push_str(&label.pretty_print(db));
                result.push(' ');
            }
        }

        // Name
        let name = name.pretty_print(db);
        result.push_str(&name);

        // Type (if not a self param with fallback)
        if !self.self_ty_fallback {
            let ty = unwrap_partial(self.ty, "FuncParam::ty").pretty_print(db);
            result.push_str(": ");
            result.push_str(&ty);
        }

        result
    }
}

// ============================================================================
// Effect Parameters
// ============================================================================

impl<'db> EffectParamListId<'db> {
    /// Pretty-prints effect parameters like `uses (Storage, Log)`.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let params = self.data(db);
        if params.is_empty() {
            return String::new();
        }

        let params_str = params
            .iter()
            .map(|p| p.pretty_print(db))
            .collect::<Vec<_>>()
            .join(", ");
        format!(" uses ({})", params_str)
    }
}

impl<'db> EffectParam<'db> {
    /// Pretty-prints an effect parameter.
    /// Formats: `mut Type`, `Type`, `name: mut Type`, or `name: Type`
    pub fn pretty_print(&self, db: &dyn HirDb) -> String {
        let mut result = String::new();

        // If we have a name binding, print it first
        if let Some(name) = self.name {
            if self.is_mut {
                result.push_str("mut ");
            }
            result.push_str(name.data(db));
            result.push_str(": ");
            let path = unwrap_partial(self.key_path, "EffectParam::key_path").pretty_print(db);
            result.push_str(&path);
        } else {
            // No name binding - shorthand form
            if self.is_mut {
                result.push_str("mut ");
            }
            let path = unwrap_partial(self.key_path, "EffectParam::key_path").pretty_print(db);
            result.push_str(&path);
        }

        result
    }
}

// ============================================================================
// Patterns
// ============================================================================

impl<'db> Pat<'db> {
    /// Pretty-prints a pattern.
    pub fn pretty_print(&self, db: &'db dyn HirDb, body: Body<'db>) -> String {
        match self {
            Pat::WildCard => "_".to_string(),
            Pat::Rest => "..".to_string(),
            Pat::Lit(lit) => {
                let lit = unwrap_partial_ref(lit, "Pat::Lit");
                lit.pretty_print(db)
            }
            Pat::Tuple(pats) => {
                let pats_str = pats
                    .iter()
                    .map(|p| {
                        let pat = unwrap_partial_ref(p.data(db, body), "Pat in tuple");
                        pat.pretty_print(db, body)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", pats_str)
            }
            Pat::Path(path, is_mut) => {
                let mut result = String::new();
                if *is_mut {
                    result.push_str("mut ");
                }
                let path = unwrap_partial_ref(path, "Pat::Path");
                result.push_str(&path.pretty_print(db));
                result
            }
            Pat::PathTuple(path, pats) => {
                let path = unwrap_partial_ref(path, "Pat::PathTuple");
                let pats_str = pats
                    .iter()
                    .map(|p| {
                        let pat = unwrap_partial_ref(p.data(db, body), "Pat in PathTuple");
                        pat.pretty_print(db, body)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", path.pretty_print(db), pats_str)
            }
            Pat::Record(path, fields) => {
                let path = unwrap_partial_ref(path, "Pat::Record");
                let fields_str = fields
                    .iter()
                    .map(|f| f.pretty_print(db, body))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} {{ {} }}", path.pretty_print(db), fields_str)
            }
            Pat::Or(lhs, rhs) => {
                let lhs_pat = unwrap_partial_ref(lhs.data(db, body), "Pat::Or lhs");
                let rhs_pat = unwrap_partial_ref(rhs.data(db, body), "Pat::Or rhs");
                format!(
                    "{} | {}",
                    lhs_pat.pretty_print(db, body),
                    rhs_pat.pretty_print(db, body)
                )
            }
        }
    }
}

impl<'db> RecordPatField<'db> {
    /// Pretty-prints a record pattern field.
    pub fn pretty_print(&self, db: &'db dyn HirDb, body: Body<'db>) -> String {
        let pat = unwrap_partial_ref(self.pat.data(db, body), "RecordPatField::pat");
        let Some(label) = self.label(db, body) else {
            return pat.pretty_print(db, body);
        };

        // Check if the pattern is just a binding with the same name as the label
        if let Pat::Path(Partial::Present(path), false) = pat
            && let Some(ident) = path.as_ident(db)
            && ident == label
        {
            // Shorthand: just `field` instead of `field: field`
            return label.data(db).to_string();
        }

        format!("{}: {}", label.data(db), pat.pretty_print(db, body))
    }
}

// ============================================================================
// Expressions
// ============================================================================

impl<'db> Expr<'db> {
    /// Pretty-prints an expression.
    pub fn pretty_print(&self, db: &'db dyn HirDb, body: Body<'db>, indent: usize) -> String {
        match self {
            Expr::Lit(lit) => lit.pretty_print(db),

            Expr::Block(stmts) => {
                if stmts.is_empty() {
                    return "{}".to_string();
                }

                let mut result = "{\n".to_string();
                for stmt_id in stmts.iter() {
                    let stmt = unwrap_partial_ref(stmt_id.data(db, body), "Stmt in block");
                    result.push_str(&indent_str(indent + 1));
                    result.push_str(&stmt.pretty_print(db, body, indent + 1));
                    result.push('\n');
                }
                result.push_str(&indent_str(indent));
                result.push('}');
                result
            }

            Expr::Bin(lhs, rhs, op) => {
                let lhs_expr = unwrap_partial_ref(lhs.data(db, body), "Bin::lhs");
                let rhs_expr = unwrap_partial_ref(rhs.data(db, body), "Bin::rhs");
                // Special handling for index operator (arr[idx] syntax)
                if matches!(op, BinOp::Index) {
                    format!(
                        "{}[{}]",
                        lhs_expr.pretty_print(db, body, indent),
                        rhs_expr.pretty_print(db, body, indent)
                    )
                } else {
                    format!(
                        "{} {} {}",
                        lhs_expr.pretty_print(db, body, indent),
                        op.pretty_print(),
                        rhs_expr.pretty_print(db, body, indent)
                    )
                }
            }

            Expr::Un(expr, op) => {
                let expr = unwrap_partial_ref(expr.data(db, body), "Un::expr");
                format!(
                    "{}{}",
                    op.pretty_print(),
                    expr.pretty_print(db, body, indent)
                )
            }

            Expr::Cast(expr, ty) => {
                let expr = unwrap_partial_ref(expr.data(db, body), "Cast::expr");
                let ty = ty
                    .to_opt()
                    .map(|ty| ty.pretty_print(db))
                    .unwrap_or_else(|| "<missing>".into());
                format!("{} as {}", expr.pretty_print(db, body, indent), ty)
            }

            Expr::Call(callee, args) => {
                let callee_expr = unwrap_partial_ref(callee.data(db, body), "Call::callee");
                let args_str = args
                    .iter()
                    .map(|arg| arg.pretty_print(db, body, indent))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{}({})",
                    callee_expr.pretty_print(db, body, indent),
                    args_str
                )
            }

            Expr::MethodCall(receiver, method, generic_args, args) => {
                let receiver_expr =
                    unwrap_partial_ref(receiver.data(db, body), "MethodCall::receiver");
                let method = unwrap_partial_ref(method, "MethodCall::method");
                let generic_args = generic_args.pretty_print(db);
                let args_str = args
                    .iter()
                    .map(|arg| arg.pretty_print(db, body, indent))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "{}.{}{}({})",
                    receiver_expr.pretty_print(db, body, indent),
                    method.data(db),
                    generic_args,
                    args_str
                )
            }

            Expr::Path(path) => {
                let path = unwrap_partial_ref(path, "Expr::Path");
                path.pretty_print(db)
            }

            Expr::RecordInit(path, fields) => {
                let path = unwrap_partial_ref(path, "RecordInit::path");
                let fields_str = fields
                    .iter()
                    .map(|f| f.pretty_print(db, body, indent))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{} {{ {} }}", path.pretty_print(db), fields_str)
            }

            Expr::Field(expr, field) => {
                let expr = unwrap_partial_ref(expr.data(db, body), "Field::expr");
                let field = unwrap_partial_ref(field, "Field::field");
                format!(
                    "{}.{}",
                    expr.pretty_print(db, body, indent),
                    field.pretty_print(db)
                )
            }

            Expr::Tuple(exprs) => {
                let exprs_str = exprs
                    .iter()
                    .map(|e| {
                        let expr = unwrap_partial_ref(e.data(db, body), "Tuple element");
                        expr.pretty_print(db, body, indent)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", exprs_str)
            }

            Expr::Array(exprs) => {
                let exprs_str = exprs
                    .iter()
                    .map(|e| {
                        let expr = unwrap_partial_ref(e.data(db, body), "Array element");
                        expr.pretty_print(db, body, indent)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", exprs_str)
            }

            Expr::ArrayRep(expr, len) => {
                let expr = unwrap_partial_ref(expr.data(db, body), "ArrayRep::expr");
                let len_body = unwrap_partial_ref(len, "ArrayRep::len");
                // For array rep, the length is a const body - we print its root expression
                let len_expr_id = len_body.expr(db);
                let len_expr =
                    unwrap_partial_ref(len_expr_id.data(db, *len_body), "ArrayRep len expr");
                format!(
                    "[{}; {}]",
                    expr.pretty_print(db, body, indent),
                    len_expr.pretty_print(db, *len_body, indent)
                )
            }

            Expr::If(cond, then_branch, else_branch) => {
                let cond_expr = unwrap_partial_ref(cond.data(db, body), "If::cond");
                let then_expr = unwrap_partial_ref(then_branch.data(db, body), "If::then");

                let mut result = format!(
                    "if {} {}",
                    cond_expr.pretty_print(db, body, indent),
                    then_expr.pretty_print(db, body, indent)
                );

                if let Some(else_id) = else_branch {
                    let else_expr = unwrap_partial_ref(else_id.data(db, body), "If::else");
                    result.push_str(" else ");
                    // else branch needs braces unless it's an if or block expression
                    if matches!(else_expr, Expr::If(..) | Expr::Block(_)) {
                        result.push_str(&else_expr.pretty_print(db, body, indent));
                    } else {
                        result.push_str(&format!(
                            "{{ {} }}",
                            else_expr.pretty_print(db, body, indent)
                        ));
                    }
                }

                result
            }

            Expr::Match(scrutinee, arms) => {
                let scrutinee_expr =
                    unwrap_partial_ref(scrutinee.data(db, body), "Match::scrutinee");
                let arms = unwrap_partial_ref(arms, "Match::arms");

                let mut result = format!(
                    "match {} {{\n",
                    scrutinee_expr.pretty_print(db, body, indent)
                );

                for arm in arms {
                    let pat = unwrap_partial_ref(arm.pat.data(db, body), "MatchArm::pat");
                    let arm_body = unwrap_partial_ref(arm.body.data(db, body), "MatchArm::body");

                    result.push_str(&indent_str(indent + 1));
                    result.push_str(&pat.pretty_print(db, body));
                    result.push_str(" => ");
                    result.push_str(&arm_body.pretty_print(db, body, indent + 1));
                    result.push_str(",\n");
                }

                result.push_str(&indent_str(indent));
                result.push('}');
                result
            }

            Expr::Assign(lhs, rhs) => {
                let lhs_expr = unwrap_partial_ref(lhs.data(db, body), "Assign::lhs");
                let rhs_expr = unwrap_partial_ref(rhs.data(db, body), "Assign::rhs");
                format!(
                    "{} = {}",
                    lhs_expr.pretty_print(db, body, indent),
                    rhs_expr.pretty_print(db, body, indent)
                )
            }

            Expr::AugAssign(lhs, rhs, op) => {
                let lhs_expr = unwrap_partial_ref(lhs.data(db, body), "AugAssign::lhs");
                let rhs_expr = unwrap_partial_ref(rhs.data(db, body), "AugAssign::rhs");
                format!(
                    "{} {}= {}",
                    lhs_expr.pretty_print(db, body, indent),
                    op.pretty_print(),
                    rhs_expr.pretty_print(db, body, indent)
                )
            }

            Expr::With(bindings, expr) => {
                let bindings_str = bindings
                    .iter()
                    .map(|b| b.pretty_print(db, body, indent))
                    .collect::<Vec<_>>()
                    .join(", ");
                let expr_ref = unwrap_partial_ref(expr.data(db, body), "With::expr");
                // With expressions always need braces around the body
                let body_str = if matches!(expr_ref, Expr::Block(_)) {
                    expr_ref.pretty_print(db, body, indent)
                } else {
                    format!("{{ {} }}", expr_ref.pretty_print(db, body, indent))
                };
                format!("with ({}) {}", bindings_str, body_str)
            }
        }
    }
}

impl<'db> CallArg<'db> {
    /// Pretty-prints a call argument.
    pub fn pretty_print(&self, db: &'db dyn HirDb, body: Body<'db>, indent: usize) -> String {
        let expr = unwrap_partial_ref(self.expr.data(db, body), "CallArg::expr");
        let expr_str = expr.pretty_print(db, body, indent);

        if let Some(label) = self.label {
            format!("{}: {}", label.data(db), expr_str)
        } else {
            expr_str
        }
    }
}

impl<'db> Field<'db> {
    /// Pretty-prints a record field in initialization.
    pub fn pretty_print(&self, db: &'db dyn HirDb, body: Body<'db>, indent: usize) -> String {
        let expr = unwrap_partial_ref(self.expr.data(db, body), "Field::expr");
        let expr_str = expr.pretty_print(db, body, indent);

        if let Some(label) = self.label {
            format!("{}: {}", label.data(db), expr_str)
        } else {
            // Shorthand: check if expr is a path matching what would be the inferred label
            expr_str
        }
    }
}

impl<'db> WithBinding<'db> {
    /// Pretty-prints a with binding.
    pub fn pretty_print(&self, db: &'db dyn HirDb, body: Body<'db>, indent: usize) -> String {
        let value = unwrap_partial_ref(self.value.data(db, body), "WithBinding::value");
        match &self.key_path {
            Some(key_partial) => {
                let key = unwrap_partial_ref(key_partial, "WithBinding::key_path");
                format!(
                    "{} = {}",
                    key.pretty_print(db),
                    value.pretty_print(db, body, indent)
                )
            }
            // Shorthand form: key is inferred from value
            None => value.pretty_print(db, body, indent),
        }
    }
}

impl<'db> FieldIndex<'db> {
    /// Pretty-prints a field index.
    pub fn pretty_print(&self, db: &dyn HirDb) -> String {
        match self {
            FieldIndex::Ident(ident) => ident.data(db).to_string(),
            FieldIndex::Index(idx) => idx.data(db).to_string(),
        }
    }
}

impl BinOp {
    /// Pretty-prints a binary operator.
    pub fn pretty_print(&self) -> &'static str {
        match self {
            BinOp::Arith(op) => op.pretty_print(),
            BinOp::Comp(op) => op.pretty_print(),
            BinOp::Logical(op) => op.pretty_print(),
            BinOp::Index => "[]", // This shouldn't be printed directly
        }
    }
}

impl ArithBinOp {
    /// Pretty-prints an arithmetic binary operator.
    pub fn pretty_print(&self) -> &'static str {
        match self {
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
        }
    }
}

impl CompBinOp {
    /// Pretty-prints a comparison binary operator.
    pub fn pretty_print(&self) -> &'static str {
        match self {
            CompBinOp::Eq => "==",
            CompBinOp::NotEq => "!=",
            CompBinOp::Lt => "<",
            CompBinOp::LtEq => "<=",
            CompBinOp::Gt => ">",
            CompBinOp::GtEq => ">=",
        }
    }
}

impl LogicalBinOp {
    /// Pretty-prints a logical binary operator.
    pub fn pretty_print(&self) -> &'static str {
        match self {
            LogicalBinOp::And => "&&",
            LogicalBinOp::Or => "||",
        }
    }
}

impl UnOp {
    /// Pretty-prints a unary operator.
    pub fn pretty_print(&self) -> &'static str {
        match self {
            UnOp::Plus => "+",
            UnOp::Minus => "-",
            UnOp::Not => "!",
            UnOp::BitNot => "~",
        }
    }
}

// ============================================================================
// Statements
// ============================================================================

impl<'db> Stmt<'db> {
    /// Pretty-prints a statement.
    pub fn pretty_print(&self, db: &'db dyn HirDb, body: Body<'db>, indent: usize) -> String {
        match self {
            Stmt::Let(pat, ty, init) => {
                let pat = unwrap_partial_ref(pat.data(db, body), "Let::pat");
                let mut result = format!("let {}", pat.pretty_print(db, body));

                if let Some(ty) = ty {
                    result.push_str(": ");
                    result.push_str(&ty.pretty_print(db));
                }

                if let Some(init) = init {
                    let init_expr = unwrap_partial_ref(init.data(db, body), "Let::init");
                    result.push_str(" = ");
                    result.push_str(&init_expr.pretty_print(db, body, indent));
                }

                result
            }

            Stmt::For(pat, iter, body_expr) => {
                let pat = unwrap_partial_ref(pat.data(db, body), "For::pat");
                let iter_expr = unwrap_partial_ref(iter.data(db, body), "For::iter");
                let body_block = unwrap_partial_ref(body_expr.data(db, body), "For::body");

                format!(
                    "for {} in {} {}",
                    pat.pretty_print(db, body),
                    iter_expr.pretty_print(db, body, indent),
                    body_block.pretty_print(db, body, indent)
                )
            }

            Stmt::While(cond, body_expr) => {
                let cond_expr = unwrap_partial_ref(cond.data(db, body), "While::cond");
                let body_block = unwrap_partial_ref(body_expr.data(db, body), "While::body");

                format!(
                    "while {} {}",
                    cond_expr.pretty_print(db, body, indent),
                    body_block.pretty_print(db, body, indent)
                )
            }

            Stmt::Continue => "continue".to_string(),
            Stmt::Break => "break".to_string(),

            Stmt::Return(expr) => {
                if let Some(expr_id) = expr {
                    let expr = unwrap_partial_ref(expr_id.data(db, body), "Return::expr");
                    format!("return {}", expr.pretty_print(db, body, indent))
                } else {
                    "return".to_string()
                }
            }

            Stmt::Expr(expr_id) => {
                let expr = unwrap_partial_ref(expr_id.data(db, body), "Stmt::Expr");
                expr.pretty_print(db, body, indent)
            }
        }
    }
}

// ============================================================================
// Body
// ============================================================================

impl<'db> Body<'db> {
    /// Pretty-prints a body (function/const body).
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let expr_id = self.expr(db);
        let expr = unwrap_partial_ref(expr_id.data(db, self), "Body::expr");
        expr.pretty_print(db, self, 0)
    }
}

// ============================================================================
// Items
// ============================================================================

impl<'db> ItemKind<'db> {
    /// Pretty-prints any item kind.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        match self {
            ItemKind::TopMod(t) => t.pretty_print(db),
            ItemKind::Mod(m) => m.pretty_print(db),
            ItemKind::Func(f) => f.pretty_print(db),
            ItemKind::Struct(s) => s.pretty_print(db),
            ItemKind::Contract(c) => c.pretty_print(db),
            ItemKind::Enum(e) => e.pretty_print(db),
            ItemKind::TypeAlias(t) => t.pretty_print(db),
            ItemKind::Impl(i) => i.pretty_print(db),
            ItemKind::Trait(t) => t.pretty_print(db),
            ItemKind::ImplTrait(i) => i.pretty_print(db),
            ItemKind::Const(c) => c.pretty_print(db),
            ItemKind::Use(u) => u.pretty_print(db),
            ItemKind::Body(_) => panic!("Body should not be printed as an item"),
        }
    }
}

impl<'db> Func<'db> {
    /// Pretty-prints a function.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let mut result = self.pretty_print_signature(db);
        if let Some(body) = self.body(db) {
            result.push(' ');
            result.push_str(&body.pretty_print(db));
        }

        result
    }

    pub fn pretty_print_signature(self, db: &'db dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // Modifiers (pub, unsafe)
        result.push_str(self.modifier(db).pretty_print());

        // fn keyword
        result.push_str("fn ");

        // Name
        let name = unwrap_partial(self.name(db), "Func::name");
        result.push_str(name.data(db));

        // Generic parameters
        result.push_str(&self.generic_params(db).pretty_print_params(db));

        // Parameters
        let params = unwrap_partial(self.params_list(db), "Func::params_list");
        result.push_str(&params.pretty_print(db));

        // Return type (comes before effects)
        if let Some(ret_ty) = self.ret_type_ref(db) {
            result.push_str(" -> ");
            result.push_str(&ret_ty.pretty_print(db));
        }

        // Effects
        result.push_str(&self.effects(db).pretty_print(db));

        // Where clause
        result.push_str(&self.where_clause(db).pretty_print(db));
        result
    }
}

impl<'db> Struct<'db> {
    /// Pretty-prints a struct.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // Visibility
        result.push_str(self.vis(db).pretty_print());

        // struct keyword
        result.push_str("struct ");

        // Name
        let name = unwrap_partial(self.name(db), "Struct::name");
        result.push_str(name.data(db));

        // Generic parameters
        result.push_str(&self.generic_params(db).pretty_print_params(db));

        // Where clause
        result.push_str(&self.where_clause(db).pretty_print(db));

        // Fields
        result.push(' ');
        result.push_str(&self.fields(db).pretty_print(db));

        result
    }
}

impl<'db> FieldDefListId<'db> {
    /// Pretty-prints a field definition list.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let fields = self.data(db);
        if fields.is_empty() {
            return "{}".to_string();
        }

        let fields_str = fields
            .iter()
            .map(|f| format_field_def(f, db, 1))
            .collect::<Vec<_>>()
            .join(",\n");

        format!("{{\n{},\n}}", fields_str)
    }
}

impl<'db> Enum<'db> {
    /// Pretty-prints an enum.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // Visibility
        result.push_str(self.vis(db).pretty_print());

        // enum keyword
        result.push_str("enum ");

        // Name
        let name = unwrap_partial(self.name(db), "Enum::name");
        result.push_str(name.data(db));

        // Generic parameters
        result.push_str(&self.generic_params(db).pretty_print_params(db));

        // Where clause
        result.push_str(&self.where_clause(db).pretty_print(db));

        // Variants
        result.push(' ');
        result.push_str(&self.variants_list(db).pretty_print(db));

        result
    }
}

impl<'db> VariantDefListId<'db> {
    /// Pretty-prints a variant definition list.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let variants = self.data(db);
        if variants.is_empty() {
            return "{}".to_string();
        }

        let variants_str = variants
            .iter()
            .map(|v| indent_text(&v.pretty_print(db), 1))
            .collect::<Vec<_>>()
            .join(",\n");

        format!("{{\n{},\n}}", variants_str)
    }
}

impl<'db> VariantDef<'db> {
    /// Pretty-prints a variant definition.
    pub fn pretty_print(&self, db: &dyn HirDb) -> String {
        let mut result = self.attributes.pretty_print_with_newline(db);
        let name = unwrap_partial(self.name, "VariantDef::name");
        result.push_str(name.data(db));

        match &self.kind {
            VariantKind::Unit => {}
            VariantKind::Tuple(tuple) => {
                let types = tuple
                    .data(db)
                    .iter()
                    .map(|t| {
                        let ty = unwrap_partial_ref(t, "VariantKind::Tuple element");
                        ty.pretty_print(db)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                result.push_str(&format!("({})", types));
            }
            VariantKind::Record(fields) => {
                result.push(' ');
                result.push_str(&fields.pretty_print(db));
            }
        }

        result
    }
}

impl<'db> Contract<'db> {
    /// Pretty-prints a contract.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // Visibility
        result.push_str(self.vis(db).pretty_print());

        // contract keyword
        result.push_str("contract ");

        // Name
        let name = unwrap_partial(self.name(db), "Contract::name");
        result.push_str(name.data(db));

        // Effects
        result.push_str(&self.effects(db).pretty_print(db));

        // Body with fields and recv handlers
        result.push_str(" {\n");

        // Fields
        for field in self.hir_fields(db).data(db) {
            let field_str = format_field_def(field, db, 1);
            result.push_str(&format!("{},\n", field_str));
        }

        // Get child items (init function, etc.)
        let scope = ScopeId::from_item(self.into());
        let scope_graph = scope.scope_graph(db);
        for item in scope_graph.child_items(scope) {
            if let ItemKind::Func(func) = item {
                result.push('\n');
                indent_lines(&mut result, &func.pretty_print(db), 1);
            }
        }

        // Recv handlers
        for recv in self.recvs(db).data(db) {
            result.push_str("\n    recv ");
            if let Some(path) = recv.msg_path {
                result.push_str(&path.pretty_print(db));
                result.push(' ');
            }
            result.push_str("{\n");

            for arm in recv.arms.data(db) {
                let pat = unwrap_partial_ref(arm.pat.data(db, arm.body), "recv arm pat");
                result.push_str("        ");
                result.push_str(&pat.pretty_print(db, arm.body));

                if let Some(ret_ty) = arm.ret_ty {
                    result.push_str(" -> ");
                    result.push_str(&ret_ty.pretty_print(db));
                }

                result.push_str(&arm.effects.pretty_print(db));
                result.push(' ');
                // Indent body continuation lines to align with the arm (2 levels)
                let body = arm.body.pretty_print(db);
                result.push_str(&indent_continuation(&body, 2));
                result.push('\n');
            }

            result.push_str("    }\n");
        }

        result.push('}');
        result
    }
}

impl<'db> Trait<'db> {
    /// Pretty-prints a trait.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // Visibility
        result.push_str(self.vis(db).pretty_print());

        // trait keyword
        result.push_str("trait ");

        // Name
        let name = unwrap_partial(self.name(db), "Trait::name");
        result.push_str(name.data(db));

        // Generic parameters
        result.push_str(&self.generic_params(db).pretty_print_params(db));

        // Super traits
        let super_traits = self.super_traits_refs(db);
        if !super_traits.is_empty() {
            result.push_str(": ");
            let traits = super_traits
                .iter()
                .map(|t| t.pretty_print(db))
                .collect::<Vec<_>>()
                .join(" + ");
            result.push_str(&traits);
        }

        // Where clause
        result.push_str(&self.where_clause(db).pretty_print(db));

        // Body
        result.push_str(" {\n");

        // Associated types
        for assoc_ty in self.types(db) {
            write_attrs(&mut result, assoc_ty.attributes, db, 1);
            result.push_str("    type ");
            let name = unwrap_partial(assoc_ty.name, "AssocTyDecl::name");
            result.push_str(name.data(db));
            if !assoc_ty.bounds.is_empty() {
                result.push_str(": ");
                let bounds = assoc_ty
                    .bounds
                    .iter()
                    .map(|b| b.pretty_print(db))
                    .collect::<Vec<_>>()
                    .join(" + ");
                result.push_str(&bounds);
            }
            if let Some(default) = assoc_ty.default {
                result.push_str(" = ");
                result.push_str(&default.pretty_print(db));
            }
            result.push_str(";\n");
        }

        // Associated consts
        for assoc_const in self.consts(db) {
            write_attrs(&mut result, assoc_const.attributes, db, 1);
            result.push_str("    const ");
            let name = unwrap_partial(assoc_const.name, "AssocConstDecl::name");
            result.push_str(name.data(db));
            result.push_str(": ");
            let ty = unwrap_partial(assoc_const.ty, "AssocConstDecl::ty");
            result.push_str(&ty.pretty_print(db));
            if let Some(default) = &assoc_const.default {
                let body = unwrap_partial_ref(default, "AssocConstDecl::default");
                result.push_str(" = ");
                result.push_str(&body.pretty_print(db));
            }
            result.push_str(";\n");
        }

        // Methods
        for method in self.methods(db) {
            result.push('\n');
            indent_lines(&mut result, &method.pretty_print(db), 1);
        }

        result.push('}');
        result
    }
}

impl<'db> Impl<'db> {
    /// Pretty-prints an impl block.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // impl keyword
        result.push_str("impl");

        // Generic parameters
        result.push_str(&self.generic_params(db).pretty_print_params(db));

        // Type
        result.push(' ');
        let ty = unwrap_partial(self.type_ref(db), "Impl::type_ref");
        result.push_str(&ty.pretty_print(db));

        // Where clause
        result.push_str(&self.where_clause(db).pretty_print(db));

        // Body
        result.push_str(" {\n");

        for func in self.funcs(db) {
            indent_lines(&mut result, &func.pretty_print(db), 1);
            result.push('\n');
        }

        result.push('}');
        result
    }
}

impl<'db> ImplTrait<'db> {
    /// Pretty-prints an impl trait block.
    pub fn pretty_print(self, db: &dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // impl keyword
        result.push_str("impl");

        // Generic parameters
        result.push_str(&self.generic_params(db).pretty_print_params(db));

        // Trait
        result.push(' ');
        let trait_ref = unwrap_partial(self.trait_ref(db), "ImplTrait::trait_ref");
        result.push_str(&trait_ref.pretty_print(db));

        // for Type
        result.push_str(" for ");
        let ty = unwrap_partial(self.type_ref(db), "ImplTrait::type_ref");
        result.push_str(&ty.pretty_print(db));

        // Where clause
        result.push_str(&self.where_clause(db).pretty_print(db));

        // Body
        result.push_str(" {\n");

        // Associated types
        for assoc_ty in self.types(db) {
            write_attrs(&mut result, assoc_ty.attributes, db, 1);
            result.push_str("    type ");
            let name = unwrap_partial(assoc_ty.name, "AssocTyDef::name");
            result.push_str(name.data(db));
            result.push_str(" = ");
            let ty = unwrap_partial(assoc_ty.type_ref, "AssocTyDef::type_ref");
            result.push_str(&ty.pretty_print(db));
            result.push('\n');
        }

        // Associated consts
        for assoc_const in self.hir_consts(db) {
            write_attrs(&mut result, assoc_const.attributes, db, 1);
            result.push_str("    const ");
            let name = unwrap_partial(assoc_const.name, "AssocConstDef::name");
            result.push_str(name.data(db));
            result.push_str(": ");
            let ty = unwrap_partial(assoc_const.ty, "AssocConstDef::ty");
            result.push_str(&ty.pretty_print(db));
            result.push_str(" = ");
            let body = unwrap_partial(assoc_const.value, "AssocConstDef::value");
            result.push_str(&body.pretty_print(db));
            result.push('\n');
        }

        // Methods
        for method in self.methods(db) {
            result.push('\n');
            indent_lines(&mut result, &method.pretty_print(db), 1);
        }

        result.push('}');
        result
    }
}

impl<'db> Const<'db> {
    /// Pretty-prints a const.
    pub fn pretty_print(self, db: &dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // Visibility
        result.push_str(self.vis(db).pretty_print());

        // const keyword
        result.push_str("const ");

        // Name
        let name = unwrap_partial(self.name(db), "Const::name");
        result.push_str(name.data(db));

        // Type
        result.push_str(": ");
        let ty = unwrap_partial(self.type_ref(db), "Const::type_ref");
        result.push_str(&ty.pretty_print(db));

        // Body
        result.push_str(" = ");
        let body = unwrap_partial(self.body(db), "Const::body");
        result.push_str(&body.pretty_print(db));

        result
    }
}

impl<'db> TypeAlias<'db> {
    /// Pretty-prints a type alias.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // Visibility
        result.push_str(self.vis(db).pretty_print());

        // type keyword
        result.push_str("type ");

        // Name
        let name = unwrap_partial(self.name(db), "TypeAlias::name");
        result.push_str(name.data(db));

        // Generic parameters
        result.push_str(&self.generic_params(db).pretty_print_params(db));

        // Type
        result.push_str(" = ");
        let ty = unwrap_partial(self.type_ref(db), "TypeAlias::type_ref");
        result.push_str(&ty.pretty_print(db));

        result
    }
}

impl<'db> Use<'db> {
    /// Pretty-prints a use statement.
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // Visibility
        result.push_str(self.vis(db).pretty_print());

        // use keyword
        result.push_str("use ");

        // Path
        let path = unwrap_partial(self.path(db), "Use::path");
        result.push_str(&path.pretty_path(db));

        // Alias
        if let Some(alias) = self.alias(db) {
            match alias {
                Partial::Present(UseAlias::Ident(ident)) => {
                    result.push_str(" as ");
                    result.push_str(ident.data(db));
                }
                Partial::Present(UseAlias::Underscore) => {
                    result.push_str(" as _");
                }
                Partial::Absent => {
                    panic!("HIR pretty_print: missing required node at Use::alias");
                }
            }
        }

        result
    }
}

impl<'db> Mod<'db> {
    /// Pretty-prints a module.
    pub fn pretty_print(self, db: &dyn HirDb) -> String {
        let mut result = String::new();

        // Attributes
        result.push_str(&self.attributes(db).pretty_print_with_newline(db));

        // Visibility
        result.push_str(self.vis(db).pretty_print());

        // mod keyword
        result.push_str("mod ");

        // Name
        let name = unwrap_partial(self.name(db), "Mod::name");
        result.push_str(name.data(db));

        // Body
        result.push_str(" {\n");

        let items: Vec<_> = self.children_non_nested(db).collect();
        print_items_with_extern_blocks(&mut result, db, &items, 1);

        result.push('}');
        result
    }
}

impl<'db> TopLevelMod<'db> {
    /// Pretty-prints a top-level module (an entire file).
    pub fn pretty_print(self, db: &'db dyn HirDb) -> String {
        let mut result = String::new();
        let items: Vec<_> = self.children_non_nested(db).collect();
        print_items_with_extern_blocks(&mut result, db, &items, 0);
        result
    }
}

/// Prints a list of items, grouping extern functions into extern blocks.
///
/// Extern functions are functions without bodies at module level.
/// They are grouped together and printed inside `extern { ... }` blocks.
/// Consecutive `use` statements are printed without blank lines between them.
fn print_items_with_extern_blocks<'db>(
    result: &mut String,
    db: &'db dyn HirDb,
    items: &[ItemKind<'db>],
    indent_level: usize,
) {
    let mut first = true;
    let mut prev_was_use = false;
    let mut extern_funcs: Vec<Func<'db>> = Vec::new();

    // Helper to flush accumulated extern functions
    let flush_extern_funcs = |result: &mut String,
                              extern_funcs: &mut Vec<Func<'db>>,
                              first: &mut bool,
                              indent_level: usize| {
        if extern_funcs.is_empty() {
            return;
        }

        if !*first {
            result.push('\n');
        }
        *first = false;

        result.push_str(&indent_str(indent_level));
        result.push_str("extern {\n");
        for func in extern_funcs.drain(..) {
            indent_lines(result, &func.pretty_print(db), indent_level + 1);
        }
        result.push_str(&indent_str(indent_level));
        result.push_str("}\n");
    };

    for item in items {
        // Check if this is an extern function (function without body at module level)
        if let ItemKind::Func(func) = item
            && func.body(db).is_none()
        {
            extern_funcs.push(*func);
            continue;
        }

        // Flush any accumulated extern functions before printing a regular item
        flush_extern_funcs(result, &mut extern_funcs, &mut first, indent_level);

        let is_use = matches!(item, ItemKind::Use(_));

        // Add blank line unless this is the first item or consecutive use statements
        if !(first || prev_was_use && is_use) {
            result.push('\n');
        }
        first = false;
        prev_was_use = is_use;

        if indent_level > 0 {
            indent_lines(result, &item.pretty_print(db), indent_level);
        } else {
            result.push_str(&item.pretty_print(db));
            result.push('\n');
        }
    }

    // Flush any remaining extern functions at the end
    flush_extern_funcs(result, &mut extern_funcs, &mut first, indent_level);
}
