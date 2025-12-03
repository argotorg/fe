//! Formatting for types, paths, generics, and type-related constructs.

use crate::{ListFormatting, ListTactic, Rewrite, RewriteContext, RewriteExt, Shape, format_list};
use parser::ast::{self, GenericArgKind, GenericArgsOwner, GenericParamKind, TypeKind};

impl Rewrite for ast::GenericParamList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .trailing_separator(false)
            .with_surround("<", ">");
        format_list(self, &formatting, context, shape)
    }
}

impl Rewrite for ast::GenericParam {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        match self.kind() {
            GenericParamKind::Type(ty_param) => ty_param.rewrite(context, shape),
            GenericParamKind::Const(const_param) => const_param.rewrite(context, shape),
        }
    }
}

impl Rewrite for ast::TypeGenericParam {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let name = self
            .name()
            .map_or(String::new(), |n| context.token(&n).to_string());
        let bounds = self
            .bounds()
            .map_or(String::new(), |b| b.rewrite_or_original(context, shape));
        let default = self.default_ty().map_or(String::new(), |ty| {
            format!(" = {}", ty.rewrite_or_original(context, shape))
        });

        Some(format!("{name}{bounds}{default}"))
    }
}

impl Rewrite for ast::ConstGenericParam {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let name = self
            .name()
            .map_or(String::new(), |n| context.token(&n).to_string());
        let ty = self.ty().map_or(String::new(), |ty| {
            format!(": {}", ty.rewrite_or_original(context, shape))
        });

        Some(format!("const {name}{ty}"))
    }
}

impl Rewrite for ast::WhereClause {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let predicates: Vec<String> = self
            .into_iter()
            .map(|pred| pred.rewrite_or_original(context, shape))
            .collect();

        if predicates.is_empty() {
            return Some("where".to_string());
        }

        Some(format!("where {}", predicates.join(", ")))
    }
}

impl Rewrite for ast::WherePredicate {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let ty = self.ty()?.rewrite_or_original(context, shape);

        if let Some(bounds) = self.bounds() {
            let bounds_text = bounds.rewrite_or_original(context, shape);
            Some(format!("{ty}{bounds_text}"))
        } else {
            Some(ty)
        }
    }
}

impl Rewrite for ast::TypeBoundList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let bounds: Vec<String> = self
            .into_iter()
            .map(|bound| bound.rewrite_or_original(context, shape))
            .collect();

        if bounds.is_empty() {
            return Some(String::new());
        }

        Some(format!(": {}", bounds.join(" + ")))
    }
}

impl Rewrite for ast::TypeBound {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        if let Some(trait_bound) = self.trait_bound() {
            trait_bound.rewrite(context, shape)
        } else {
            self.kind_bound()
                .map(|kind_bound| context.snippet_trimmed(&kind_bound))
        }
    }
}

impl Rewrite for ast::TraitRef {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        self.path()?.rewrite(context, shape)
    }
}

impl Rewrite for ast::SuperTraitList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let traits: Vec<String> = self
            .into_iter()
            .map(|t| t.rewrite_or_original(context, shape))
            .collect();

        if traits.is_empty() {
            return Some(String::new());
        }

        Some(format!(": {}", traits.join(" + ")))
    }
}

impl Rewrite for ast::Type {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        match self.kind() {
            TypeKind::Ptr(ptr) => ptr.rewrite(context, shape),
            TypeKind::Path(path) => path.rewrite(context, shape),
            TypeKind::Tuple(tuple) => tuple.rewrite(context, shape),
            TypeKind::Array(array) => array.rewrite(context, shape),
            TypeKind::Never(never) => never.rewrite(context, shape),
        }
    }
}

impl Rewrite for ast::PtrType {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let inner = self.inner()?.rewrite_or_original(context, shape);
        Some(format!("*{inner}"))
    }
}

impl Rewrite for ast::PathType {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        self.path()?.rewrite(context, shape)
    }
}

impl Rewrite for ast::Path {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let segments: Vec<String> = self
            .segments()
            .map(|seg| seg.rewrite_or_original(context, shape))
            .collect();
        Some(segments.join("::"))
    }
}

impl Rewrite for ast::PathSegment {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        // Get the segment identifier (or qualified type)
        if let Some(kind) = self.kind() {
            match kind {
                ast::PathSegmentKind::QualifiedType(q) => {
                    out.push_str(&q.rewrite_or_original(context, shape));
                }
                _ => {
                    if let Some(ident) = self.ident() {
                        out.push_str(context.snippet(ident.text_range()).trim());
                    }
                }
            }
        }

        // Format generic arguments if present
        if let Some(args) = self.generic_args() {
            out.push_str(&args.rewrite_or_original(context, shape));
        }

        Some(out)
    }
}

impl Rewrite for ast::QualifiedType {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let ty = self.ty()?.rewrite_or_original(context, shape);
        let trait_path = self.trait_qualifier()?.rewrite_or_original(context, shape);
        Some(format!("<{ty} as {trait_path}>"))
    }
}

impl Rewrite for ast::GenericArgList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .trailing_separator(false)
            .with_surround("<", ">");
        format_list(self, &formatting, context, shape)
    }
}

impl Rewrite for ast::GenericArg {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        match self.kind() {
            GenericArgKind::Type(ty_arg) => ty_arg.rewrite(context, shape),
            GenericArgKind::Const(const_arg) => const_arg.rewrite(context, shape),
            GenericArgKind::AssocType(assoc_arg) => assoc_arg.rewrite(context, shape),
        }
    }
}

impl Rewrite for ast::TypeGenericArg {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        self.ty()?.rewrite(context, shape)
    }
}

impl Rewrite for ast::ConstGenericArg {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        self.expr()?.rewrite(context, shape)
    }
}

impl Rewrite for ast::AssocTypeGenericArg {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let name = context.snippet(self.name()?.text_range()).trim();
        let ty = self.ty()?.rewrite_or_original(context, shape);
        Some(format!("{name} = {ty}"))
    }
}

impl Rewrite for ast::TupleType {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");
        format_list(self.elem_tys(), &formatting, context, shape)
    }
}

impl Rewrite for ast::ArrayType {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let elem_ty = self.elem_ty()?.rewrite_or_original(context, shape);
        let len = self.len()?.rewrite_or_original(context, shape);

        Some(format!("[{elem_ty}; {len}]"))
    }
}

impl Rewrite for ast::NeverType {
    fn rewrite(&self, _context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        Some("!".to_string())
    }
}
