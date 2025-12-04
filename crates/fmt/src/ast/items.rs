//! Formatting for top-level items (functions, structs, enums, traits, etc.)

use crate::{
    Indent, ListFormatting, ListTactic, Rewrite, RewriteContext, RewriteExt, Shape, format_list,
};
use parser::TextRange;
use parser::ast::{self, ItemKind, ItemModifierOwner, TraitItemKind, prelude::AstNode};

use super::{
    push_indent, rewrite_block_items, write_attrs, write_generics, write_item_modifier,
    write_where_clause,
};

impl Rewrite for ast::Root {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        use parser::syntax_node::NodeOrToken;

        let mut out = String::new();
        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    if let Some(item_list) = ast::ItemList::cast(node.clone()) {
                        out.push_str(&item_list.rewrite_or_original(context, shape));
                    } else {
                        out.push_str(context.snippet(node.text_range()));
                    }
                }
                NodeOrToken::Token(token) => {
                    out.push_str(context.snippet(token.text_range()));
                }
            }
        }
        Some(out)
    }
}

impl Rewrite for ast::ItemList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        use parser::syntax_node::NodeOrToken;

        let mut out = String::new();
        for child in self.syntax().children_with_tokens() {
            match child {
                NodeOrToken::Node(node) => {
                    if let Some(item) = ast::Item::cast(node.clone()) {
                        out.push_str(&item.rewrite_or_original(context, shape));
                    } else {
                        out.push_str(context.snippet(node.text_range()));
                    }
                }
                NodeOrToken::Token(token) => {
                    out.push_str(context.snippet(token.text_range()));
                }
            }
        }
        Some(out)
    }
}

impl Rewrite for ast::Item {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        match self.kind()? {
            ItemKind::Mod(mod_) => mod_.rewrite(context, _shape),
            ItemKind::Func(func) => func.rewrite(context, _shape),
            ItemKind::Struct(struct_) => struct_.rewrite(context, _shape),
            ItemKind::Contract(contract) => contract.rewrite(context, _shape),
            ItemKind::Enum(enum_) => enum_.rewrite(context, _shape),
            ItemKind::TypeAlias(type_alias) => type_alias.rewrite(context, _shape),
            ItemKind::Impl(impl_) => impl_.rewrite(context, _shape),
            ItemKind::Trait(trait_) => trait_.rewrite(context, _shape),
            ItemKind::ImplTrait(impl_trait) => impl_trait.rewrite(context, _shape),
            ItemKind::Const(const_) => const_.rewrite(context, _shape),
            ItemKind::Use(use_) => use_.rewrite(context, _shape),
            ItemKind::Extern(extern_) => extern_.rewrite(context, _shape),
        }
    }
}

impl Rewrite for ast::FuncSignature {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let outer_indent = shape.indent.indent_width();
        let indent_width = context.config.indent_width;
        let clause_offset = context.config.clause_indent;

        let mut prefix = format!("fn {}", context.token(&self.name()?));
        write_generics(self, context, shape, &mut prefix);

        let params_h = match self.params() {
            Some(p) => {
                let fmt = ListFormatting::new(shape)
                    .tactic(ListTactic::Horizontal)
                    .with_surround("(", ")");
                format_list(p, &fmt, context, shape)?
            }
            None => "()".to_string(),
        };

        let ret = self
            .ret_ty()
            .map(|ty| format!(" -> {}", ty.rewrite_or_original(context, shape)))
            .unwrap_or_default();

        let uses = self
            .uses_clause()
            .map(|u| format!(" {}", u.rewrite_or_original(context, shape)))
            .unwrap_or_default();

        let where_text = self
            .where_clause()
            .map(|w| format!(" {}", w.rewrite_or_original(context, shape)))
            .unwrap_or_default();

        // Try single line
        let parts = [&prefix, &params_h, &ret, &uses, &where_text];
        let all_single_line = parts.iter().all(|s| !s.contains('\n'));
        let total_len: usize = outer_indent + parts.iter().map(|s| s.len()).sum::<usize>();

        if all_single_line && total_len <= shape.width {
            return Some(format!("{prefix}{params_h}{ret}{uses}{where_text}"));
        }

        // Build with line breaks as needed
        let mut out = prefix;
        let mut line_len = last_line_width(&out, outer_indent);

        // Params: try horizontal, fall back to vertical
        if !params_h.contains('\n') && line_len + params_h.len() <= shape.width {
            out.push_str(&params_h);
        } else if let Some(p) = self.params() {
            let fmt = ListFormatting::new(shape)
                .tactic(ListTactic::Vertical)
                .with_surround("(", ")");
            out.push_str(&format_list(p, &fmt, context, shape)?);
        } else {
            out.push_str("()");
        }
        line_len = last_line_width(&out, outer_indent);

        // Return type
        if !ret.is_empty() {
            if !ret.contains('\n') && line_len + ret.len() <= shape.width {
                out.push_str(&ret);
            } else {
                out.push('\n');
                push_indent(&mut out, outer_indent);
                out.push_str(ret.trim_start());
            }
            line_len = last_line_width(&out, outer_indent);
        }

        // Uses clause
        if let Some(u) = self.uses_clause() {
            if !uses.contains('\n') && line_len + uses.len() <= shape.width {
                out.push_str(&uses);
            } else {
                out.push('\n');
                let uses_indent = outer_indent + clause_offset;
                let uses_shape = Shape::with_width(
                    shape.width.saturating_sub(uses_indent),
                    Indent::from_block(uses_indent),
                );
                push_indent(&mut out, uses_indent);
                out.push_str(u.rewrite(context, uses_shape)?.trim_start());
            }
            line_len = last_line_width(&out, outer_indent);
        }

        // Where clause: inline -> compact (own line) -> vertical (each pred on line)
        if let Some(where_clause) = self.where_clause() {
            let where_trimmed = where_text.trim_start();
            let clause_indent = outer_indent + clause_offset;

            if !where_text.contains('\n') && line_len + where_text.len() <= shape.width {
                out.push_str(&where_text);
            } else if clause_indent + where_trimmed.len() <= shape.width {
                out.push('\n');
                push_indent(&mut out, clause_indent);
                out.push_str(where_trimmed);
            } else {
                out.push('\n');
                push_indent(&mut out, clause_indent);
                out.push_str("where\n");
                for pred in where_clause {
                    push_indent(&mut out, outer_indent + indent_width);
                    out.push_str(&pred.rewrite_or_original(context, shape));
                    out.push_str(",\n");
                }
                out.pop();
            }
        }

        Some(out)
    }
}

impl Rewrite for ast::Func {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let sig = self.sig();
        let body_opt = self.body();

        let suffix = if let Some(body) = &body_opt {
            let func_range = self.syntax().text_range();
            let body_range = body.syntax().text_range();
            let suffix_range = TextRange::new(body_range.end(), func_range.end());
            context.snippet(suffix_range).to_string()
        } else {
            String::new()
        };

        let mut out = String::new();

        write_attrs(self, context, &mut out);

        let mut modifier_prefix = String::new();
        write_item_modifier(self, &mut modifier_prefix);

        let sig_shape = Shape::with_width(
            shape.width.saturating_sub(modifier_prefix.len()),
            shape.indent,
        );
        let signature = sig.rewrite(context, sig_shape)?;

        out.push_str(&modifier_prefix);
        out.push_str(&signature);

        if let Some(body) = &body_opt {
            let formatted_body = body.rewrite(context, shape)?;
            let signature_multiline = signature.contains('\n');

            if sig.where_clause().is_some() && signature_multiline {
                out.push('\n');
                push_indent(&mut out, shape.indent.indent_width());
                out.push_str(&formatted_body);
            } else {
                out.push(' ');
                out.push_str(&formatted_body);
            }

            out.push_str(&suffix);
        }

        Some(out)
    }
}

fn last_line_width(text: &str, first_line_indent: usize) -> usize {
    if let Some(idx) = text.rfind('\n') {
        text.len().saturating_sub(idx + 1)
    } else {
        first_line_indent + text.len()
    }
}

impl Rewrite for ast::FuncParamList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("(", ")");
        format_list(self, &formatting, context, shape)
    }
}

impl Rewrite for ast::FuncParam {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        if self.mut_token().is_some() {
            out.push_str("mut ");
        }

        let label = self.label();
        let name = self.name();

        if let (Some(label), Some(name_ref)) = (&label, &name)
            && label.syntax().text_range() != name_ref.syntax().text_range()
        {
            out.push_str(context.snippet(label.syntax().text_range()).trim());
            out.push(' ');
        }

        if let Some(name) = name {
            out.push_str(context.snippet(name.syntax().text_range()).trim());
        }

        if let Some(ty) = self.ty() {
            if !out.is_empty() {
                out.push_str(": ");
            }
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        Some(out)
    }
}

impl Rewrite for ast::Struct {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("struct ");
        if let Some(name) = self.name() {
            out.push_str(context.token(&name));
        }

        write_generics(self, context, shape, &mut out);
        write_where_clause(self, context, shape, &mut out);

        if let Some(fields) = self.fields() {
            out.push(' ');
            out.push_str(&fields.rewrite_or_original(context, shape));
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::RecordFieldDefList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .horizontal_padding(true)
            .with_surround("{", "}");
        format_list(self, &formatting, context, shape)
    }
}

impl Rewrite for ast::RecordFieldDef {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);

        if self.pub_kw().is_some() {
            out.push_str("pub ");
        }

        if let Some(name) = self.name() {
            out.push_str(context.token(&name));
        }

        if let Some(ty) = self.ty() {
            out.push_str(": ");
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        Some(out)
    }
}

impl Rewrite for ast::Contract {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("contract ");
        if let Some(name) = self.name() {
            out.push_str(context.token(&name));
        }

        if let Some(fields) = self.fields() {
            out.push(' ');
            out.push_str(&fields.rewrite_or_original(context, shape));
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::Enum {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("enum ");
        if let Some(name) = self.name() {
            out.push_str(context.token(&name));
        }

        write_generics(self, context, shape, &mut out);
        write_where_clause(self, context, shape, &mut out);

        if let Some(variants) = self.variants() {
            out.push(' ');
            out.push_str(&variants.rewrite_or_original(context, shape));
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::VariantDefList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Vertical)
            .with_surround("{", "}");
        format_list(self, &formatting, context, shape)
    }
}

impl Rewrite for ast::VariantDef {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);

        if let Some(name) = self.name() {
            out.push_str(context.token(&name));
        }

        match self.kind() {
            ast::VariantKind::Unit => {}
            ast::VariantKind::Tuple(tuple_type) => {
                out.push_str(&tuple_type.rewrite_or_original(context, shape));
            }
            ast::VariantKind::Record(fields) => {
                out.push(' ');
                out.push_str(&fields.rewrite_or_original(context, shape));
            }
        }

        Some(out)
    }
}

impl Rewrite for ast::Trait {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("trait ");
        if let Some(name) = self.name() {
            out.push_str(context.token(&name));
        }

        write_generics(self, context, shape, &mut out);

        if let Some(super_traits) = self.super_trait_list() {
            out.push_str(&super_traits.rewrite_or_original(context, shape));
        }

        write_where_clause(self, context, shape, &mut out);

        if let Some(items) = self.item_list() {
            out.push(' ');
            out.push_str(&items.rewrite_or_original(context, shape));
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::TraitItemList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        rewrite_block_items(self, context, shape)
    }
}

impl Rewrite for ast::TraitItem {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        match self.kind() {
            TraitItemKind::Func(func) => func.rewrite(context, shape),
            TraitItemKind::Type(ty) => ty.rewrite(context, shape),
            TraitItemKind::Const(c) => c.rewrite(context, shape),
        }
    }
}

impl Rewrite for ast::TraitTypeItem {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);

        out.push_str("type ");
        if let Some(name) = self.name() {
            out.push_str(context.token(&name));
        }

        if let Some(bounds) = self.bounds() {
            out.push_str(&bounds.rewrite_or_original(context, shape));
        }

        if let Some(ty) = self.ty() {
            out.push_str(" = ");
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        Some(out)
    }
}

impl Rewrite for ast::TraitConstItem {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);

        out.push_str("const ");
        if let Some(name) = self.name() {
            out.push_str(context.token(&name));
        }

        if let Some(ty) = self.ty() {
            out.push_str(": ");
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        if let Some(value) = self.value() {
            out.push_str(" = ");
            out.push_str(&value.rewrite_or_original(context, shape));
        }

        Some(out)
    }
}

impl Rewrite for ast::Impl {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);

        out.push_str("impl");

        write_generics(self, context, shape, &mut out);

        if let Some(ty) = self.ty() {
            out.push(' ');
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        write_where_clause(self, context, shape, &mut out);

        if let Some(items) = self.item_list() {
            out.push(' ');
            out.push_str(&items.rewrite_or_original(context, shape));
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::ImplItemList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        rewrite_block_items(self, context, shape)
    }
}

impl Rewrite for ast::ImplTrait {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);

        out.push_str("impl");
        write_generics(self, context, shape, &mut out);

        if let Some(trait_ref) = self.trait_ref() {
            out.push(' ');
            out.push_str(&trait_ref.rewrite_or_original(context, shape));
        }

        if let Some(ty) = self.ty() {
            out.push_str(" for ");
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        write_where_clause(self, context, shape, &mut out);

        if let Some(items) = self.item_list() {
            out.push(' ');
            out.push_str(&items.rewrite_or_original(context, shape));
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::Const {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("const ");
        if let Some(name) = self.name() {
            out.push_str(context.token(&name));
        }

        if let Some(ty) = self.ty() {
            out.push_str(": ");
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        if let Some(value) = self.value() {
            out.push_str(" = ");
            out.push_str(&value.rewrite_or_original(context, shape));
        }

        Some(out)
    }
}

impl Rewrite for ast::Use {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        // Account for "use " (4 chars) or "pub use " (8 chars) prefix
        let prefix_len = if self.modifier().is_some_and(|m| m.pub_kw().is_some()) {
            8 // "pub use "
        } else {
            4 // "use "
        };
        let use_shape = Shape::with_width(shape.width.saturating_sub(prefix_len), shape.indent);

        out.push_str("use ");
        out.push_str(&self.use_tree()?.rewrite_or_original(context, use_shape));

        Some(out)
    }
}

impl Rewrite for ast::UseTree {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();
        let mut used_width = 0;

        // Format path if present (e.g., `Foo::Bar` in `Foo::Bar::{x, y}`)
        if let Some(path) = self.path() {
            let path_str = path.rewrite_or_original(context, shape);
            used_width += path_str.len();
            out.push_str(&path_str);
        }

        // Format children if present (e.g., `{x, y}` in `Foo::{x, y}`)
        if let Some(children) = self.children() {
            // Add `::` separator between path and children list
            if self.path().is_some() {
                out.push_str("::");
                used_width += 2;
            }
            // Reduce available width by what we've already used
            let remaining_width = shape.width.saturating_sub(used_width);
            let children_shape = Shape::with_width(remaining_width, shape.indent);
            out.push_str(&children.rewrite_or_original(context, children_shape));
        }

        // Format alias if present (e.g., `as Bar` in `Foo as Bar`)
        if let Some(alias) = self.alias() {
            out.push_str(&alias.rewrite_or_original(context, shape));
        }

        Some(out)
    }
}

impl Rewrite for ast::UseTreeList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let formatting = ListFormatting::new(shape)
            .tactic(ListTactic::Mixed)
            .with_surround("{", "}");
        format_list(self, &formatting, context, shape)
    }
}

impl Rewrite for ast::UsePath {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let segments: Vec<String> = self
            .into_iter()
            .map(|seg| seg.rewrite_or_original(context, shape))
            .collect();
        Some(segments.join("::"))
    }
}

impl Rewrite for ast::UsePathSegment {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        use parser::ast::UsePathSegmentKind;
        match self.kind()? {
            UsePathSegmentKind::Ingot(token) => Some(context.token(&token).to_string()),
            UsePathSegmentKind::Super(token) => Some(context.token(&token).to_string()),
            UsePathSegmentKind::Self_(token) => Some(context.token(&token).to_string()),
            UsePathSegmentKind::Ident(token) => Some(context.token(&token).to_string()),
            UsePathSegmentKind::Glob(token) => Some(context.token(&token).to_string()),
        }
    }
}

impl Rewrite for ast::UseAlias {
    fn rewrite(&self, context: &RewriteContext<'_>, _shape: Shape) -> Option<String> {
        if let Some(alias) = self.alias() {
            let alias_text = context.snippet(alias.text_range()).trim();
            Some(format!(" as {alias_text}"))
        } else {
            None
        }
    }
}

impl Rewrite for ast::TypeAlias {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("type ");
        if let Some(alias) = self.alias() {
            out.push_str(context.token(&alias));
        }

        write_generics(self, context, shape, &mut out);

        if let Some(ty) = self.ty() {
            out.push_str(" = ");
            out.push_str(&ty.rewrite_or_original(context, shape));
        }

        Some(out)
    }
}

impl Rewrite for ast::Mod {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);
        write_item_modifier(self, &mut out);

        out.push_str("mod ");
        if let Some(name) = self.name() {
            out.push_str(context.token(&name));
        }

        if let Some(items) = self.items() {
            let block = rewrite_block_items(items.into_iter(), context, shape)?;
            out.push(' ');
            out.push_str(&block);
        }

        Some(out)
    }
}

impl Rewrite for ast::Extern {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        let mut out = String::new();

        write_attrs(self, context, &mut out);

        out.push_str("extern");

        if let Some(items) = self.extern_block() {
            out.push(' ');
            out.push_str(&items.rewrite_or_original(context, shape));
        } else {
            out.push_str(" {}");
        }

        Some(out)
    }
}

impl Rewrite for ast::ExternItemList {
    fn rewrite(&self, context: &RewriteContext<'_>, shape: Shape) -> Option<String> {
        rewrite_block_items(self, context, shape)
    }
}
