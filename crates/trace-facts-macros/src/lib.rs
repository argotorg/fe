use proc_macro::TokenStream;
use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{
    Attribute, Data, DeriveInput, Expr, Fields, GenericArgument, LitStr, Path, PathArguments, Type,
    parse_macro_input, spanned::Spanned,
};

#[proc_macro_derive(
    TraceFactSpec,
    attributes(trace_fact, trace_key, trace_ref, trace_col, trace_skip)
)]
pub fn derive_trace_fact_spec(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand_trace_fact_spec(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn expand_trace_fact_spec(input: DeriveInput) -> syn::Result<TokenStream2> {
    let ident = input.ident;
    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let fact_attr = trace_fact_attr(&input.attrs)?;
    let extra_columns = extra_columns(&input.attrs)?;
    let trace_facts = trace_facts_crate()?;

    let fields = match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => fields.named,
            fields => {
                return Err(syn::Error::new(
                    fields.span(),
                    "TraceFactSpec only supports structs with named fields",
                ));
            }
        },
        _data => {
            return Err(syn::Error::new(
                Span::call_site(),
                "TraceFactSpec only supports structs",
            ));
        }
    };

    let mut columns = Vec::new();
    let mut row_values = Vec::new();
    let mut refs = Vec::new();
    let mut primary_key = None::<syn::Ident>;

    for field in fields {
        let field_ident = field
            .ident
            .clone()
            .expect("named fields should have identifiers");
        if has_attr(&field.attrs, "trace_skip") {
            reject_skip_conflicts(&field.attrs)?;
            continue;
        }
        let key_attr = field_flag_attr(&field.attrs, "trace_key")?;
        let ref_attr = trace_ref_attr(&field.attrs)?;
        let col_attrs = trace_col_attrs(&field.attrs)?;

        if let Some(attr) = key_attr {
            require_origin_key_type(&field.ty, attr.span, "trace_key")?;
            if ref_attr.as_ref().is_some_and(|attr| attr.optional) {
                return Err(syn::Error::new(
                    attr.span,
                    "trace_key cannot be combined with trace_ref(optional)",
                ));
            }
            if primary_key.replace(field_ident.clone()).is_some() {
                return Err(syn::Error::new(
                    attr.span,
                    "TraceFactSpec supports only one #[trace_key] field",
                ));
            }
            let name = attr.name.unwrap_or_else(|| field_ident.to_string());
            columns.push(column_tokens(&trace_facts, &name, quote!(Key)));
            row_values.push(encode_tokens(
                &trace_facts,
                &field.ty,
                quote!(self.#field_ident),
                ColumnKind::Key,
            ));
            if let Some(attr) = ref_attr {
                refs.push(origin_ref_tokens(
                    &trace_facts,
                    &name,
                    quote!(&self.#field_ident),
                    attr.optional,
                    attr.kind,
                ));
            }
        } else if let Some(attr) = ref_attr {
            require_trace_ref_type(&field.ty, attr.optional)?;
            let name = attr.name.unwrap_or_else(|| field_ident.to_string());
            let kind = attr.kind;
            let column_kind = if attr.optional {
                quote!(OptionalKey)
            } else {
                quote!(Key)
            };
            columns.push(column_tokens(&trace_facts, &name, column_kind));
            let role = if attr.optional {
                ColumnKind::OptionalKey
            } else {
                ColumnKind::Key
            };
            row_values.push(encode_tokens(
                &trace_facts,
                &field.ty,
                quote!(self.#field_ident),
                role,
            ));
            refs.push(origin_ref_tokens(
                &trace_facts,
                &name,
                quote!(&self.#field_ident),
                attr.optional,
                kind,
            ));
        }

        for attr in col_attrs {
            let name = attr.name.unwrap_or_else(|| field_ident.to_string());
            let kind = attr
                .kind
                .map_or_else(|| infer_column_kind(&field.ty, attr.expr.is_some()), Ok)?;
            let expr = if let Some(expr) = attr.expr {
                quote!(#expr)
            } else {
                quote!(self.#field_ident)
            };
            columns.push(column_tokens(&trace_facts, &name, kind.tokens()));
            row_values.push(encode_tokens(&trace_facts, &field.ty, expr, kind));
        }
    }

    for attr in extra_columns {
        let name = attr
            .name
            .as_deref()
            .expect("extra trace columns are validated before expansion");
        let kind = attr
            .kind
            .expect("extra trace columns are validated before expansion");
        columns.push(column_tokens(&trace_facts, name, kind.tokens()));
        let expr = attr
            .expr
            .ok_or_else(|| syn::Error::new(Span::call_site(), "struct #[trace_col] needs expr"))?;
        row_values.push(encode_tokens(
            &trace_facts,
            &Type::Verbatim(TokenStream2::new()),
            quote!(#expr),
            kind,
        ));
    }

    let primary_key_tokens =
        primary_key.map_or_else(|| quote!(None), |field| quote!(Some(&self.#field)));
    let type_name = fact_attr.type_name;
    let relation_name = fact_attr.relation_name;
    let local_validation = fact_attr.validate_hook.map_or_else(
        || quote!(::std::vec::Vec::new()),
        |hook| quote!(#hook(self)),
    );

    Ok(quote! {
        impl #impl_generics #trace_facts::relation::TraceFactSpec for #ident #ty_generics #where_clause {
            const TYPE_NAME: &'static str = #type_name;
            const RELATION_NAME: &'static str = #relation_name;

            fn primary_key(&self) -> Option<&#trace_facts::OriginExportKey> {
                #primary_key_tokens
            }

            fn origin_refs(&self) -> Vec<#trace_facts::relation::OriginRef<'_>> {
                let mut refs = Vec::new();
                #(#refs)*
                refs
            }

            fn relation_schema() -> #trace_facts::relation::RelationSchema {
                #trace_facts::relation::RelationSchema {
                    name: Self::RELATION_NAME,
                    columns: vec![#(#columns),*],
                }
            }

            fn relation_row(&self) -> #trace_facts::relation::RelationRow {
                #trace_facts::relation::RelationRow {
                    relation: Self::RELATION_NAME,
                    values: vec![#(#row_values),*],
                }
            }

            fn local_validation(&self) -> Vec<#trace_facts::relation::ValidationIssue> {
                #local_validation
            }
        }
    })
}

fn trace_facts_crate() -> syn::Result<TokenStream2> {
    match crate_name("fe-trace-facts") {
        Ok(FoundCrate::Itself) => Ok(quote!(crate)),
        Ok(FoundCrate::Name(name)) => {
            let ident = format_ident!("{}", name);
            Ok(quote!(::#ident))
        }
        Err(err) => Err(syn::Error::new(
            Span::call_site(),
            format!("failed to resolve fe-trace-facts crate for TraceFactSpec derive: {err}"),
        )),
    }
}

struct TraceFactAttr {
    type_name: LitStr,
    relation_name: LitStr,
    validate_hook: Option<Path>,
}

fn trace_fact_attr(attrs: &[Attribute]) -> syn::Result<TraceFactAttr> {
    let mut type_name = None;
    let mut relation_name = None;
    let mut validate_hook = None;
    for attr in attrs
        .iter()
        .filter(|attr| attr.path().is_ident("trace_fact"))
    {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("type") {
                type_name = Some(meta.value()?.parse()?);
                Ok(())
            } else if meta.path.is_ident("relation") {
                relation_name = Some(meta.value()?.parse()?);
                Ok(())
            } else if meta.path.is_ident("validate") {
                let value = meta.value()?.parse::<LitStr>()?;
                validate_hook = Some(syn::parse_str(&value.value()).map_err(|err| {
                    syn::Error::new(value.span(), format!("invalid validation hook path: {err}"))
                })?);
                Ok(())
            } else {
                Err(meta.error("unsupported trace_fact attribute"))
            }
        })?;
    }
    Ok(TraceFactAttr {
        type_name: type_name.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "TraceFactSpec requires #[trace_fact(type = \"...\")]",
            )
        })?,
        relation_name: relation_name.ok_or_else(|| {
            syn::Error::new(
                Span::call_site(),
                "TraceFactSpec requires #[trace_fact(relation = \"...\")]",
            )
        })?,
        validate_hook,
    })
}

struct FieldFlagAttr {
    span: Span,
    name: Option<String>,
}

fn field_flag_attr(attrs: &[Attribute], attr_name: &str) -> syn::Result<Option<FieldFlagAttr>> {
    let Some(attr) = attrs.iter().find(|attr| attr.path().is_ident(attr_name)) else {
        return Ok(None);
    };
    let mut result = FieldFlagAttr {
        span: attr.span(),
        name: None,
    };
    if matches!(attr.meta, syn::Meta::Path(_)) {
        return Ok(Some(result));
    }
    attr.parse_nested_meta(|meta| {
        if meta.path.is_ident("name") {
            result.name = Some(meta.value()?.parse::<LitStr>()?.value());
            Ok(())
        } else {
            Err(meta.error(format!("unsupported {attr_name} attribute")))
        }
    })?;
    Ok(Some(result))
}

#[derive(Default)]
struct TraceRefAttr {
    name: Option<String>,
    kind: Option<LitStr>,
    optional: bool,
}

fn trace_ref_attr(attrs: &[Attribute]) -> syn::Result<Option<TraceRefAttr>> {
    let Some(attr) = attrs.iter().find(|attr| attr.path().is_ident("trace_ref")) else {
        return Ok(None);
    };
    let mut result = TraceRefAttr::default();
    if matches!(attr.meta, syn::Meta::Path(_)) {
        return Ok(Some(result));
    }
    attr.parse_nested_meta(|meta| {
        if meta.path.is_ident("name") {
            result.name = Some(meta.value()?.parse::<LitStr>()?.value());
            Ok(())
        } else if meta.path.is_ident("kind") {
            result.kind = Some(meta.value()?.parse()?);
            Ok(())
        } else if meta.path.is_ident("optional") {
            result.optional = true;
            Ok(())
        } else {
            Err(meta.error("unsupported trace_ref attribute"))
        }
    })?;
    Ok(Some(result))
}

#[derive(Clone)]
struct TraceColAttr {
    name: Option<String>,
    kind: Option<ColumnKind>,
    expr: Option<Expr>,
}

fn trace_col_attrs(attrs: &[Attribute]) -> syn::Result<Vec<TraceColAttr>> {
    attrs
        .iter()
        .filter(|attr| attr.path().is_ident("trace_col"))
        .map(|attr| {
            let mut result = TraceColAttr {
                name: None,
                kind: None,
                expr: None,
            };
            if matches!(attr.meta, syn::Meta::Path(_)) {
                return Ok(result);
            }
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("name") {
                    result.name = Some(meta.value()?.parse::<LitStr>()?.value());
                    Ok(())
                } else if meta.path.is_ident("kind") {
                    result.kind = Some(parse_column_kind(&meta.value()?.parse::<LitStr>()?)?);
                    Ok(())
                } else if meta.path.is_ident("expr") {
                    let value = meta.value()?.parse::<LitStr>()?;
                    result.expr = Some(syn::parse_str(&value.value())?);
                    Ok(())
                } else {
                    Err(meta.error("unsupported trace_col attribute"))
                }
            })?;
            Ok(result)
        })
        .collect()
}

fn extra_columns(attrs: &[Attribute]) -> syn::Result<Vec<TraceColAttr>> {
    trace_col_attrs(attrs)?
        .into_iter()
        .map(|attr| {
            if attr.name.is_none() || attr.kind.is_none() || attr.expr.is_none() {
                Err(syn::Error::new(
                    Span::call_site(),
                    "struct #[trace_col] requires name, kind, and expr",
                ))
            } else {
                Ok(attr)
            }
        })
        .collect()
}

fn has_attr(attrs: &[Attribute], name: &str) -> bool {
    attrs.iter().any(|attr| attr.path().is_ident(name))
}

fn reject_skip_conflicts(attrs: &[Attribute]) -> syn::Result<()> {
    if let Some(attr) = attrs.iter().find(|attr| attr.path().is_ident("trace_key")) {
        return Err(syn::Error::new(
            attr.span(),
            "trace_skip cannot be combined with trace_key",
        ));
    }
    if let Some(attr) = attrs.iter().find(|attr| attr.path().is_ident("trace_ref")) {
        return Err(syn::Error::new(
            attr.span(),
            "trace_skip cannot be combined with trace_ref",
        ));
    }
    if let Some(attr) = attrs.iter().find(|attr| attr.path().is_ident("trace_col")) {
        return Err(syn::Error::new(
            attr.span(),
            "trace_skip cannot be combined with trace_col",
        ));
    }
    Ok(())
}

#[derive(Clone, Copy)]
enum ColumnKind {
    Key,
    OptionalKey,
    Text,
    OptionalText,
    U32,
    U64,
    I64,
    List,
}

impl ColumnKind {
    fn tokens(self) -> TokenStream2 {
        match self {
            Self::Key => quote!(Key),
            Self::OptionalKey => quote!(OptionalKey),
            Self::Text => quote!(Text),
            Self::OptionalText => quote!(OptionalText),
            Self::U32 => quote!(U32),
            Self::U64 => quote!(U64),
            Self::I64 => quote!(I64),
            Self::List => quote!(List),
        }
    }
}

fn parse_column_kind(value: &LitStr) -> syn::Result<ColumnKind> {
    match value.value().as_str() {
        "key" => Ok(ColumnKind::Key),
        "optional_key" => Ok(ColumnKind::OptionalKey),
        "text" => Ok(ColumnKind::Text),
        "optional_text" => Ok(ColumnKind::OptionalText),
        "u32" => Ok(ColumnKind::U32),
        "u64" => Ok(ColumnKind::U64),
        "i64" => Ok(ColumnKind::I64),
        "list" => Ok(ColumnKind::List),
        _ => Err(syn::Error::new(
            value.span(),
            "unsupported trace relation column kind",
        )),
    }
}

fn infer_column_kind(ty: &Type, explicit_expr: bool) -> syn::Result<ColumnKind> {
    if explicit_expr {
        return Ok(ColumnKind::Text);
    }
    if path_is(ty, "OriginExportKey") {
        return Ok(ColumnKind::Key);
    }
    if option_inner(ty).is_some_and(|inner| path_is(inner, "OriginExportKey")) {
        return Ok(ColumnKind::OptionalKey);
    }
    if option_inner(ty).is_some() {
        return Ok(ColumnKind::OptionalText);
    }
    if path_is(ty, "u32") {
        return Ok(ColumnKind::U32);
    }
    if path_is(ty, "u64") {
        return Ok(ColumnKind::U64);
    }
    if path_is(ty, "i32") || path_is(ty, "i64") {
        return Ok(ColumnKind::I64);
    }
    if path_is(ty, "Vec") {
        return Ok(ColumnKind::List);
    }
    if is_plain_path(ty) {
        return Ok(ColumnKind::Text);
    }
    Err(syn::Error::new(
        ty.span(),
        "TraceFactSpec cannot infer a relation column kind for this type; add #[trace_col(kind = \"...\")] or #[trace_skip]",
    ))
}

fn path_is(ty: &Type, expected: &str) -> bool {
    let Type::Path(path) = ty else {
        return false;
    };
    path.path
        .segments
        .last()
        .is_some_and(|segment| segment.ident == expected)
}

fn option_inner(ty: &Type) -> Option<&Type> {
    let Type::Path(path) = ty else {
        return None;
    };
    let segment = path.path.segments.last()?;
    if segment.ident != "Option" {
        return None;
    }
    let PathArguments::AngleBracketed(args) = &segment.arguments else {
        return None;
    };
    args.args.iter().find_map(|arg| {
        if let GenericArgument::Type(ty) = arg {
            Some(ty)
        } else {
            None
        }
    })
}

fn is_plain_path(ty: &Type) -> bool {
    let Type::Path(path) = ty else {
        return false;
    };
    path.path
        .segments
        .last()
        .is_some_and(|segment| matches!(segment.arguments, PathArguments::None))
}

fn require_origin_key_type(ty: &Type, span: Span, attr_name: &str) -> syn::Result<()> {
    if path_is(ty, "OriginExportKey") {
        Ok(())
    } else {
        Err(syn::Error::new(
            span,
            format!("{attr_name} fields must have type OriginExportKey"),
        ))
    }
}

fn require_trace_ref_type(ty: &Type, optional: bool) -> syn::Result<()> {
    if optional {
        if option_inner(ty).is_some_and(|inner| path_is(inner, "OriginExportKey")) {
            Ok(())
        } else {
            Err(syn::Error::new(
                ty.span(),
                "trace_ref(optional) fields must have type Option<OriginExportKey>",
            ))
        }
    } else if path_is(ty, "OriginExportKey") {
        Ok(())
    } else {
        Err(syn::Error::new(
            ty.span(),
            "trace_ref fields must have type OriginExportKey",
        ))
    }
}

fn column_tokens(trace_facts: &TokenStream2, name: &str, kind: TokenStream2) -> TokenStream2 {
    let name = LitStr::new(name, Span::call_site());
    quote! {
        #trace_facts::relation::RelationColumn {
            name: #name,
            kind: #trace_facts::relation::RelationColumnKind::#kind,
        }
    }
}

fn encode_tokens(
    trace_facts: &TokenStream2,
    _ty: &Type,
    expr: TokenStream2,
    kind: ColumnKind,
) -> TokenStream2 {
    match kind {
        ColumnKind::Key => quote!(#trace_facts::relation::encode_key(&#expr)),
        ColumnKind::OptionalKey => {
            quote!(#trace_facts::relation::encode_optional_key(#expr.as_ref()))
        }
        ColumnKind::OptionalText => {
            quote!(#trace_facts::relation::encode_optional_value(#expr.as_ref()))
        }
        ColumnKind::U32 | ColumnKind::U64 | ColumnKind::I64 => quote!((#expr).to_string()),
        ColumnKind::Text | ColumnKind::List => quote!(#trace_facts::relation::encode_value(&#expr)),
    }
}

fn origin_ref_tokens(
    trace_facts: &TokenStream2,
    name: &str,
    expr: TokenStream2,
    optional: bool,
    expected_kind: Option<LitStr>,
) -> TokenStream2 {
    let name = LitStr::new(name, Span::call_site());
    let expected_kind = expected_kind.map_or_else(|| quote!(None), |kind| quote!(Some(#kind)));
    if optional {
        quote! {
            if let Some(key) = #expr.as_ref() {
                refs.push(#trace_facts::relation::OriginRef {
                    field: #name,
                    key,
                    required: false,
                    expected_kind: #expected_kind,
                });
            }
        }
    } else {
        quote! {
            refs.push(#trace_facts::relation::OriginRef {
                field: #name,
                key: #expr,
                required: true,
                expected_kind: #expected_kind,
            });
        }
    }
}
