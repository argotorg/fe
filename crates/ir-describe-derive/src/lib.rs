use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{Data, DeriveInput, Fields};

#[proc_macro_derive(IrDescribe, attributes(describe))]
pub fn derive_ir_describe(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as DeriveInput);
    match derive_impl(input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

enum FieldMode {
    FieldU64(TokenStream2),
    FieldBool(TokenStream2),
    FieldStr(TokenStream2),
    FieldBytes(TokenStream2),
    AsU32(TokenStream2),
    Child,
    With(syn::Path),
    Skip,
    IterU32(TokenStream2),
}

struct FieldAttrs {
    mode: FieldMode,
}

struct VariantAttrs {
    effect: Option<String>,
    node_name: Option<String>,
}

fn parse_variant_attrs(attrs: &[syn::Attribute]) -> syn::Result<VariantAttrs> {
    let mut result = VariantAttrs { effect: None, node_name: None };
    for attr in attrs {
        if !attr.path().is_ident("describe") { continue; }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("effect") {
                let value = meta.value()?;
                let lit: syn::LitStr = value.parse()?;
                result.effect = Some(lit.value());
                Ok(())
            } else if meta.path.is_ident("node") {
                let value = meta.value()?;
                let lit: syn::LitStr = value.parse()?;
                result.node_name = Some(lit.value());
                Ok(())
            } else {
                Err(meta.error("unknown describe attribute on variant"))
            }
        })?;
    }
    Ok(result)
}

fn parse_field_attrs(field: &syn::Field) -> syn::Result<FieldAttrs> {
    let mut dim: Option<TokenStream2> = None;
    let mut skip = false;
    let mut child = false;
    let mut with: Option<syn::Path> = None;
    let mut as_u32 = false;
    let mut iter_u32 = false;

    let mut has_describe_attr = false;

    for attr in &field.attrs {
        if !attr.path().is_ident("describe") { continue; }
        has_describe_attr = true;

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("skip") {
                skip = true;
                Ok(())
            } else if meta.path.is_ident("child") {
                child = true;
                Ok(())
            } else if meta.path.is_ident("as_u32") {
                as_u32 = true;
                Ok(())
            } else if meta.path.is_ident("iter_u32") {
                iter_u32 = true;
                Ok(())
            } else if meta.path.is_ident("dim") {
                let value = meta.value()?;
                let ident: syn::Ident = value.parse()?;
                dim = Some(quote! { common::ir_describe::Dim::#ident });
                Ok(())
            } else if meta.path.is_ident("with") {
                let value = meta.value()?;
                let path: syn::Path = value.parse()?;
                with = Some(path);
                Ok(())
            } else {
                Err(meta.error("unknown describe attribute"))
            }
        })?;
    }

    if !has_describe_attr {
        let field_name = field.ident.as_ref()
            .map(|i| i.to_string())
            .unwrap_or_else(|| "unnamed".to_string());
        return Err(syn::Error::new_spanned(
            field,
            format!(
                "field `{field_name}` is missing #[describe(...)] attribute. \
                 Use #[describe(dim = Structure)] or #[describe(skip)] etc."
            ),
        ));
    }

    if skip {
        return Ok(FieldAttrs { mode: FieldMode::Skip });
    }
    if child {
        return Ok(FieldAttrs { mode: FieldMode::Child });
    }
    if let Some(path) = with {
        return Ok(FieldAttrs { mode: FieldMode::With(path) });
    }
    if as_u32 {
        let dim = dim.ok_or_else(|| syn::Error::new_spanned(
            field, "#[describe(as_u32)] requires dim = ... to be specified",
        ))?;
        return Ok(FieldAttrs { mode: FieldMode::AsU32(dim) });
    }
    if iter_u32 {
        let dim = dim.ok_or_else(|| syn::Error::new_spanned(
            field, "#[describe(iter_u32)] requires dim = ... to be specified",
        ))?;
        return Ok(FieldAttrs { mode: FieldMode::IterU32(dim) });
    }

    let dim = dim.ok_or_else(|| syn::Error::new_spanned(
        field, "missing dim = ... in #[describe(...)]",
    ))?;

    let ty = &field.ty;
    let ty_str = quote!(#ty).to_string();
    let mode = if ty_str.contains("bool") {
        FieldMode::FieldBool(dim)
    } else if ty_str.contains("& str") || ty_str.contains("String") || ty_str.contains("str") {
        FieldMode::FieldStr(dim)
    } else if ty_str.contains("Vec < u8 >") || ty_str.contains("[u8]") {
        FieldMode::FieldBytes(dim)
    } else {
        FieldMode::FieldU64(dim)
    };

    Ok(FieldAttrs { mode })
}

fn emit_field(access: &TokenStream2, attrs: &FieldAttrs, is_ref: bool) -> TokenStream2 {
    match &attrs.mode {
        FieldMode::Skip => quote! {},
        FieldMode::Child => quote! { __c.child(__cx, &#access); },
        FieldMode::With(path) => quote! { #path(__cx, __c, &#access); },
        FieldMode::AsU32(dim) => quote! {
            __c.field_u64(#dim, ::cranelift_entity::EntityRef::as_u32(&#access) as u64);
        },
        FieldMode::IterU32(dim) => quote! {
            for __v in (#access).iter() {
                __c.field_u64(#dim, ::cranelift_entity::EntityRef::as_u32(__v) as u64);
            }
        },
        FieldMode::FieldU64(dim) => {
            if is_ref {
                quote! { __c.field_u64(#dim, *#access as u64); }
            } else {
                quote! { __c.field_u64(#dim, #access as u64); }
            }
        },
        FieldMode::FieldBool(dim) => {
            if is_ref {
                quote! { __c.field_bool(#dim, *#access); }
            } else {
                quote! { __c.field_bool(#dim, #access); }
            }
        },
        FieldMode::FieldStr(dim) => {
            quote! { __c.field_str(#dim, AsRef::<str>::as_ref(&#access)); }
        },
        FieldMode::FieldBytes(dim) => {
            quote! { __c.field_bytes(#dim, AsRef::<[u8]>::as_ref(&#access)); }
        },
    }
}

fn derive_impl(input: DeriveInput) -> syn::Result<TokenStream2> {
    let name = &input.ident;
    let node_name_str = name.to_string();
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let body = match &input.data {
        Data::Struct(data) => derive_struct_body(&data.fields, &node_name_str)?,
        Data::Enum(data) => derive_enum_body(data)?,
        Data::Union(_) => {
            return Err(syn::Error::new_spanned(
                name, "IrDescribe cannot be derived for unions",
            ));
        }
    };

    Ok(quote! {
        impl #impl_generics common::ir_describe::IrDescribe for #name #ty_generics #where_clause {
            fn describe<__C: common::ir_describe::IrConsumer>(
                &self,
                __cx: &common::ir_describe::DescribeCtx<'_>,
                __c: &mut __C,
            ) {
                #body
            }
        }
    })
}

fn derive_struct_body(fields: &Fields, node_name: &str) -> syn::Result<TokenStream2> {
    let field_emissions = emit_struct_fields(fields)?;
    Ok(quote! {
        __c.enter_node(#node_name);
        #(#field_emissions)*
        __c.exit_node();
    })
}

fn emit_struct_fields(fields: &Fields) -> syn::Result<Vec<TokenStream2>> {
    let mut emissions = Vec::new();
    match fields {
        Fields::Named(fields) => {
            for field in &fields.named {
                let attrs = parse_field_attrs(field)?;
                let field_name = field.ident.as_ref().unwrap();
                let access = quote! { self.#field_name };
                emissions.push(emit_field(&access, &attrs, false));
            }
        }
        Fields::Unnamed(fields) => {
            for (i, field) in fields.unnamed.iter().enumerate() {
                let attrs = parse_field_attrs(field)?;
                let idx = syn::Index::from(i);
                let access = quote! { self.#idx };
                emissions.push(emit_field(&access, &attrs, false));
            }
        }
        Fields::Unit => {}
    }
    Ok(emissions)
}

fn derive_enum_body(data: &syn::DataEnum) -> syn::Result<TokenStream2> {
    let mut arms = Vec::new();

    for variant in &data.variants {
        let variant_name = &variant.ident;
        let variant_attrs = parse_variant_attrs(&variant.attrs)?;
        let node_name = variant_attrs.node_name
            .unwrap_or_else(|| variant_name.to_string());

        let effect_emit = variant_attrs.effect.map(|e| {
            quote! { __c.effect(#e); }
        });

        let (pattern, field_emissions) = match &variant.fields {
            Fields::Named(fields) => {
                let mut names = Vec::new();
                let mut emissions = Vec::new();

                for field in &fields.named {
                    let field_name = field.ident.as_ref().unwrap();
                    names.push(field_name.clone());
                    let attrs = parse_field_attrs(field)?;
                    let access = quote! { #field_name };
                    emissions.push(emit_field(&access, &attrs, true));
                }

                let pat = quote! { Self::#variant_name { #(#names),* } };
                (pat, emissions)
            }
            Fields::Unnamed(fields) => {
                let mut bindings = Vec::new();
                let mut emissions = Vec::new();

                for (i, field) in fields.unnamed.iter().enumerate() {
                    let binding = syn::Ident::new(
                        &format!("__f{i}"), proc_macro2::Span::call_site(),
                    );
                    bindings.push(binding.clone());
                    let attrs = parse_field_attrs(field)?;
                    let access = quote! { #binding };
                    emissions.push(emit_field(&access, &attrs, true));
                }

                let pat = quote! { Self::#variant_name(#(#bindings),*) };
                (pat, emissions)
            }
            Fields::Unit => {
                let pat = quote! { Self::#variant_name };
                (pat, Vec::new())
            }
        };

        arms.push(quote! {
            #pattern => {
                __c.enter_node(#node_name);
                #effect_emit
                #(#field_emissions)*
                __c.exit_node();
            }
        });
    }

    Ok(quote! {
        match self {
            #(#arms)*
        }
    })
}
