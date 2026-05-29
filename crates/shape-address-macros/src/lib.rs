use proc_macro::TokenStream;
use proc_macro_crate::{FoundCrate, crate_name};
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{Attribute, Data, DeriveInput, Fields, LitStr, parse_macro_input, spanned::Spanned};

#[proc_macro_derive(ShapeDescribe, attributes(shape))]
pub fn derive_shape_describe(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand_shape_describe(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

fn expand_shape_describe(input: DeriveInput) -> syn::Result<TokenStream2> {
    let ident = input.ident;
    let generics = input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let kind = shape_kind(&input.attrs)?;
    let shape_address = shape_address_crate()?;

    let fields = match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => fields.named,
            fields => {
                return Err(syn::Error::new(
                    fields.span(),
                    "ShapeDescribe only supports structs with named fields",
                ));
            }
        },
        _ => {
            return Err(syn::Error::new(
                Span::call_site(),
                "ShapeDescribe only supports structs",
            ));
        }
    };

    let mut field_tokens = Vec::new();
    for field in fields {
        let field_ident = field
            .ident
            .clone()
            .expect("named fields should have identifiers");
        let attr = shape_field_attr(&field.attrs, &field_ident.to_string())?;
        if attr.skip {
            continue;
        }
        let dimension = attr
            .dimension
            .ok_or_else(|| syn::Error::new(field.span(), "shape field needs a dimension"))?;
        let name = LitStr::new(&attr.name, Span::call_site());
        field_tokens.push(quote! {
            sink.add_field(
                &key,
                #shape_address::ShapeDimension::#dimension,
                #name,
                self.#field_ident.clone(),
            )?;
        });
    }

    Ok(quote! {
        impl #impl_generics #shape_address::ShapeDescribe for #ident #ty_generics #where_clause {
            const SHAPE_KIND: &'static str = #kind;

            fn describe_shape(
                &self,
                sink: &mut impl #shape_address::ShapeSink,
                key: #shape_address::ShapeNodeKey,
            ) -> Result<(), #shape_address::ShapeError> {
                sink.add_node(key.clone(), Self::SHAPE_KIND)?;
                #(#field_tokens)*
                Ok(())
            }
        }
    })
}

fn shape_address_crate() -> syn::Result<TokenStream2> {
    match crate_name("fe-shape-address") {
        Ok(FoundCrate::Itself) => Ok(quote!(crate)),
        Ok(FoundCrate::Name(name)) => {
            let ident = format_ident!("{}", name);
            Ok(quote!(::#ident))
        }
        Err(err) => Err(syn::Error::new(
            Span::call_site(),
            format!("failed to resolve fe-shape-address crate for ShapeDescribe derive: {err}"),
        )),
    }
}

fn shape_kind(attrs: &[Attribute]) -> syn::Result<LitStr> {
    let mut kind = None;
    for attr in attrs.iter().filter(|attr| attr.path().is_ident("shape")) {
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("kind") {
                kind = Some(meta.value()?.parse()?);
                Ok(())
            } else {
                Err(meta.error("unsupported shape attribute"))
            }
        })?;
    }
    kind.ok_or_else(|| {
        syn::Error::new(
            Span::call_site(),
            "ShapeDescribe requires #[shape(kind = \"...\")]",
        )
    })
}

#[derive(Default)]
struct ShapeFieldAttr {
    dimension: Option<TokenStream2>,
    name: String,
    skip: bool,
    skip_reason: Option<LitStr>,
}

fn shape_field_attr(attrs: &[Attribute], default_name: &str) -> syn::Result<ShapeFieldAttr> {
    let mut result = ShapeFieldAttr {
        name: default_name.to_string(),
        ..ShapeFieldAttr::default()
    };
    for attr in attrs.iter().filter(|attr| attr.path().is_ident("shape")) {
        attr.parse_nested_meta(|meta| {
            if let Some(dimension) = parse_dimension(&meta.path) {
                if result.dimension.replace(dimension).is_some() {
                    return Err(meta.error("shape field can have only one dimension"));
                }
                Ok(())
            } else if meta.path.is_ident("name") {
                result.name = meta.value()?.parse::<LitStr>()?.value();
                Ok(())
            } else if meta.path.is_ident("skip") {
                result.skip = true;
                if meta.input.is_empty() {
                    Ok(())
                } else {
                    meta.parse_nested_meta(|nested| {
                        if nested.path.is_ident("reason") {
                            result.skip_reason = Some(nested.value()?.parse()?);
                            Ok(())
                        } else {
                            Err(nested.error("unsupported shape skip attribute"))
                        }
                    })
                }
            } else {
                Err(meta.error("unsupported shape field attribute"))
            }
        })?;
    }
    if result.skip {
        if result.skip_reason.is_none() {
            return Err(syn::Error::new(
                Span::call_site(),
                "shape skip requires reason",
            ));
        }
        if result.dimension.is_some() {
            return Err(syn::Error::new(
                Span::call_site(),
                "shape skip cannot be combined with a dimension",
            ));
        }
    }
    Ok(result)
}

fn parse_dimension(path: &syn::Path) -> Option<TokenStream2> {
    if path.is_ident("structure") {
        Some(quote!(Structure))
    } else if path.is_ident("names") {
        Some(quote!(Names))
    } else if path.is_ident("constants") {
        Some(quote!(Constants))
    } else if path.is_ident("types") {
        Some(quote!(Types))
    } else if path.is_ident("trace_events") {
        Some(quote!(TraceEvents))
    } else {
        None
    }
}
