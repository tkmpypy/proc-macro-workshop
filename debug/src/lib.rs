use std::fmt::Display;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Lit, Meta, MetaNameValue, NestedMeta};

struct Field<T> {
    value: T,
    bitmask: u8,
}

impl<T> std::fmt::Debug for Field<T>
where
    T: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Field")
            .field("value", &self.value)
            .field("bitmask", &self.bitmask)
            .finish()
    }
}

fn error<T: Display>(span: Span, msg: T) -> TokenStream {
    syn::Error::new(span, msg).to_compile_error().into()
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    dbg!(&ast);
    let ident = &ast.ident;
    let ident_str = &ast.ident.to_string();
    
    let generic = &mut ast.generics.clone();
    for g in generic.params.iter_mut() {
        if let syn::GenericParam::Type(t) = g {
            t.bounds.push(syn::parse_quote!(std::fmt::Debug));
        }
    }
    let (impl_gen, ty_gen, where_clause) = generic.split_for_impl();

    let named_fields = match &ast.data {
        syn::Data::Struct(s) => match &s.fields {
            syn::Fields::Named(f) => &f.named,
            _ => return error(ident.span(), "expects named fields"),
        },
        _ => return error(ident.span(), "expects struct"),
    };

    let struct_fields = named_fields.iter().map(|f| {
        let debug_attr = f
            .attrs
            .first()
            .map(|attr| {
                let meta = attr.parse_meta();
                let args: syn::Result<Meta> = attr.parse_args();

                match &meta {
                    Ok(Meta::NameValue(named)) if named.path.is_ident("debug") => {
                        if let Lit::Str(ref val) = named.lit {
                            return Some(val.value());
                        };

                        None
                    }
                    _ => None,
                }
            })
            .flatten();

        match &f.ident {
            Some(id) => {
                let id_str = id.to_string();
                let mut format = "{:?}".to_string();
                if let Some(s) = debug_attr {
                    format = s.to_string();
                }
                quote! {
                    .field(#id_str, &format_args!(#format, &self.#id))
                }
            }
            _ => quote! {},
        }
    });

    let q = quote! {
        impl #impl_gen std::fmt::Debug for #ident #ty_gen #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#ident_str)
                    #(#struct_fields)*
                    .finish()
            }
        }
    };
    let ts = q.into();
    dbg!(&ts);
    ts
}
