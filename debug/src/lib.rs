use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput, Lit, Meta, MetaNameValue, NestedMeta};

struct Test {
    test: String,
}

impl std::fmt::Debug for Test {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Test").finish()
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    dbg!(&ast);
    let ident = ast.ident;
    let ident_str = ident.to_string();
    let struct_vis = ast.vis;
    let named_fields = match &ast.data {
        syn::Data::Struct(s) => match &s.fields {
            syn::Fields::Named(f) => &f.named,
            _ => {
                return syn::Error::new(ident.span(), "expects named fields")
                    .to_compile_error()
                    .into()
            }
        },
        _ => {
            return syn::Error::new(ident.span(), "expects struct")
                .to_compile_error()
                .into()
        }
    };
    let (idents, types): (Vec<syn::Ident>, Vec<syn::Type>) = match &ast.data {
        syn::Data::Struct(s) => match &s.fields {
            syn::Fields::Named(f) => f
                .clone()
                .named
                .into_iter()
                .map(|_f| {
                    let _ident = _f.ident;
                    let _ty = _f.ty;
                    (_ident.unwrap(), _ty)
                })
                .unzip(),
            _ => {
                return syn::Error::new(ident.span(), "expects named fields")
                    .to_compile_error()
                    .into()
            }
        },
        _ => {
            return syn::Error::new(ident.span(), "expects struct")
                .to_compile_error()
                .into()
        }
    };

    let debug_fields = named_fields.iter().map(|f| {
        let debug_attr = f.attrs.first().map(|attr| {
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
        });

        match &f.ident {
            Some(id) => {
                let id_str = id.to_string();
                match &debug_attr {
                    Some(s) => {
                        quote! {
                            .field(#id_str, &format_args!(#s, &self.#id))
                        }
                    }
                    None => {
                        quote! {
                            .field(#id_str, &self.#id)
                        }
                    }
                }
            }
            _ => quote! {},
        }
    });

    let q = quote! {
        impl std::fmt::Debug for #ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#ident_str)
                    #(#debug_fields)*
                    .finish()
            }
        }
    };
    q.into()
}
