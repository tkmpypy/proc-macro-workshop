use std::fmt::Display;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Lit, Meta, PathSegment};

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

fn contains_type(types: &Vec<syn::Type>, type_name: &str) -> bool {
    let exists = types.iter().find(|_ty| {
        if let Some(seg) = get_last_path_segment(_ty) {
            let id_str = seg.ident.to_string();
            if type_name == id_str {
                return true;
            }
        }
        false
    });
    exists.is_some()
}

fn is_phantom_data(ty: &syn::Type) -> bool {
    match get_last_path_segment(ty) {
        Some(seg) => seg.ident.to_string() == "PhantomData",
        _ => false,
    }
}
fn unwrap_phantom_data(ty: &syn::Type) -> Option<&syn::Type> {
    if !is_phantom_data(ty) {
        return None;
    }
    unwrap_generic_type(ty)
}
fn get_last_path_segment(ty: &syn::Type) -> Option<&PathSegment> {
    match ty {
        syn::Type::Path(path) => path.path.segments.last(),
        _ => None,
    }
}
fn unwrap_generic_type(ty: &syn::Type) -> Option<&syn::Type> {
    match get_last_path_segment(ty) {
        Some(seg) => match &seg.arguments {
            syn::PathArguments::AngleBracketed(args) => match args.args.first() {
                Some(a) => match a {
                    syn::GenericArgument::Type(t) => Some(t),
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        },
        _ => None,
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    dbg!(&ast);

    let ident = &ast.ident;
    let ident_str = &ast.ident.to_string();

    let named_fields = match &ast.data {
        syn::Data::Struct(s) => match &s.fields {
            syn::Fields::Named(f) => &f.named,
            _ => panic!("not supported"),
        },
        _ => panic!("not supported"),
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
            _ => panic!("not supported"),
        },
        _ => panic!("not supported"),
    };
    let phantom_generic_type_names: Vec<syn::Type> = types
        .iter()
        .filter(|ty| is_phantom_data(ty))
        .map(|f| unwrap_phantom_data(f).unwrap().to_owned())
        .collect();

    let generic = &mut ast.generics.clone();
    for g in generic.params.iter_mut() {
        if let syn::GenericParam::Type(t) = g {
            let type_name = t.ident.to_string();
            if contains_type(&phantom_generic_type_names, &type_name)
                && !contains_type(&types, &type_name)
            {
                continue;
            }
            t.bounds.push(syn::parse_quote!(std::fmt::Debug));
        }
    }
    let (impl_gen, ty_gen, where_clause) = generic.split_for_impl();

    let struct_fields = named_fields.iter().map(|f| {
        let debug_attr = f
            .attrs
            .first()
            .map(|attr| {
                let meta = attr.parse_meta();

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
