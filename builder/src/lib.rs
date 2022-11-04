use proc_macro2::Span;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, DeriveInput, Ident, Lit, Meta, MetaNameValue, NestedMeta, PathSegment,
};

enum LitOrError {
    Lit(String),
    Error(syn::Error),
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

fn unwrap_option(ty: &syn::Type) -> Option<&syn::Type> {
    if !is_optional(ty) {
        return None;
    }
    unwrap_generic_type(ty)
}

fn unwrap_vector(ty: &syn::Type) -> Option<&syn::Type> {
    if !is_vec(ty) {
        return None;
    }
    unwrap_generic_type(ty)
}

fn is_vec(ty: &syn::Type) -> bool {
    match get_last_path_segment(ty) {
        Some(seg) => seg.ident.to_string() == "Vec",
        _ => false,
    }
}

fn is_optional(ty: &syn::Type) -> bool {
    match get_last_path_segment(ty) {
        Some(seg) => seg.ident.to_string() == "Option",
        _ => false,
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let ident = &ast.ident;
    let builder_ident = format_ident!("{}Builder", ident);

    let fields = match &ast.data {
        syn::Data::Struct(s) => match &s.fields {
            syn::Fields::Named(f) => f,
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
            syn::Fields::Unnamed(_) => panic!("not supported"),
            syn::Fields::Unit => panic!("not supported"),
        },
        syn::Data::Enum(_) => panic!("not supported"),
        syn::Data::Union(_) => panic!("not supported"),
    };

    let checks = idents
        .iter()
        .zip(&types)
        .filter(|(_, ty)| !is_optional(ty))
        .filter(|(_, ty)| !is_vec(ty))
        .map(|(id, _)| {
            let err = format!("Required field '{}' is missing", id.to_string());
            quote! {
                if self.#id.is_none() {
                    return Err(#err.into())
                }
            }
        });

    let builder_fields = idents.iter().zip(&types).map(|(id, ty)| {
        let type_val = if is_optional(ty) {
            match unwrap_generic_type(ty) {
                Some(t) => t,
                None => ty,
            }
        } else {
            ty
        };
        if is_vec(type_val) {
            quote! {
                #id: #type_val
            }
        } else {
            quote! {
                #id: std::option::Option<#type_val>
            }
        }
    });

    let struct_fields = idents.iter().zip(&types).map(|(ident, ty)| {
        if is_optional(ty) || is_vec(ty) {
            quote! {
                #ident: self.#ident.clone()
            }
        } else {
            quote! {
                #ident: self.#ident.clone().unwrap()
            }
        }
    });

    let setters = fields.named.iter().map(|field| {
        let ident_each_name = field
            .attrs
            .first()
            .map(|attr| match attr.parse_meta() {
                Ok(meta) => match meta {
                    syn::Meta::List(list) => match list.nested.first() {
                        Some(NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                            ref path,
                            eq_token: _,
                            lit: Lit::Str(ref str),
                        }))) => {
                            if let Some(name) = path.segments.first() {
                                if name.ident.to_string() != "each" {
                                    return Some(LitOrError::Error(syn::Error::new_spanned(
                                        list,
                                        "expected `builder(each = \"...\")`",
                                    )));
                                }
                            }
                            Some(LitOrError::Lit(str.value()))
                        }
                        _ => None,
                    },
                    _ => None,
                },
                Err(_) => None,
            })
            .flatten();

        let ident = field.ident.as_ref();
        let ty = unwrap_option(&field.ty).unwrap_or(&field.ty);
        match ident_each_name {
            Some(LitOrError::Lit(name)) => {
                let ty_each = unwrap_vector(ty).unwrap();
                let ident_each = Ident::new(name.as_str(), Span::call_site());
                if ident.unwrap().to_string() == name {
                    quote! {
                        pub fn #ident_each(&mut self, #ident_each: #ty_each) -> &mut Self {
                            self.#ident.push(#ident_each);
                            self
                        }
                    }
                } else {
                    quote! {
                        pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = #ident;
                            self
                        }
                        pub fn #ident_each(&mut self, #ident_each: #ty_each) -> &mut Self {
                            self.#ident.push(#ident_each);
                            self
                        }
                    }
                }
            }
            Some(LitOrError::Error(err)) => err.to_compile_error().into(),
            None => {
                if is_vec(&ty) {
                    quote! {
                        pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = #ident;
                            self
                        }
                    }
                } else {
                    quote! {
                        pub fn #ident(&mut self, #ident: #ty) -> &mut Self {
                            self.#ident = std::option::Option::Some(#ident);
                            self
                        }
                    }
                }
            }
        }
    });

    let defaults = idents.iter().zip(&types).map(|(ident, ty)| {
        let t = if is_optional(ty) {
            match unwrap_generic_type(ty) {
                Some(t) => t,
                None => ty,
            }
        } else {
            ty
        };
        if is_vec(t) {
            quote! {
                #ident: Vec::new()
            }
        } else {
            quote! {
                #ident: std::option::Option::None
            }
        }
    });

    let q = quote! {
        pub struct #builder_ident {
            #(#builder_fields),*
        }

        impl #builder_ident {
            #(#setters)*

            pub fn build(&mut self) -> std::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                #(#checks)*
                Ok(
                    #ident{
                        #(#struct_fields),*
                    }
                )
            }
        }

        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#defaults),*
                }
            }
        }
    };
    q.into()
}
