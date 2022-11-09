use proc_macro::TokenStream;
use quote::{quote, format_ident};
use syn::{parse_macro_input, DeriveInput};

struct Test;

impl std::fmt::Debug for Test {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Test").finish()
    }
}

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    dbg!(&ast);
    let struct_name = ast.ident;
    let struct_name_str = struct_name.to_string();
    let struct_vis = ast.vis;
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

    let debug_fields = idents.iter().map(|id| {
        let id_str = id.to_string();
        quote!{
            .field(#id_str, &self.#id)
        }
    });

    let q = quote! {
        impl std::fmt::Debug for #struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#struct_name_str)
                    #(#debug_fields)*
                    .finish()
            }
        }
    };
    q.into()
}
