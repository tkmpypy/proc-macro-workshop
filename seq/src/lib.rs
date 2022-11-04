use proc_macro::{TokenStream, Ident};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, parse::Parse, punctuated::Punctuated, Token, token, Field, parenthesized};

struct SeqInput {
    variable: syn::Ident,
    in_token: syn::Token![in],
    s_num: syn::LitInt,
    range: syn::Token![..],
    e_num: syn::LitInt,
    brace_token: token::Brace,
    body: Punctuated<BodyInput, syn::Token![;]>,
}

impl Parse for SeqInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        dbg!(&input);
        let content;
        Ok(SeqInput{
            variable: input.parse()?,
            in_token: input.parse()?,
            s_num: input.parse()?,
            range: input.parse()?,
            e_num: input.parse()?,
            brace_token: syn::braced!(content in input),
            body: content.parse_terminated(BodyInput::parse)?,
        })
    }
}

struct BodyInput {
    call_func: syn::Ident,
    exclamation: syn::Token![!],
    paren_token: token::Paren,
    args: Punctuated<syn::Ident, Token![,]>,
}

impl Parse for BodyInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        Ok(BodyInput {
            call_func: input.parse()?,
            exclamation: input.parse()?,
            paren_token: parenthesized!(content in input),
            args: content.parse_terminated(syn::Ident::parse)?,
        })
    }
}

struct Arg {

}

impl Parse for Arg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        todo!()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as SeqInput);
    let q = quote!{

    };
    q.into()
}
