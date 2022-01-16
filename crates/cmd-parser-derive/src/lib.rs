use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;

fn derive_struct(_name: syn::Ident, _data: syn::DataStruct) -> Result<TokenStream2, syn::Error> {
    Ok(quote! {})
}

fn derive_enum(_name: syn::Ident, _data: syn::DataEnum) -> Result<TokenStream2, syn::Error> {
    Ok(quote! {})
}

#[proc_macro_derive(CmdParsable, attributes(cmd))]
pub fn derive_parseable(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident;

    let result = match input.data {
        syn::Data::Struct(data) => derive_struct(name, data),
        syn::Data::Enum(data) => derive_enum(name, data),
        syn::Data::Union(data) => Err(syn::Error::new(
            data.union_token.span(),
            "parsing unions is not supported",
        )),
    };
    match result {
        Ok(token_stream) => token_stream.into(),
        Err(error) => error.into_compile_error().into(),
    }
}
