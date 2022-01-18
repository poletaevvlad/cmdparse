mod attributes;
mod gen;
mod schema;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use schema::{ParsableContext, ParsableStruct};
use syn::spanned::Spanned;

fn derive_struct<'a>(
    ctx: &mut ParsableContext<'a>,
    data: &'a syn::DataStruct,
) -> Result<(TokenStream2, TokenStream2), syn::Error> {
    let _parsable_struct = ParsableStruct::from_fields(ctx, &data.fields)?;
    Ok((quote! {todo!()}, quote! {todo!()}))
}

fn derive_enum<'a>(
    _ctx: &mut ParsableContext<'a>,
    _data: &'a syn::DataEnum,
) -> Result<(TokenStream2, TokenStream2), syn::Error> {
    Ok((quote! {todo!()}, quote! {todo!()}))
}

#[proc_macro_derive(Parsable, attributes(cmd))]
pub fn derive_parseable(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident;

    let mut context = ParsableContext::default();

    let result = match input.data {
        syn::Data::Struct(ref data) => derive_struct(&mut context, data),
        syn::Data::Enum(ref data) => derive_enum(&mut context, data),
        syn::Data::Union(data) => Err(syn::Error::new(
            data.union_token.span(),
            "parsing unions is not supported",
        )),
    };
    match result {
        Ok((parse_impl, complete_impl)) => {
            gen::implementation(name, &context, parse_impl, complete_impl).into()
        }
        Err(error) => error.into_compile_error().into(),
    }
}
