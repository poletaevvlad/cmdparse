mod attributes;
mod fields;
mod fields_gen;
mod gen;

use attributes::{BuildableAttributes, TypeAttributes};
use fields::{FieldsSet, ParsableContext};
use fields_gen::gen_parse_struct;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;

type DeriveResult = Result<(TokenStream2, TokenStream2), syn::Error>;

fn derive_struct<'a>(
    name: &syn::Ident,
    ctx: &mut ParsableContext<'a>,
    data: &'a syn::DataStruct,
) -> DeriveResult {
    let fieldset = FieldsSet::from_fields(ctx, &data.fields)?;
    let parse_tokens = gen_parse_struct(quote! { #name }, fieldset);
    Ok((parse_tokens, quote! {todo!()}))
}

fn derive_enum<'a>(_ctx: &mut ParsableContext<'a>, _data: &'a syn::DataEnum) -> DeriveResult {
    Ok((quote! {todo!()}, quote! {todo!()}))
}

fn derive<'a>(
    name: &syn::Ident,
    context: &mut ParsableContext<'a>,
    input: &'a syn::DeriveInput,
) -> DeriveResult {
    let type_attributes = TypeAttributes::from_attributes(input.attrs.iter())?;
    context.context_type = type_attributes.context_type;

    match &input.data {
        syn::Data::Struct(data) => derive_struct(name, context, data),
        syn::Data::Enum(data) => derive_enum(context, data),
        syn::Data::Union(data) => Err(syn::Error::new(
            data.union_token.span(),
            "parsing unions is not supported",
        )),
    }
}

#[proc_macro_derive(Parsable, attributes(cmd))]
pub fn derive_parseable(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = &input.ident;

    let mut context = ParsableContext::default();
    let result = derive(name, &mut context, &input);

    match result {
        Ok((parse_impl, complete_impl)) => {
            gen::implementation(name, &context, parse_impl, complete_impl).into()
        }
        Err(error) => error.into_compile_error().into(),
    }
}
