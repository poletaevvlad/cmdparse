mod attributes;
mod context;
mod fields;
mod fields_gen;
mod gen;
mod variants;
mod variants_gen;

use attributes::{BuildableAttributes, TypeAttributes};
use context::CodegenContext;
use fields::FieldsSet;
use fields_gen::gen_parse_struct;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;
use variants::VariantsSet;
use variants_gen::gen_parse_enum;

type DeriveResult = Result<(TokenStream2, TokenStream2), syn::Error>;

fn derive_struct<'a>(ctx: &mut CodegenContext<'a>, data: &'a syn::DataStruct) -> DeriveResult {
    let fieldset = FieldsSet::from_fields(ctx, &data.fields)?;
    let name = ctx.type_name;
    let parse_tokens = gen_parse_struct(quote! { #name }, ctx, &fieldset);
    Ok((parse_tokens, quote! {todo!()}))
}

fn derive_enum<'a>(ctx: &mut CodegenContext<'a>, data: &'a syn::DataEnum) -> DeriveResult {
    let variantset = VariantsSet::from_variants(ctx, data.variants.iter())?;
    let parse_tokens = gen_parse_enum(ctx, &variantset);
    Ok((parse_tokens, quote! {todo!()}))
}

fn derive<'a>(ctx: &mut CodegenContext<'a>, input: &'a syn::DeriveInput) -> DeriveResult {
    let type_attributes = TypeAttributes::from_attributes(input.attrs.iter())?;
    ctx.context_type = type_attributes.context_type;

    match &input.data {
        syn::Data::Struct(data) => derive_struct(ctx, data),
        syn::Data::Enum(data) => derive_enum(ctx, data),
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

    let mut context = CodegenContext::from_derive_input(&input);
    let result = derive(&mut context, &input);

    match result {
        Ok((parse_impl, complete_impl)) => {
            gen::implementation(name, &context, parse_impl, complete_impl).into()
        }
        Err(error) => error.into_compile_error().into(),
    }
}
