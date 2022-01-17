mod gen;
mod schema;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use schema::{ParsableContext, ParsableStruct};
use syn::spanned::Spanned;

fn derive_struct(type_name: syn::Ident, data: syn::DataStruct) -> Result<TokenStream2, syn::Error> {
    let mut context = ParsableContext::default();
    let _parsable_struct = ParsableStruct::from_fields(&mut context, &data.fields)?;
    let parser_name = format_ident!("{}Parser", type_name);

    let parsers_definition = gen::parsers::definition(&context);
    let parsers_initialization = gen::parsers::initialization(&context);
    let where_clause = context.generics.where_clause.as_ref();

    let type_generics = gen::generics::usage(&context, false);
    let ctx_generics = gen::generics::context_usage(&context);
    let trait_generics = gen::generics::definition(&context, true);
    let parser_struct_generics = gen::generics::definition(&context, !context.parsers.is_empty());
    let parser_usage_generics = gen::generics::usage(&context, !context.parsers.is_empty());

    Ok(quote! {
        struct #parser_struct_generics #parser_name #where_clause { #parsers_definition }

        impl #trait_generics ::cmd_parser::Parser #ctx_generics for #parser_name #where_clause {
            type Value = #type_name #type_generics;

            fn create(ctx: Ctx) -> Self {
                #parser_name { #parsers_initialization }
            }

            fn parse<'a>(&self, input: &'a str) -> ::cmd_parser::ParseResult<'a, Self::Value> {
                todo!();
            }

            fn complete<'a>(&self, input: &'a str) -> ::cmd_parser::CompletionResult<'a> {
                todo!();
            }
        }

        impl #trait_generics ::cmd_parser::Parsable #ctx_generics for #type_name #type_generics #where_clause {
            type Parser = #parser_name #parser_usage_generics;
        }
    })
}

fn derive_enum(_name: syn::Ident, _data: syn::DataEnum) -> Result<TokenStream2, syn::Error> {
    Ok(quote! {})
}

#[proc_macro_derive(Parsable, attributes(cmd))]
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
