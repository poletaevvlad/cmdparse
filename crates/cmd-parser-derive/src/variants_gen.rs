use crate::fields_gen::gen_parse_struct;
use crate::variants::{VariantView, VariantsSet};
use proc_macro2::TokenStream;
use quote::quote;

impl<'a> VariantView<'a> {
    fn gen_parse(&self, ident: &syn::Ident, ctx: &TokenStream) -> TokenStream {
        let label = self.label;
        let variant_ident = self.ident;
        let parse_variant = gen_parse_struct(quote! { #ident::#variant_ident }, ctx, self.fields);
        quote! {
            #label => {
                input = remaining;
                #parse_variant
            }
        }
    }
}

pub(crate) fn gen_parse_enum(
    type_ident: &syn::Ident,
    ctx: &TokenStream,
    variants: &VariantsSet<'_>,
) -> TokenStream {
    let mut variants_parsing = TokenStream::new();

    for variant in variants.variant_views() {
        variants_parsing.extend(variant.gen_parse(type_ident, ctx));
    }

    quote! {
        let (token, remaining) = ::cmd_parser::tokens::take_token(input);
        match token {
            ::cmd_parser::tokens::Token::Text(variant) => match &variant {
                #variants_parsing
                _ => ::cmd_parser::ParseResult::Failed(::cmd_parser::ParseError::unknown_variant(variant)),
            }
            ::cmd_parser::tokens::Token::Attribute(attr) => ::cmd_parser::ParseResult::UnrecognizedAttribute(attr, remaining),
        }
    }
}
