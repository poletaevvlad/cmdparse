use crate::fields_gen::gen_parse_struct;
use crate::variants::{TransparentVariantView, VariantView, VariantsSet};
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

impl<'a> TransparentVariantView<'a> {
    fn gen_parse(&self, ident: &syn::Ident, ctx: &TokenStream) -> TokenStream {
        let variant_ident = self.ident;
        let parse_variant = gen_parse_struct(quote! { #ident::#variant_ident }, ctx, self.fields);
        quote! {
            let parsed = (||{ #parse_variant })();
            if ! parsed.is_unrecognized() {
                return parsed
            }
        }
    }
}

pub(crate) fn gen_parse_enum(
    type_ident: &syn::Ident,
    ctx: &TokenStream,
    variants: &VariantsSet<'_>,
) -> TokenStream {
    let variants_parsing = variants
        .variant_views()
        .map(|variant| variant.gen_parse(type_ident, ctx));

    let transparent_parsed = variants
        .transparent_variants()
        .map(|variant| variant.gen_parse(type_ident, ctx));

    quote! {
        let (token, remaining) = ::cmd_parser::tokens::take_token(input);
        match token {
            ::cmd_parser::tokens::Token::Text(variant) => match ::std::borrow::Borrow::<str>::borrow(&variant) {
                #(#variants_parsing)*
                token if token.is_empty() => ::cmd_parser::ParseResult::Failed(::cmd_parser::ParseError::token_required("variant")),
                _ => {
                    #(#transparent_parsed)*
                    ::cmd_parser::ParseResult::UnrecognizedVariant(variant)
                }
            }
            ::cmd_parser::tokens::Token::Attribute(attr) => ::cmd_parser::ParseResult::UnrecognizedAttribute(attr, remaining),
        }
    }
}
