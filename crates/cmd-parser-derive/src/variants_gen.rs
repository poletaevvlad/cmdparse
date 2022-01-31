use crate::context::CodegenContext;
use crate::fields_gen::gen_parse_struct;
use crate::variants::{TransparentVariantView, VariantView, VariantsSet};
use proc_macro2::TokenStream;
use quote::quote;

impl<'a> VariantView<'a> {
    fn gen_parse(&self, ctx: &CodegenContext) -> TokenStream {
        let label = self.label;
        let variant_ident = self.ident;
        let ident = ctx.type_name;
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
    fn gen_parse(&self, ctx: &CodegenContext) -> TokenStream {
        let variant_ident = self.ident;
        let ident = ctx.type_name;
        let parse_variant = gen_parse_struct(quote! { #ident::#variant_ident }, ctx, self.fields);
        quote! {
            match (||{ #parse_variant })() {
                Ok(result) => return Ok(result),
                Err(::cmd_parser::error::ParseFailure::Unrecognized(_)) => {},
                Err(error) => return Err(error),
            }
        }
    }
}

pub(crate) fn gen_parse_enum(
    codegen_ctx: &CodegenContext<'_>,
    variants: &VariantsSet<'_>,
) -> TokenStream {
    let variants_parsing = variants
        .variant_views()
        .map(|variant| variant.gen_parse(codegen_ctx));

    let transparent_parsed = variants
        .transparent_variants()
        .map(|variant| variant.gen_parse(codegen_ctx));

    quote! {
        match input.take() {
            None => Err(::cmd_parser::error::ParseError::token_required().expected("variant").into()),
            Some(Err(err)) => Err(err.into()),
            Some(Ok((token, remaining))) => match token.value() {
                ::cmd_parser::tokens::TokenValue::Attribute(_) => {
                    Err(::cmd_parser::error::UnrecognizedToken::new(token, remaining).into())
                }
                ::cmd_parser::tokens::TokenValue::Text(text) => {
                    let text = text.parse_string();
                    match ::std::borrow::Borrow::<str>::borrow(&text) {
                        #(#variants_parsing)*
                       _ => {
                            #(#transparent_parsed)*
                            Err(::cmd_parser::error::UnrecognizedToken::new(token, remaining).into())
                        }
                    }
                }
            }
        }
    }
}
