use crate::context::CodegenContext;
use crate::fields_gen::{gen_complete_struct, gen_parse_struct};
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

    fn gen_complete(&self, ctx: &CodegenContext) -> TokenStream {
        let label = self.label;
        let complete_variant = gen_complete_struct(ctx, self.fields);
        quote! {
            #label => {
                input = remaining;
                #complete_variant
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

    fn gen_complete(&self, ctx: &CodegenContext) -> TokenStream {
        let complete_variant = gen_complete_struct(ctx, self.fields);
        quote! {
            let result = (||{ #complete_variant })();
            if result.value_consumed {
                return result
            }
            suggestions.extend(result.suggestions);
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
            Some(Ok((token @ ::cmd_parser::tokens::Token::Attribute(_), remaining))) => {
                Err(::cmd_parser::error::UnrecognizedToken::new(token, remaining).into())
            }
            Some(Ok((token @ ::cmd_parser::tokens::Token::Text(text) , remaining))) => {
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

pub(crate) fn gen_complete_enum(
    codegen_ctx: &CodegenContext<'_>,
    variants: &VariantsSet<'_>,
) -> TokenStream {
    let mut variant_names = Vec::new();
    let mut variants_complete = TokenStream::new();

    for variant in variants.variant_views() {
        variant_names.push(variant.label);
        variants_complete.extend(variant.gen_complete(codegen_ctx));
    }
    variant_names.sort_unstable();

    let transparent_complete = variants
        .transparent_variants()
        .map(|variant| variant.gen_complete(codegen_ctx));

    quote! {
        const VARIANT_NAMES: &[&str] = &[#(#variant_names),*];
        let mut suggestions = ::std::collections::BTreeSet::new();
        match input.take() {
            None | Some(Err(_)) => return ::cmd_parser::CompletionResult::new_final(false),
            Some(Ok((::cmd_parser::tokens::Token::Text(text), remaining))) => {
                let text = text.parse_string();
                if remaining.is_all_consumed() {
                    suggestions.extend(::cmd_parser::tokens::complete_variants(&text, VARIANT_NAMES).map(::std::borrow::Cow::Borrowed));
                } else {
                    match ::std::borrow::Borrow::<str>::borrow(&text) {
                        #variants_complete
                        _ => {}
                    }
                }
            }
            Some(_) => {}
        }
        #(#transparent_complete)*
        ::cmd_parser::CompletionResult::new(input, false).add_suggestions(suggestions)
    }
}
