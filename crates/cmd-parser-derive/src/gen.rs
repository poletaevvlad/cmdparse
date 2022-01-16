use crate::schema::{ParsableContext, Parser};
use proc_macro2::TokenStream;
use quote::quote;

pub(crate) fn parsers_definition(ctx: &ParsableContext) -> TokenStream {
    let definitions = ctx.parsers.iter().map(|(parser, index)| {
        let ident = index.ident();
        let parser_type = match parser {
            Parser::Explicit(explicit) => quote! { #explicit },
            Parser::FromParsable(ty) => quote! { <#ty as ::cmd_parser::Parsable<Ctx>>::Parser },
        };
        quote! { #ident: #parser_type }
    });
    quote! { #(#definitions,)* }
}

#[cfg(test)]
mod tests {
    use crate::schema::{ParsableContext, Parser};
    use proc_macro2::TokenStream;
    use quote::quote;

    fn assert_tokens_eq(stream1: TokenStream, stream2: TokenStream) {
        assert_eq!(format!("{:?}", stream1), format!("{:?}", stream2));
    }

    mod parsers_definition_tests {
        use super::super::parsers_definition;
        use super::*;

        #[test]
        fn empty() {
            let ctx = ParsableContext::default();
            let result = parsers_definition(&ctx);
            let expected = quote! {};
            assert_tokens_eq(result, expected);
        }

        #[test]
        fn with_parsers() {
            let ty: syn::Type = syn::parse2(quote! {u8}).unwrap();

            let mut ctx = ParsableContext::default();
            ctx.push_parser(Parser::Explicit(
                syn::parse2(quote! {super::Parser}).unwrap(),
            ));
            ctx.push_parser(Parser::FromParsable(&ty));

            let result = parsers_definition(&ctx);
            let expected = quote! {
                parser_0: super::Parser,
                parser_1: <u8 as ::cmd_parser::Parsable<Ctx>>::Parser,
            };
            assert_tokens_eq(result, expected);
        }
    }
}
