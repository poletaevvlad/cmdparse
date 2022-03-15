use crate::context::{CodegenContext, ContextType, Parser};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

fn ctx_generic(ctx: &CodegenContext) -> TokenStream {
    match &ctx.context_type {
        Some(ContextType::Generic(_)) | None => quote! {CmdParserCtx},
        Some(ContextType::Concrete(ty)) => quote! {#ty},
    }
}

mod parsers {
    use super::*;

    pub(crate) fn initialization(ctx: &CodegenContext) -> TokenStream {
        let generic = ctx_generic(ctx);
        let initializations = ctx.parsers.iter().map(|(parser, index)| {
            let ident = index.ident();

            match parser {
                Parser::Explicit(explicit) => quote! {
                    let #ident = <#explicit as Default>::default()
                },
                Parser::FromParsable(ty) => quote! {
                    let #ident = <#ty as ::cmdparse::Parsable<#generic>>::Parser::default()
                },
            }
        });
        quote! { #(#initializations;)* }
    }
}

mod generics {
    use super::*;

    fn quote_generics(params: &[TokenStream]) -> TokenStream {
        if params.is_empty() {
            quote! {}
        } else {
            quote! {<#(#params),*>}
        }
    }

    pub(crate) fn context_usage(ctx: &CodegenContext) -> TokenStream {
        let generic = ctx_generic(ctx);
        quote! {<#generic>}
    }

    pub(crate) fn usage(ctx: &CodegenContext, include_ctx: bool) -> TokenStream {
        let mut params = Vec::with_capacity(ctx.generics.params.len() + 1);
        params.extend(ctx.generics.lifetimes().map(|lifetime| {
            let lifetime = &lifetime.lifetime;
            quote! {#lifetime}
        }));

        if include_ctx {
            if let Some(ContextType::Generic(_)) | None = ctx.context_type {
                params.push(quote! {CmdParserCtx});
            }
        }

        params.extend(
            ctx.generics
                .params
                .iter()
                .filter(|param| !matches!(param, syn::GenericParam::Lifetime(_)))
                .map(|param| {
                    let ident = match param {
                        syn::GenericParam::Lifetime(_) => unreachable!(),
                        syn::GenericParam::Type(ty) => &ty.ident,
                        syn::GenericParam::Const(r#const) => &r#const.ident,
                    };
                    quote! {#ident}
                }),
        );
        quote_generics(&params)
    }

    pub(crate) fn definition(ctx: &CodegenContext, include_ctx: bool) -> TokenStream {
        let mut params = Vec::with_capacity(ctx.generics.params.len() + 1);
        params.extend(ctx.generics.lifetimes().map(|lifetime| {
            quote! {#lifetime}
        }));

        if include_ctx {
            match ctx.context_type {
                Some(ContextType::Generic(ref bounds)) => {
                    params.push(quote! {CmdParserCtx: #bounds + Clone});
                }
                Some(ContextType::Concrete(_)) => {}
                None => params.push(quote! {CmdParserCtx: Clone}),
            }
        }

        params.extend(
            ctx.generics
                .params
                .iter()
                .filter(|param| !matches!(param, syn::GenericParam::Lifetime(_)))
                .map(|param| quote! {#param}),
        );
        quote_generics(&params)
    }
}

pub(crate) fn implementation(
    type_name: &syn::Ident,
    ctx: &CodegenContext,
    parse_impl: TokenStream,
    complete_impl: TokenStream,
) -> TokenStream {
    let parser_name = format_ident!("{}Parser", type_name);

    let parsers_initialization = parsers::initialization(ctx);
    let where_clause = ctx.generics.where_clause.as_ref();
    let ctx_generic = ctx_generic(ctx);

    let type_generics = generics::usage(ctx, false);
    let ctx_generics = generics::context_usage(ctx);
    let trait_generics = generics::definition(ctx, true);
    let visibility_mod = ctx.visibility_mod;

    quote! {
        #[derive(Default)]
        #visibility_mod struct #parser_name;

        impl #trait_generics ::cmdparse::Parser #ctx_generics for #parser_name #where_clause {
            type Value = #type_name #type_generics;

            #[allow(unreachable_code)]
            fn parse<'a>(
                &self,
                mut input: ::cmdparse::tokens::TokenStream<'a>,
                ctx: #ctx_generic,
            ) -> ::cmdparse::ParseResult<'a, Self::Value> {
                #parsers_initialization
                #parse_impl
            }

            #[allow(unreachable_code)]
            fn complete<'a>(
                &self,
                mut input: ::cmdparse::tokens::TokenStream<'a>,
                ctx: #ctx_generic,
            ) -> ::cmdparse::CompletionResult<'a> {
                #parsers_initialization
                #complete_impl
            }
        }

        impl #trait_generics ::cmdparse::Parsable #ctx_generics for #type_name #type_generics #where_clause {
            type Parser = #parser_name;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::context::{ContextType, MockCodegenContext, Parser};
    use proc_macro2::TokenStream;
    use quote::quote;

    fn assert_tokens_eq(stream1: TokenStream, stream2: TokenStream) {
        let stream1_formatted = format!("{}", stream1).replace(" ", "");
        let stream2_formatted = format!("{}", stream2).replace(" ", "");
        assert_eq!(stream1_formatted, stream2_formatted);
    }

    mod parsers {
        use super::super::parsers::initialization;
        use super::*;

        #[test]
        fn empty() {
            let mock_context = MockCodegenContext::default();
            let ctx = mock_context.context();

            let initialization = initialization(&ctx);
            let initialization_expected = quote! {};
            assert_tokens_eq(initialization, initialization_expected);
        }

        #[test]
        fn with_parsers() {
            let ty: syn::Type = syn::parse2(quote! {u8}).unwrap();

            let mock_context = MockCodegenContext::default();
            let mut ctx = mock_context.context();

            ctx.push_parser(Parser::Explicit(
                syn::parse2(quote! {super::Parser}).unwrap(),
            ));
            ctx.push_parser(Parser::FromParsable(&ty));

            let initialization = initialization(&ctx);
            let initialization_expected = quote! {
                let parser_0 = <super::Parser as Default>::default();
                let parser_1 = <u8 as ::cmdparse::Parsable<CmdParserCtx>>::Parser::default();
            };
            assert_tokens_eq(initialization, initialization_expected);
        }

        #[test]
        fn with_parsers_and_concrete_generics() {
            let ty: syn::Type = syn::parse2(quote! {u8}).unwrap();

            let mock_context = MockCodegenContext::default();
            let mut ctx = mock_context.context();
            ctx.context_type = Some(ContextType::Concrete(
                syn::parse2(quote! {CustomCtx}).unwrap(),
            ));
            ctx.push_parser(Parser::Explicit(
                syn::parse2(quote! {super::Parser}).unwrap(),
            ));
            ctx.push_parser(Parser::FromParsable(&ty));

            let initialization = initialization(&ctx);
            let initialization_expected = quote! {
                let parser_0 = <super::Parser as Default>::default();
                let parser_1 = <u8 as ::cmdparse::Parsable<CustomCtx>>::Parser::default();
            };
            assert_tokens_eq(initialization, initialization_expected);
        }
    }

    mod generics {
        use super::super::generics::{definition, usage};
        use super::*;

        fn make_generics() -> syn::Generics {
            syn::parse2(quote! {<'a, 'b: 'a, T: Iterator<Item = u8>, const X: u8 = 5>}).unwrap()
        }

        #[test]
        fn null_context_type() {
            let mock_context = MockCodegenContext::default();
            let generics = make_generics();
            let mut ctx = mock_context.context();
            ctx.generics = &generics;

            assert_tokens_eq(usage(&ctx, false), quote! {<'a, 'b, T, X>});
            assert_tokens_eq(usage(&ctx, true), quote! {<'a, 'b, CmdParserCtx, T, X>});

            assert_tokens_eq(
                definition(&ctx, false),
                quote! {<'a, 'b: 'a, T:Iterator<Item = u8>, const X: u8 = 5>},
            );
            assert_tokens_eq(
                definition(&ctx, true),
                quote! {<'a, 'b: 'a, CmdParserCtx: Clone, T:Iterator<Item = u8>, const X: u8 = 5>},
            );
        }

        #[test]
        fn null_context_type_no_generics() {
            let mock_context = MockCodegenContext::default();
            let ctx = mock_context.context();

            assert_tokens_eq(usage(&ctx, false), quote! {});
            assert_tokens_eq(usage(&ctx, true), quote! {<CmdParserCtx>});

            assert_tokens_eq(definition(&ctx, false), quote! {});
            assert_tokens_eq(definition(&ctx, true), quote! {<CmdParserCtx:Clone>});
        }

        #[test]
        fn concrete_context() {
            let mock_context = MockCodegenContext::default();
            let generics = make_generics();
            let mut ctx = mock_context.context();
            ctx.generics = &generics;
            ctx.context_type = Some(ContextType::Concrete(syn::parse2(quote! {u8}).unwrap()));

            assert_tokens_eq(usage(&ctx, false), quote! {<'a, 'b, T, X>});
            assert_tokens_eq(usage(&ctx, true), quote! {<'a, 'b, T, X>});

            assert_tokens_eq(
                definition(&ctx, false),
                quote! {<'a, 'b: 'a, T:Iterator<Item = u8>, const X: u8 = 5>},
            );
            assert_tokens_eq(
                definition(&ctx, true),
                quote! {<'a, 'b: 'a, T:Iterator<Item = u8>, const X: u8 = 5>},
            );
        }

        #[test]
        fn concrete_context_no_generics() {
            let mock_context = MockCodegenContext::default();
            let mut ctx = mock_context.context();
            ctx.context_type = Some(ContextType::Concrete(syn::parse2(quote! {u8}).unwrap()));

            assert_tokens_eq(usage(&ctx, false), quote! {});
            assert_tokens_eq(usage(&ctx, true), quote! {});

            assert_tokens_eq(definition(&ctx, false), quote! {});
            assert_tokens_eq(definition(&ctx, true), quote! {});
        }

        #[test]
        fn generic_context() {
            let mock_context = MockCodegenContext::default();
            let generics = make_generics();
            let mut ctx = mock_context.context();
            ctx.generics = &generics;
            ctx.context_type = Some(ContextType::Generic(Box::new(
                syn::parse2::<syn::TypeParam>(quote! {T: Send + Sync})
                    .unwrap()
                    .bounds,
            )));

            assert_tokens_eq(usage(&ctx, false), quote! {<'a, 'b, T, X>});
            assert_tokens_eq(usage(&ctx, true), quote! {<'a, 'b, CmdParserCtx, T, X>});

            assert_tokens_eq(
                definition(&ctx, false),
                quote! {<'a, 'b: 'a, T:Iterator<Item = u8>, const X: u8 = 5>},
            );
            assert_tokens_eq(
                definition(&ctx, true),
                quote! {<'a, 'b: 'a, CmdParserCtx: Send + Sync + Clone, T:Iterator<Item = u8>, const X: u8 = 5>},
            );
        }

        #[test]
        fn generic_context_no_generics() {
            let mock_context = MockCodegenContext::default();
            let mut ctx = mock_context.context();
            ctx.context_type = Some(ContextType::Generic(Box::new(
                syn::parse2::<syn::TypeParam>(quote! {T: Send + Sync})
                    .unwrap()
                    .bounds,
            )));

            assert_tokens_eq(usage(&ctx, false), quote! {});
            assert_tokens_eq(usage(&ctx, true), quote! {<CmdParserCtx>});

            assert_tokens_eq(definition(&ctx, false), quote! {});
            assert_tokens_eq(
                definition(&ctx, true),
                quote! {<CmdParserCtx: Send + Sync + Clone>},
            );
        }
    }

    mod sceleton {
        use super::super::implementation;
        use super::*;
        use quote::format_ident;

        #[test]
        fn no_parsers() {
            let mock_context = MockCodegenContext::default();
            let ctx = mock_context.context();
            let result = implementation(
                &format_ident!("NoFields"),
                &ctx,
                quote! {parse!()},
                quote! {complete!()},
            );

            let expected = quote! {
                #[derive(Default)]
                struct NoFieldsParser;

                impl<CmdParserCtx: Clone> ::cmdparse::Parser<CmdParserCtx> for NoFieldsParser {
                    type Value = NoFields;

                    #[allow(unreachable_code)]
                    fn parse<'a>(&self, mut input: ::cmdparse::tokens::TokenStream<'a>, ctx: CmdParserCtx,) -> ::cmdparse::ParseResult<'a, Self::Value> { parse!() }
                    #[allow(unreachable_code)]
                    fn complete<'a>(&self, mut input: ::cmdparse::tokens::TokenStream<'a>, ctx: CmdParserCtx,) -> ::cmdparse::CompletionResult<'a> { complete!() }
                }

                impl<CmdParserCtx: Clone> ::cmdparse::Parsable<CmdParserCtx> for NoFields {
                    type Parser = NoFieldsParser;
                }
            };

            assert_tokens_eq(result, expected);
        }

        #[test]
        fn parsers_concrete_ctx() {
            let ty: syn::Type = syn::parse2(quote! {u8}).unwrap();

            let mock_context = MockCodegenContext::default();
            let mut ctx = mock_context.context();
            ctx.context_type = Some(ContextType::Concrete(
                syn::parse2(quote! {CustomCtx}).unwrap(),
            ));
            ctx.push_parser(Parser::Explicit(
                syn::parse2(quote! {super::Parser}).unwrap(),
            ));
            ctx.push_parser(Parser::FromParsable(&ty));
            let result = implementation(
                &format_ident!("WithConcreteCtx"),
                &ctx,
                quote! {parse!()},
                quote! {complete!()},
            );

            let expected = quote! {
                #[derive(Default)]
                struct WithConcreteCtxParser;

                impl ::cmdparse::Parser<CustomCtx> for WithConcreteCtxParser {
                    type Value = WithConcreteCtx;

                    #[allow(unreachable_code)]
                    fn parse<'a>(&self, mut input: ::cmdparse::tokens::TokenStream<'a>, ctx: CustomCtx,) -> ::cmdparse::ParseResult<'a, Self::Value> {
                        let parser_0 = <super::Parser as Default>::default();
                        let parser_1 = <u8 as ::cmdparse::Parsable<CustomCtx>>::Parser::default();
                        parse!()
                    }
                    #[allow(unreachable_code)]
                    fn complete<'a>(&self, mut input: ::cmdparse::tokens::TokenStream<'a>, ctx:CustomCtx,) -> ::cmdparse::CompletionResult<'a> {
                        let parser_0 = <super::Parser as Default>::default();
                        let parser_1 = <u8 as ::cmdparse::Parsable<CustomCtx>>::Parser::default();
                        complete!()
                    }
                }

                impl ::cmdparse::Parsable<CustomCtx> for WithConcreteCtx {
                    type Parser = WithConcreteCtxParser;
                }
            };

            assert_tokens_eq(result, expected);
        }

        #[test]
        fn with_generics() {
            let mock_context = MockCodegenContext {
                vis_modifier: syn::Visibility::Public(syn::VisPublic {
                    pub_token: syn::parse_str("pub").unwrap(),
                }),
                ..Default::default()
            };
            let generics = syn::parse2(quote! {<'a, T: Parsable<CmdParserCtx>>}).unwrap();
            let mut ctx = mock_context.context();
            ctx.context_type = Some(ContextType::Generic(Box::new(
                syn::parse2::<syn::TypeParam>(quote! {T: Send + Sync})
                    .unwrap()
                    .bounds,
            )));
            ctx.generics = &generics;
            ctx.push_parser(Parser::Explicit(
                syn::parse2(quote! {super::ParserA<'a, T>}).unwrap(),
            ));
            ctx.push_parser(Parser::Explicit(
                syn::parse2(quote! {super::ParserB<'a>}).unwrap(),
            ));
            let result = implementation(
                &format_ident!("WithGenerics"),
                &ctx,
                quote! {parse!()},
                quote! {complete!()},
            );

            let expected = quote! {
                #[derive(Default)]
                pub struct WithGenericsParser;

                impl<'a, CmdParserCtx: Send + Sync + Clone, T: Parsable<CmdParserCtx>> ::cmdparse::Parser<CmdParserCtx> for WithGenericsParser {
                    type Value = WithGenerics<'a, T>;

                    #[allow(unreachable_code)]
                    fn parse<'a>(&self, mut input: ::cmdparse::tokens::TokenStream<'a>, ctx: CmdParserCtx,) -> ::cmdparse::ParseResult<'a, Self::Value> {
                        let parser_0 = <super::ParserA<'a, T> as Default>::default();
                        let parser_1 = <super::ParserB<'a> as Default>::default();
                        parse!()
                    }
                    #[allow(unreachable_code)]
                    fn complete<'a>(&self, mut input: ::cmdparse::tokens::TokenStream<'a>, ctx: CmdParserCtx,) -> ::cmdparse::CompletionResult<'a> {
                        let parser_0 = <super::ParserA<'a, T> as Default>::default();
                        let parser_1 = <super::ParserB<'a> as Default>::default();
                        complete!()
                    }
                }

                impl<'a, CmdParserCtx: Send + Sync + Clone, T: Parsable<CmdParserCtx>> ::cmdparse::Parsable<CmdParserCtx> for WithGenerics<'a, T> {
                    type Parser = WithGenericsParser;
                }
            };

            assert_tokens_eq(result, expected);
        }
    }
}
