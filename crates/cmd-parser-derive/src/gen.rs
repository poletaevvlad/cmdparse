use crate::schema::{ContextType, ParsableContext, Parser};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

fn ctx_generic(ctx: &ParsableContext) -> TokenStream {
    match &ctx.context_type {
        Some(ContextType::Generic(_)) | None => quote! {CmdParserCtx},
        Some(ContextType::Concrete(ty)) => quote! {#ty},
    }
}

mod parsers {
    use super::*;

    pub(crate) fn definition(ctx: &ParsableContext) -> TokenStream {
        let generic = ctx_generic(ctx);
        let definitions = ctx.parsers.iter().map(|(parser, index)| {
            let ident = index.ident();
            match parser {
                Parser::Explicit(explicit) => quote! { #ident: #explicit },
                Parser::FromParsable(ty) => {
                    quote! { #ident: <#ty as ::cmd_parser::Parsable<#generic>>::Parser }
                }
            }
        });
        quote! { #(#definitions,)* }
    }

    pub(crate) fn initialization(ctx: &ParsableContext) -> TokenStream {
        let generic = ctx_generic(ctx);
        let initializations = ctx.parsers.iter().map(|(parser, index)| {
            let ident = index.ident();
            match parser {
                Parser::Explicit(explicit) => quote! {
                    #ident: <#explicit as ::cmd_parser::Parser<#generic>>::create(ctx)
                },
                Parser::FromParsable(ty) => quote! {
                    #ident: <#ty as ::cmd_parser::Parsable<#generic>>::new_parser(ctx)
                },
            }
        });
        quote! { #(#initializations,)* }
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

    pub(crate) fn context_usage(ctx: &ParsableContext) -> TokenStream {
        let generic = ctx_generic(ctx);
        quote! {<#generic>}
    }

    pub(crate) fn usage(ctx: &ParsableContext, include_ctx: bool) -> TokenStream {
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

    pub(crate) fn definition(ctx: &ParsableContext, include_ctx: bool) -> TokenStream {
        let mut params = Vec::with_capacity(ctx.generics.params.len() + 1);
        params.extend(ctx.generics.lifetimes().map(|lifetime| {
            quote! {#lifetime}
        }));

        if include_ctx {
            match ctx.context_type {
                Some(ContextType::Generic(ref bounds)) => {
                    params.push(quote! {CmdParserCtx: #bounds});
                }
                Some(ContextType::Concrete(_)) => {}
                None => params.push(quote! {CmdParserCtx}),
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
    ctx: &ParsableContext,
    parse_impl: TokenStream,
    complete_impl: TokenStream,
) -> TokenStream {
    let parser_name = format_ident!("{}Parser", type_name);

    let parsers_definition = parsers::definition(ctx);
    let parsers_initialization = parsers::initialization(ctx);
    let where_clause = ctx.generics.where_clause.as_ref();
    let ctx_generic = ctx_generic(ctx);

    let type_generics = generics::usage(ctx, false);
    let ctx_generics = generics::context_usage(ctx);
    let trait_generics = generics::definition(ctx, true);
    let parser_struct_generics = generics::definition(ctx, !ctx.parsers.is_empty());
    let parser_usage_generics = generics::usage(ctx, !ctx.parsers.is_empty());

    quote! {
        struct #parser_struct_generics #parser_name #where_clause { #parsers_definition }

        impl #trait_generics ::cmd_parser::Parser #ctx_generics for #parser_name #parser_usage_generics #where_clause {
            type Value = #type_name #type_generics;

            fn create(ctx: #ctx_generic) -> Self {
                #parser_name { #parsers_initialization }
            }

            fn parse<'a>(&self, input: &'a str) -> ::cmd_parser::ParseResult<'a, Self::Value> {
                #parse_impl
            }

            fn complete<'a>(&self, input: &'a str) -> ::cmd_parser::CompletionResult<'a> {
                #complete_impl
            }
        }

        impl #trait_generics ::cmd_parser::Parsable #ctx_generics for #type_name #type_generics #where_clause {
            type Parser = #parser_name #parser_usage_generics;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::schema::{ContextType, ParsableContext, Parser};
    use proc_macro2::TokenStream;
    use quote::quote;

    fn assert_tokens_eq(stream1: TokenStream, stream2: TokenStream) {
        let stream1_formatted = format!("{}", stream1).replace(" ", "");
        let stream2_formatted = format!("{}", stream2).replace(" ", "");
        assert_eq!(stream1_formatted, stream2_formatted);
    }

    mod parsers {
        use super::super::parsers::{definition, initialization};
        use super::*;

        #[test]
        fn empty() {
            let ctx = ParsableContext::default();

            let defintion = definition(&ctx);
            let definition_expected = quote! {};
            assert_tokens_eq(defintion, definition_expected);

            let initialization = initialization(&ctx);
            let initialization_expected = quote! {};
            assert_tokens_eq(initialization, initialization_expected);
        }

        #[test]
        fn with_parsers() {
            let ty: syn::Type = syn::parse2(quote! {u8}).unwrap();

            let mut ctx = ParsableContext::default();
            ctx.push_parser(Parser::Explicit(
                syn::parse2(quote! {super::Parser}).unwrap(),
            ));
            ctx.push_parser(Parser::FromParsable(&ty));

            let definition = definition(&ctx);
            let definition_expected = quote! {
                parser_0: super::Parser,
                parser_1: <u8 as ::cmd_parser::Parsable<CmdParserCtx>>::Parser,
            };
            assert_tokens_eq(definition, definition_expected);

            let initialization = initialization(&ctx);
            let initialization_expected = quote! {
                parser_0: <super::Parser as ::cmd_parser::Parser<CmdParserCtx>>::create(ctx),
                parser_1: <u8 as ::cmd_parser::Parsable<CmdParserCtx>>::new_parser(ctx),
            };
            assert_tokens_eq(initialization, initialization_expected);
        }

        #[test]
        fn with_parsers_and_concrete_generics() {
            let ty: syn::Type = syn::parse2(quote! {u8}).unwrap();

            let mut ctx = ParsableContext {
                context_type: Some(ContextType::Concrete(
                    syn::parse2(quote! {CustomCtx}).unwrap(),
                )),
                ..Default::default()
            };
            ctx.push_parser(Parser::Explicit(
                syn::parse2(quote! {super::Parser}).unwrap(),
            ));
            ctx.push_parser(Parser::FromParsable(&ty));

            let definition = definition(&ctx);
            let definition_expected = quote! {
                parser_0: super::Parser,
                parser_1: <u8 as ::cmd_parser::Parsable<CustomCtx>>::Parser,
            };
            assert_tokens_eq(definition, definition_expected);

            let initialization = initialization(&ctx);
            let initialization_expected = quote! {
                parser_0: <super::Parser as ::cmd_parser::Parser<CustomCtx>>::create(ctx),
                parser_1: <u8 as ::cmd_parser::Parsable<CustomCtx>>::new_parser(ctx),
            };
            assert_tokens_eq(initialization, initialization_expected);
        }
    }

    mod generics {
        use super::super::generics::{definition, usage};
        use super::*;

        fn make_context_with_generic() -> ParsableContext<'static> {
            ParsableContext {
                generics: syn::parse2(
                    quote! {<'a, 'b: 'a, T: Iterator<Item = u8>, const X: u8 = 5>},
                )
                .unwrap(),
                ..Default::default()
            }
        }

        #[test]
        fn usage_null_context_type() {
            let ctx = make_context_with_generic();
            assert_tokens_eq(usage(&ctx, false), quote! {<'a, 'b, T, X>});
            assert_tokens_eq(usage(&ctx, true), quote! {<'a, 'b, CmdParserCtx, T, X>});

            assert_tokens_eq(
                definition(&ctx, false),
                quote! {<'a, 'b: 'a, T:Iterator<Item = u8>, const X: u8 = 5>},
            );
            assert_tokens_eq(
                definition(&ctx, true),
                quote! {<'a, 'b: 'a, CmdParserCtx, T:Iterator<Item = u8>, const X: u8 = 5>},
            );
        }

        #[test]
        fn usage_null_context_type_no_generics() {
            let ctx = ParsableContext::default();
            assert_tokens_eq(usage(&ctx, false), quote! {});
            assert_tokens_eq(usage(&ctx, true), quote! {<CmdParserCtx>});

            assert_tokens_eq(definition(&ctx, false), quote! {});
            assert_tokens_eq(definition(&ctx, true), quote! {<CmdParserCtx>});
        }

        #[test]
        fn usage_concrete_context() {
            let mut ctx = make_context_with_generic();
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
        fn usage_concrete_context_no_generics() {
            let ctx = ParsableContext {
                context_type: Some(ContextType::Concrete(syn::parse2(quote! {u8}).unwrap())),
                ..Default::default()
            };
            assert_tokens_eq(usage(&ctx, false), quote! {});
            assert_tokens_eq(usage(&ctx, true), quote! {});

            assert_tokens_eq(definition(&ctx, false), quote! {});
            assert_tokens_eq(definition(&ctx, true), quote! {});
        }

        #[test]
        fn usage_generic_context() {
            let mut ctx = make_context_with_generic();
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
                quote! {<'a, 'b: 'a, CmdParserCtx: Send + Sync, T:Iterator<Item = u8>, const X: u8 = 5>},
            );
        }

        #[test]
        fn usage_generic_context_no_generics() {
            let ctx = ParsableContext {
                context_type: Some(ContextType::Generic(Box::new(
                    syn::parse2::<syn::TypeParam>(quote! {T: Send + Sync})
                        .unwrap()
                        .bounds,
                ))),
                ..Default::default()
            };

            assert_tokens_eq(usage(&ctx, false), quote! {});
            assert_tokens_eq(usage(&ctx, true), quote! {<CmdParserCtx>});

            assert_tokens_eq(definition(&ctx, false), quote! {});
            assert_tokens_eq(definition(&ctx, true), quote! {<CmdParserCtx: Send + Sync>});
        }
    }

    mod sceleton {
        use super::super::implementation;
        use super::*;
        use quote::format_ident;

        #[test]
        fn no_parsers() {
            let ctx = ParsableContext::default();
            let result = implementation(
                &format_ident!("NoFields"),
                &ctx,
                quote! {parse!()},
                quote! {complete!()},
            );

            let expected = quote! {
                struct NoFieldsParser {}

                impl<CmdParserCtx> ::cmd_parser::Parser<CmdParserCtx> for NoFieldsParser {
                    type Value = NoFields;

                    fn create(ctx: CmdParserCtx) -> Self {
                        NoFieldsParser{}
                    }
                    fn parse<'a>(&self, input: &'a str) -> ::cmd_parser::ParseResult<'a, Self::Value> { parse!() }
                    fn complete<'a>(&self, input: &'a str) -> ::cmd_parser::CompletionResult<'a> { complete!() }
                }

                impl<CmdParserCtx> ::cmd_parser::Parsable<CmdParserCtx> for NoFields {
                    type Parser = NoFieldsParser;
                }
            };

            assert_tokens_eq(result, expected);
        }

        #[test]
        fn parsers_concrete_ctx() {
            let ty: syn::Type = syn::parse2(quote! {u8}).unwrap();

            let mut ctx = ParsableContext {
                context_type: Some(ContextType::Concrete(
                    syn::parse2(quote! {CustomCtx}).unwrap(),
                )),
                ..Default::default()
            };
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
                struct WithConcreteCtxParser {
                    parser_0: super::Parser,
                    parser_1: <u8 as ::cmd_parser::Parsable<CustomCtx>>::Parser,
                }

                impl ::cmd_parser::Parser<CustomCtx> for WithConcreteCtxParser {
                    type Value = WithConcreteCtx;

                    fn create(ctx: CustomCtx) -> Self {
                        WithConcreteCtxParser{
                            parser_0: <super::Parser as ::cmd_parser::Parser<CustomCtx>>::create(ctx),
                            parser_1: <u8 as ::cmd_parser::Parsable<CustomCtx>>::new_parser(ctx),
                        }
                    }
                    fn parse<'a>(&self, input: &'a str) -> ::cmd_parser::ParseResult<'a, Self::Value> { parse!() }
                    fn complete<'a>(&self, input: &'a str) -> ::cmd_parser::CompletionResult<'a> { complete!() }
                }

                impl ::cmd_parser::Parsable<CustomCtx> for WithConcreteCtx {
                    type Parser = WithConcreteCtxParser;
                }
            };

            assert_tokens_eq(result, expected);
        }

        #[test]
        fn with_generics() {
            let mut ctx = ParsableContext {
                context_type: Some(ContextType::Generic(Box::new(
                    syn::parse2::<syn::TypeParam>(quote! {T: Send + Sync})
                        .unwrap()
                        .bounds,
                ))),
                generics: syn::parse2(quote! {<'a, T: Parsable<CmdParserCtx>>}).unwrap(),
                ..Default::default()
            };
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
                struct<'a, CmdParserCtx: Send + Sync, T: Parsable<CmdParserCtx>> WithGenericsParser {
                    parser_0: super::ParserA<'a, T>,
                    parser_1: super::ParserB<'a>,
                }

                impl<'a, CmdParserCtx: Send + Sync, T: Parsable<CmdParserCtx>> ::cmd_parser::Parser<CmdParserCtx> for WithGenericsParser<'a, CmdParserCtx, T> {
                    type Value = WithGenerics<'a, T>;

                    fn create(ctx: CmdParserCtx) -> Self {
                        WithGenericsParser{
                            parser_0: <super::ParserA<'a, T> as ::cmd_parser::Parser<CmdParserCtx>>::create(ctx),
                            parser_1: <super::ParserB<'a> as ::cmd_parser::Parser<CmdParserCtx>>::create(ctx),
                        }
                    }
                    fn parse<'a>(&self, input: &'a str) -> ::cmd_parser::ParseResult<'a, Self::Value> { parse!() }
                    fn complete<'a>(&self, input: &'a str) -> ::cmd_parser::CompletionResult<'a> { complete!() }
                }

                impl<'a, CmdParserCtx: Send + Sync, T: Parsable<CmdParserCtx>> ::cmd_parser::Parsable<CmdParserCtx> for WithGenerics<'a, T> {
                    type Parser = WithGenericsParser<'a, CmdParserCtx, T>;
                }
            };

            assert_tokens_eq(result, expected);
        }
    }
}
