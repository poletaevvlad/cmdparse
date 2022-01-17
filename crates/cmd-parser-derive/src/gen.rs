use crate::schema::{ContextType, ParsableContext, Parser};
use proc_macro2::TokenStream;
use quote::quote;

fn ctx_generic(ctx: &ParsableContext) -> TokenStream {
    match &ctx.context_type {
        Some(ContextType::Generic(_)) | None => quote! {CmdParserCtx},
        Some(ContextType::Concrete(ty)) => quote! {#ty},
    }
}

pub(crate) mod parsers {
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

pub(crate) mod generics {
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

#[cfg(test)]
mod tests {
    use crate::schema::{ContextType, ParsableContext, Parser};
    use proc_macro2::TokenStream;
    use quote::quote;

    fn assert_tokens_eq(stream1: TokenStream, stream2: TokenStream) {
        assert_eq!(format!("{:?}", stream1), format!("{:?}", stream2));
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
            ctx.context_type = Some(ContextType::Generic(
                syn::parse2::<syn::TypeParam>(quote! {T: Send + Sync})
                    .unwrap()
                    .bounds,
            ));

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
                context_type: Some(ContextType::Generic(
                    syn::parse2::<syn::TypeParam>(quote! {T: Send + Sync})
                        .unwrap()
                        .bounds,
                )),
                ..Default::default()
            };

            assert_tokens_eq(usage(&ctx, false), quote! {});
            assert_tokens_eq(usage(&ctx, true), quote! {<CmdParserCtx>});

            assert_tokens_eq(definition(&ctx, false), quote! {});
            assert_tokens_eq(definition(&ctx, true), quote! {<CmdParserCtx: Send + Sync>});
        }
    }
}
