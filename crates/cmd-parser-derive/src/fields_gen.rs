use crate::{
    context::CodegenContext,
    fields::{FieldView, FieldsSet, StructType},
};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::HashSet;

impl<'a> FieldView<'a> {
    fn var_ident(&self) -> syn::Ident {
        format_ident!("field_{}", self.field_index())
    }

    fn gen_var_instantiate(&self) -> TokenStream {
        match self {
            FieldView::Default { .. } => TokenStream::new(),
            _ => {
                let var_ident = self.var_ident();
                quote! { let mut #var_ident = None; }
            }
        }
    }

    fn gen_parse_required(&self, ctx: &CodegenContext) -> TokenStream {
        match self {
            FieldView::Required {
                position, parser, ..
            } => {
                let var_ident = self.var_ident();
                let parser_ident = parser.ident();
                let unrecognized_variant = match *position {
                    0 => quote! { return Err(unrecognized.into()) },
                    _ => quote! { unrecognized },
                };
                let parse_ctx = ctx.parse_context_ident();

                quote! {
                    #position => match input.with_nested(|input| ::cmd_parser::Parser::<#parse_ctx>::parse(&self.#parser_ident, input)) {
                        Ok((result, remaining)) => {
                            input = remaining;
                            #var_ident = Some(result);
                            first_token = false;
                            required_index += 1;
                            continue
                        }
                        Err(error @ ::cmd_parser::ParseFailure::Error(_)) => return Err(error),
                        Err(::cmd_parser::ParseFailure::Unrecognized(unrecognized)) => #unrecognized_variant,
                    }
                }
            }
            _ => TokenStream::new(),
        }
    }

    fn gen_complete_required(&self, ctx: &CodegenContext) -> TokenStream {
        match self {
            FieldView::Required {
                parser, position, ..
            } => {
                let parse_ctx = ctx.parse_context_ident();
                let parser_ident = parser.ident();

                quote! {
                    #position => match ::cmd_parser::Parser::<#parse_ctx>::complete(&self.#parser_ident, input) {
                        ::cmd_parser::CompletionResult::Consumed(remaining) => {
                            input = remaining;
                            required_index += 1;
                            first_token = false;
                            continue;
                        }
                        suggestions @ ::cmd_parser::CompletionResult::Suggestions(_) => return suggestions,
                        ::cmd_parser::CompletionResult::Unrecognized => (),

                    }
                }
            }
            _ => TokenStream::new(),
        }
    }

    fn gen_parse_optional(&self, ctx: &CodegenContext) -> TokenStream {
        match self {
            FieldView::Optional { parser, name, .. } => {
                let parser_ident = parser.ident();
                let var_ident = self.var_ident();
                let parse_ctx = ctx.parse_context_ident();
                quote! {
                    #name => {
                        let (result, remaining) = unexpected.remaining().with_nested(|input| {
                            ::cmd_parser::Parser::<#parse_ctx>::parse(&self.#parser_ident, input)
                        })?;
                        input = remaining;
                        #var_ident = Some(result);
                    }
                }
            }
            FieldView::Fixed { value, name, .. } => {
                let var_ident = self.var_ident();
                quote! {
                    #name => {
                        #var_ident = Some(#value);
                        input = *unexpected.remaining();
                    }
                }
            }
            _ => TokenStream::new(),
        }
    }

    fn gen_complete_optional(&self, ctx: &CodegenContext) -> TokenStream {
        match self {
            FieldView::Optional { parser, name, .. } => {
                let parser_ident = parser.ident();
                let parse_ctx = ctx.parse_context_ident();
                quote! {
                    #name => {
                        match ::cmd_parser::Parser::<#parse_ctx>::complete(&self.#parser_ident, ::cmd_parser::tokens::skip_ws(remaining)) {
                            ::cmd_parser::CompletionResult::Consumed(remaining) => {
                                input = remaining;
                                first_token = false;
                                continue;
                            }
                            suggestions @ ::cmd_parser::CompletionResult::Suggestions(_) => return suggestions,
                            ::cmd_parser::CompletionResult::Unrecognized => return ::cmd_parser::CompletionResult::empty(),

                        }
                    }
                }
            }
            FieldView::Fixed { name, .. } => quote! { #name => continue, },
            _ => TokenStream::new(),
        }
    }

    fn gen_unwrap(&self) -> TokenStream {
        match self {
            FieldView::Required { .. } => {
                let var_ident = self.var_ident();
                quote! { #var_ident.unwrap() }
            }
            FieldView::Optional { default: None, .. } | FieldView::Fixed { default: None, .. } => {
                let var_ident = self.var_ident();
                quote! { #var_ident.unwrap_or_default() }
            }
            FieldView::Optional {
                default: Some(default),
                ..
            }
            | FieldView::Fixed {
                default: Some(default),
                ..
            } => {
                let var_ident = self.var_ident();
                quote! { #var_ident.unwrap_or_else(|| #default) }
            }
            FieldView::Default { default: None, .. } => quote! { Default::default() },
            FieldView::Default {
                default: Some(default),
                ..
            } => quote! { #default },
        }
    }
}

pub(crate) fn gen_parse_struct(
    constructor: TokenStream,
    ctx: &CodegenContext,
    fields: &FieldsSet<'_>,
) -> TokenStream {
    let mut initialization = TokenStream::new();
    let mut required_parsing = TokenStream::new();
    let mut optional_parsing = TokenStream::new();
    let mut unwrap_fields = TokenStream::new();

    let mut required_count: usize = 0;
    let mut initialized_field_indices = HashSet::new();

    for field in fields.fields_views() {
        required_parsing.extend(field.gen_parse_required(ctx));
        optional_parsing.extend(field.gen_parse_optional(ctx));

        let index = field.field_index();
        if !initialized_field_indices.contains(&index) {
            initialization.extend(field.gen_var_instantiate());

            let unwrap_value = field.gen_unwrap();
            let unwrap_field = match fields.get_ident(field.field_index()) {
                Some(ident) => quote! { #ident: #unwrap_value, },
                None => quote! { #unwrap_value, },
            };
            unwrap_fields.extend(unwrap_field);
            initialized_field_indices.insert(index);
        }

        if matches!(field, FieldView::Required { .. }) {
            required_count += 1;
        }
    }

    let result_struct = match fields.struct_type() {
        StructType::Named => quote! { #constructor{ #unwrap_fields } },
        StructType::Unnamed => quote! { #constructor( #unwrap_fields ) },
        StructType::Unit => quote! {#constructor},
    };

    quote! {
        #initialization
        let mut required_index = 0;
        let mut first_token = true;
        loop {
            let unexpected = match required_index {
                #required_parsing
                _ => match input.take() {
                    None | Some(Err(_)) => break,
                    Some(Ok((token, _))) if token.value().is_text() => break,
                    Some(Ok((token, remaining))) => ::cmd_parser::error::UnrecognizedToken::new(token, remaining),
                }
            };
            match unexpected.token().value() {
                ::cmd_parser::tokens::TokenValue::Text(_) => return Err(unexpected.into()),
                ::cmd_parser::tokens::TokenValue::Attribute(attribute) => {
                    let attribute = attribute.parse_string();
                    match ::std::borrow::Borrow::<str>::borrow(&attribute) {
                        #optional_parsing
                        _ if required_index >= #required_count => break,
                        _ if first_token => return Err(unexpected.into()),
                        _ => return Err(unexpected.into_error().into()),
                    }
                }
            }
            first_token = false;
        }
        Ok((#result_struct, input))
    }
}

pub(crate) fn gen_complete_struct(_ctx: &CodegenContext, _fields: &FieldsSet<'_>) -> TokenStream {
    // let mut required_complete = TokenStream::new();
    // let mut optional_complete = TokenStream::new();
    // let mut required_count: usize = 0;
    // let mut attribute_names = Vec::new();

    // for field in fields.fields_views() {
    //     required_complete.extend(field.gen_complete_required(ctx));
    //     optional_complete.extend(field.gen_complete_optional(ctx));
    //     if matches!(field, FieldView::Required { .. }) {
    //         required_count += 1;
    //     }

    //     match field {
    //         FieldView::Optional { name, .. } | FieldView::Fixed { name, .. } => {
    //             attribute_names.push(name);
    //         }
    //         _ => {}
    //     }
    // }
    // attribute_names.sort_unstable();

    quote! {
        todo!();
        /*const ATTRIBUTE_NAMES: &[&str] = &[#(#attribute_names),*];
        let mut required_index = 0;
        let mut first_token = true;
        loop {
            match required_index {
                #required_complete
                _ => (),
            }
            if let Some(attribute) = input.strip_prefix("--") {
                let (attribute, remaining) = ::cmd_parser::tokens::take_string(attribute);
                if remaining.is_empty() {
                    let suggestions = ::cmd_parser::tokens::complete_variants(&attribute, ATTRIBUTE_NAMES);
                    if suggestions.is_empty() && required_index >= #required_count {
                        return ::cmd_parser::CompletionResult::Consumed(input);
                    } else if suggestions.is_empty() && first_token {
                        return ::cmd_parser::CompletionResult::Unrecognized;
                    }
                    return ::cmd_parser::CompletionResult::Suggestions(suggestions);
                }

                match ::std::borrow::Borrow::<str>::borrow(&attribute) {
                    #optional_complete
                    _ if required_index >= #required_count => return ::cmd_parser::CompletionResult::Consumed(input),
                    _ if first_token => return ::cmd_parser::CompletionResult::Unrecognized,
                    _ => return ::cmd_parser::CompletionResult::empty(),
                }
            } else if required_index >= #required_count {
                return ::cmd_parser::CompletionResult::Consumed(input);
            } else {
                return ::cmd_parser::CompletionResult::empty();
            }
            first_token = false;
        }*/
    }
}
