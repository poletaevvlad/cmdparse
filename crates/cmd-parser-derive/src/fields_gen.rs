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
                let unrecognized_variant = if *position == 0 {
                    quote! {::cmd_parser::ParseResult::UnrecognizedVariant(variant)}
                } else {
                    quote! {::cmd_parser::ParseResult::Failed(::cmd_parser::ParseError::unknown_variant(variant))}
                };
                let parse_ctx = ctx.parse_context_ident();

                quote! {
                    #position => match ::cmd_parser::Parser::<#parse_ctx>::parse(&self.#parser_ident, input) {
                        ::cmd_parser::ParseResult::UnrecognizedAttribute(attr, remaining) => (attr, remaining),
                        ::cmd_parser::ParseResult::UnrecognizedVariant(variant) => return #unrecognized_variant,
                        ::cmd_parser::ParseResult::Failed(error) => return ::cmd_parser::ParseResult::Failed(error),
                        ::cmd_parser::ParseResult::Parsed(result, remaining) => {
                            input = remaining;
                            #var_ident = Some(result);
                            first_token = false;
                            required_index += 1;
                            continue;
                        },
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
                        match ::cmd_parser::Parser::<#parse_ctx>::parse(&self.#parser_ident, remaining) {
                            ::cmd_parser::ParseResult::Parsed(result, remaining) => {
                                input = remaining;
                                #var_ident = Some(result);
                            }
                            ::cmd_parser::ParseResult::UnrecognizedAttribute(attr, _) =>
                                return ::cmd_parser::ParseResult::Failed(::cmd_parser::ParseError::unknown_attribute(attr)),
                            ::cmd_parser::ParseResult::UnrecognizedVariant(variant) =>
                                return ::cmd_parser::ParseResult::Failed(::cmd_parser::ParseError::unknown_variant(variant)),
                            ::cmd_parser::ParseResult::Failed(error) => {
                                return ParseResult::Failed(error)
                            }
                        }
                    }
                }
            }
            FieldView::Fixed { value, name, .. } => {
                let var_ident = self.var_ident();
                quote! {
                    #name => {
                        #var_ident = Some(#value);
                        input = remaining;
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
            let (attr, remaining) = match required_index {
                #required_parsing
                _ => {
                    let (token, remaining) = ::cmd_parser::tokens::take_token(input);
                    match token {
                        ::cmd_parser::tokens::Token::Text(_) => break,
                        ::cmd_parser::tokens::Token::Attribute(attr) => (attr, remaining),
                    }
                }
            };
            match std::borrow::Borrow::<str>::borrow(&attr) {
                #optional_parsing
                _ if required_index >= #required_count => break,
                _ if first_token => return ::cmd_parser::ParseResult::UnrecognizedAttribute(attr, remaining),
                _ => return ::cmd_parser::ParseResult::Failed(::cmd_parser::ParseError::unknown_attribute(attr)),
            }
            first_token = false;
        }
        ::cmd_parser::ParseResult::Parsed(#result_struct, input)
    }
}

pub(crate) fn gen_complete_struct(ctx: &CodegenContext, fields: &FieldsSet<'_>) -> TokenStream {
    let mut required_complete = TokenStream::new();
    let mut optional_complete = TokenStream::new();
    let mut required_count: usize = 0;
    let mut attribute_names = Vec::new();

    for field in fields.fields_views() {
        required_complete.extend(field.gen_complete_required(ctx));
        optional_complete.extend(field.gen_complete_optional(ctx));
        if matches!(field, FieldView::Required { .. }) {
            required_count += 1;
        }

        match field {
            FieldView::Optional { name, .. } | FieldView::Fixed { name, .. } => {
                attribute_names.push(name);
            }
            _ => {}
        }
    }
    attribute_names.sort_unstable();

    quote! {
        const ATTRIBUTE_NAMES: &[&str] = &[#(#attribute_names),*];
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
        }
    }
}
