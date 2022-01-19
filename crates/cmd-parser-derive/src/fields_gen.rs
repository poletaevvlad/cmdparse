use super::fields::ParserIndex;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub(crate) struct RequiredFieldView<'a> {
    index: usize,
    required_index: usize,
    parser: ParserIndex,
    name: Option<&'a syn::Ident>,
}

impl<'a> RequiredFieldView<'a> {
    fn gen_parse(&self) -> TokenStream {
        let parser_ident = self.parser.ident();
        let field_ident = format_ident!("field_{}", self.index);
        let required_index = self.required_index;

        quote! {
            #required_index => match self.#parser_ident.parse(input) {
                ::cmd_parser::ParseResult::Unrecognized(err)
                    if err.kind() == ::cmd_parser::ParseErrorKind::UnknownAttribute => {}
                ::cmd_parser::ParseResult::Unrecognized(err) | ::cmd_parser::ParseResult::Failed(error) => {
                    return ::cmd_parser::ParseResult::Failed(err)
                }
                ::cmd_parser::ParseResult::Parsed(result, remaining) => {
                    input = remaining;
                    #field_ident = Some(result);
                    first_token = false;
                    required_index += 1;
                    continue;
                },
            }
        }
    }

    fn gen_unwrap(&self) -> TokenStream {
        let field_ident = format_ident!("field_{}", self.index);
        quote! { #field_ident.unwrap(), }
    }
}

pub(crate) enum OptionalFieldType<'a> {
    Value(&'a syn::Expr),
    Parser(ParserIndex),
}

pub(crate) struct OptionalFieldView<'a> {
    index: usize,
    name: Option<&'a syn::Ident>,
    label: &'a str,
    field_type: OptionalFieldType<'a>,
    default: Option<&'a syn::Expr>,
}

impl<'a> OptionalFieldView<'a> {
    fn gen_parse(&self) -> TokenStream {
        let label = self.label;
        let field_ident = format_ident!("field_{}", self.index);
        let parse_tokens = match self.field_type {
            OptionalFieldType::Value(value) => quote! { #field_ident = Some(#value); },
            OptionalFieldType::Parser(parser) => {
                let parser_ident = parser.ident();
                quote! {
                    match self.#parser_ident.parse(remaining) {
                        ::cmd_parser::ParseResult::Parsed(result, remaining) => {
                            input = remaining;
                            #field_ident = Some(result);
                        }
                        ::cmd_parser::ParseResult::Unrecofnized(error) | ::cmd_parser::ParseReult::Failed(error) => {
                            return ParseResult::Failed(error)
                        }
                    }
                }
            }
        };

        quote! { #label => { #parse_tokens } }
    }

    fn gen_unwrap(&self) -> TokenStream {
        let field_ident = format_ident!("field_{}", self.index);
        match self.default {
            Some(value) => quote! { #field_ident.unwrap_or_else(|| #value), },
            None => quote! { #field_ident.unwrap_or_default(), },
        }
    }
}

pub(crate) struct ConstantFieldView<'a> {
    name: Option<&'a syn::Ident>,
    value: Option<&'a syn::Expr>,
}

impl<'a> ConstantFieldView<'a> {
    fn gen_unwrap(&self) -> TokenStream {
        match self.value {
            Some(value) => quote! {#value,},
            None => quote! {Default::default(),},
        }
    }
}

pub(crate) enum FieldView<'a> {
    Required(RequiredFieldView<'a>),
    Optional(OptionalFieldView<'a>),
    Constant(ConstantFieldView<'a>),
}

impl<'a> FieldView<'a> {
    fn gen_parse_required(&self) -> TokenStream {
        match self {
            FieldView::Required(required) => required.gen_parse(),
            _ => TokenStream::new(),
        }
    }

    fn gen_parse_optional(&self) -> TokenStream {
        match self {
            FieldView::Optional(optional) => optional.gen_parse(),
            _ => TokenStream::new(),
        }
    }

    fn gen_unwrap(&self) -> TokenStream {
        let (name, unwrap_tokens) = match self {
            FieldView::Required(required) => (required.name, required.gen_unwrap()),
            FieldView::Optional(optional) => (optional.name, optional.gen_unwrap()),
            FieldView::Constant(constant) => (constant.name, constant.gen_unwrap()),
        };
        match name {
            Some(name) => quote! {#name: #unwrap_tokens},
            None => unwrap_tokens,
        }
    }

    fn is_named(&self) -> bool {
        match self {
            FieldView::Required(required) => required.name.is_some(),
            FieldView::Optional(optional) => optional.name.is_some(),
            FieldView::Constant(constant) => constant.name.is_some(),
        }
    }

    fn gen_initialization(&self) -> TokenStream {
        match self {
            FieldView::Required(required) => {
                let field_ident = format_ident!("field_{}", required.index);
                quote! { let #field_ident = None; }
            }
            FieldView::Optional(optional) => {
                let field_ident = format_ident!("field_{}", optional.index);
                quote! { let #field_ident = None; }
            }
            FieldView::Constant(_) => TokenStream::new(),
        }
    }
}

fn gen_fields<'a>(
    constructor: TokenStream,
    fields: impl Iterator<Item = FieldView<'a>>,
) -> TokenStream {
    let mut initialization = TokenStream::new();
    let mut required_parsing = TokenStream::new();
    let mut optional_parsing = TokenStream::new();
    let mut unwrap_fields = TokenStream::new();
    let mut is_named = None;

    for field in fields {
        initialization.extend(field.gen_initialization());
        required_parsing.extend(field.gen_parse_required());
        optional_parsing.extend(field.gen_parse_optional());
        unwrap_fields.extend(field.gen_unwrap());
        is_named = Some(field.is_named());
    }

    let initialization = match is_named {
        Some(true) => quote! { #constructor{ #unwrap_fields } },
        Some(false) => quote! { #constructor( #unwrap_fields ) },
        None => quote! {#constructor},
    };

    quote! {
        #initialization
        let mut required_index = 0;
        let first_token = true;
        loop {
            match index {
                #required_parsing
                _ => break;
            }
            let (token, remaining) = take_token(input);
            match token {
                Token::Attribute(attr) => match &attr {
                    #optional_parsing
                    attr => {
                        let error = ParseError::unknown_attribute(attr);
                        return match first_token {
                            true => ParseResult::Unrecognized(attr)
                            false => ParseResult::Failed(attr)
                        };
                    }
                }
                Token::Text(_) => panic!("Parser failure: parse function returned unknown attribure error but the input does not begin with an attribute")
            }
            first_token = false;
        }
        ParserResult::Parsed(#initialization, input)
    }
}
