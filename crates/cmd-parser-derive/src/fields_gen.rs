use crate::fields::{FieldView, FieldsSet, StructType};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

impl<'a> FieldView<'a> {
    fn var_ident(&self) -> syn::Ident {
        format_ident!("field_{}", self.field_index())
    }

    fn gen_var_instantiate(&self) -> TokenStream {
        match self {
            FieldView::Default { .. } => TokenStream::new(),
            _ => {
                let var_ident = self.var_ident();
                quote! { let #var_ident = None; }
            }
        }
    }

    fn gen_parse_required(&self) -> TokenStream {
        match self {
            FieldView::Required {
                position, parser, ..
            } => {
                let var_ident = self.var_ident();
                let parser_ident = parser.ident();

                quote! {
                    #position => match self.#parser_ident.parse(input) {
                        ::cmd_parser::ParseResult::Unrecognized(err)
                            if err.kind() == ::cmd_parser::ParseErrorKind::UnknownAttribute => {}
                        ::cmd_parser::ParseResult::Unrecognized(err) | ::cmd_parser::ParseResult::Failed(error) => {
                            return ::cmd_parser::ParseResult::Failed(err)
                        }
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

    fn gen_parse_optional(&self) -> TokenStream {
        match self {
            FieldView::Optional { parser, name, .. } => {
                let parser_ident = parser.ident();
                let var_ident = self.var_ident();
                quote! {
                    #name => {
                        match self.#parser_ident.parse(remaining) {
                            ::cmd_parser::ParseResult::Parsed(result, remaining) => {
                                input = remaining;
                                #var_ident = Some(result);
                            }
                            ::cmd_parser::ParseResult::Unrecofnized(error) | ::cmd_parser::ParseReult::Failed(error) => {
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
                    }
                }
            }
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

pub(crate) fn gen_parse_struct(constructor: TokenStream, fields: FieldsSet<'_>) -> TokenStream {
    let mut initialization = TokenStream::new();
    let mut required_parsing = TokenStream::new();
    let mut optional_parsing = TokenStream::new();
    let mut unwrap_fields = TokenStream::new();

    for field in fields.fields_views() {
        initialization.extend(field.gen_var_instantiate());
        required_parsing.extend(field.gen_parse_required());
        optional_parsing.extend(field.gen_parse_optional());

        let unwrap_value = field.gen_unwrap();
        let unwrap_field = match fields.get_ident(field.field_index()) {
            Some(ident) => quote! { #ident: #unwrap_value, },
            None => quote! { #unwrap_value, },
        };
        unwrap_fields.extend(unwrap_field);
    }

    let result_struct = match fields.struct_type() {
        StructType::Named => quote! { #constructor{ #unwrap_fields } },
        StructType::Unnamed => quote! { #constructor( #unwrap_fields ) },
        StructType::Unit => quote! {#constructor},
    };

    quote! {
        #initialization
        let mut required_index = 0;
        let first_token = true;
        loop {
            match required_index {
                #required_parsing
                _ => break,
            }
            let (token, remaining) = ::cmd_parser::tokens::take_token(input);
            match token {
                ::cmd_parser::tokens::Token::Attribute(attr) => match attr {
                    #optional_parsing
                    attr => {
                        let error = ::cmd_parser::ParseError::unknown_attribute(attr);
                        return match first_token {
                            true => ::cmd_parser::ParseResult::Unrecognized(error),
                            false => ::cmd_parser::ParseResult::Failed(error),
                        };
                    }
                }
                ::cmd_parser::tokens::Token::Text(_) => {
                    panic!("Parser failure: parse function returned unknown attribure error but the input does not begin with an attribute");
                }
            }
            first_token = false;
        }
        ::cmd_parser::ParseResult::Parsed(#result_struct, input)
    }
}
