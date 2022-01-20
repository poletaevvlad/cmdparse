use crate::attributes::{BuildableAttributes, FieldAttributes};
use linked_hash_map::LinkedHashMap;
use quote::format_ident;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub(crate) enum Parser<'a> {
    Explicit(syn::Type),
    FromParsable(&'a syn::Type),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ParserIndex(usize);

impl ParserIndex {
    pub(crate) fn ident(self) -> syn::Ident {
        format_ident!("parser_{}", self.0)
    }
}

#[derive(Debug)]
pub(crate) enum ContextType {
    Generic(Box<syn::punctuated::Punctuated<syn::TypeParamBound, syn::token::Add>>),
    Concrete(Box<syn::Type>),
}

#[derive(Default)]
pub(crate) struct ParsableContext<'a> {
    pub(crate) context_type: Option<ContextType>,
    pub(crate) generics: syn::Generics,
    pub(crate) parsers: LinkedHashMap<Parser<'a>, ParserIndex>,
}

impl<'a> ParsableContext<'a> {
    pub(crate) fn push_parser(&mut self, parser: Parser<'a>) -> ParserIndex {
        let items_count = self.parsers.len();
        *self
            .parsers
            .entry(parser)
            .or_insert_with(|| ParserIndex(items_count))
    }
}

#[derive(Debug)]
pub(crate) enum FieldValue {
    Required {
        parser: ParserIndex,
    },
    Optional {
        parser: ParserIndex,
        default: Option<usize>,
        name: String,
    },
    Fixed {
        value: Box<syn::Expr>,
        name: String,
    },
    Default(Option<usize>),
}

pub(crate) struct Field {
    value: FieldValue,
    field_index: usize,
}

#[derive(Default)]
pub(crate) struct FieldsSet<'a> {
    idents: Vec<&'a syn::Ident>,
    defaults: Vec<syn::Expr>,
    fields: Vec<Field>,
}

impl<'a> FieldsSet<'a> {
    pub(crate) fn from_fields(
        context: &mut ParsableContext<'a>,
        fields: &'a syn::Fields,
    ) -> Result<Self, syn::Error> {
        let mut result = FieldsSet::default();

        let fields = match fields {
            syn::Fields::Named(fields) => &fields.named,
            syn::Fields::Unnamed(fields) => &fields.unnamed,
            syn::Fields::Unit => return Ok(result),
        };

        for (field_index, field) in fields.iter().enumerate() {
            if let Some(ident) = &field.ident {
                result.idents.push(ident);
            }

            let attributes = FieldAttributes::from_attributes(field.attrs.iter())?;
            let parser = match attributes.parser {
                None => Parser::FromParsable(&field.ty),
                Some(parser) => Parser::Explicit(parser),
            };

            let default = attributes.default.map(|default| {
                default.map(|default| {
                    result.defaults.push(default);
                    result.defaults.len() - 1
                })
            });

            if attributes.names.is_empty() {
                let value = match default {
                    Some(default) => FieldValue::Default(default),
                    None => FieldValue::Required {
                        parser: context.push_parser(parser),
                    },
                };
                result.fields.push(Field { value, field_index });
            } else {
                let has_parser = attributes.names.values().any(Option::is_none);
                let parser_id = if has_parser {
                    Some(context.push_parser(parser))
                } else {
                    None
                };
                for (name, value) in attributes.names {
                    let value = match value {
                        Some(value) => FieldValue::Fixed {
                            value: Box::new(value),
                            name,
                        },
                        None => FieldValue::Optional {
                            name,
                            parser: parser_id.unwrap(),
                            default: default.flatten(),
                        },
                    };
                    result.fields.push(Field { value, field_index });
                }
            }
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::{Field, FieldValue, FieldsSet, ParsableContext};
    use quote::quote;

    mod parsable_struct {
        use super::*;

        #[test]
        fn unit_struct() {
            let struct_ = quote! { struct Mock; };
            let fields = syn::parse2::<syn::ItemStruct>(struct_).unwrap().fields;
            let mut context = ParsableContext::default();

            let parsable = FieldsSet::from_fields(&mut context, &fields).unwrap();
            assert!(context.parsers.is_empty());
            assert!(parsable.fields.is_empty());
        }
        #[test]
        fn tuple_struct() {
            let struct_ = quote! { struct Mock(
                u8,
                #[cmd(parser = "CustomParser", attr(yes="true", no="false"))] bool,
                #[cmd(attr(a="10", b))] u16,
                #[cmd(attr(name), default="\"unknown\".to_string()")] String,
                #[cmd(default)] u32,
                #[cmd(default = "4")] u64,
            ); };
            let fields = syn::parse2::<syn::ItemStruct>(struct_).unwrap().fields;
            let mut context = ParsableContext::default();

            let fieldset = FieldsSet::from_fields(&mut context, &fields).unwrap();
            assert!(fieldset.idents.is_empty());
            assert_eq!(fieldset.fields.len(), 8);
            assert_fieldset(fieldset);
        }

        #[test]
        fn tuple_named() {
            let struct_ = quote! { struct Mock{
                required: u8,
                #[cmd(parser = "CustomParser", attr(yes="true", no="false"))] opt_no_entry: bool,
                #[cmd(attr(a="10", b))] opt_maybe_entry: u16,
                #[cmd(attr(name), default="\"unknown\".to_string()")] opt_only_entry: String,
                #[cmd(default)] default_only: u32,
                #[cmd(default = "4")] custom_default_only: u64,
            } };
            let fields = syn::parse2::<syn::ItemStruct>(struct_).unwrap().fields;
            let mut context = ParsableContext::default();

            let fieldset = FieldsSet::from_fields(&mut context, &fields).unwrap();

            let idents: Vec<_> = fieldset
                .idents
                .iter()
                .map(|ident| ident.to_string())
                .collect();
            assert_eq!(
                idents,
                vec![
                    "required",
                    "opt_no_entry",
                    "opt_maybe_entry",
                    "opt_only_entry",
                    "default_only",
                    "custom_default_only"
                ]
            );

            assert_fieldset(fieldset);
        }

        fn assert_fieldset(fieldset: FieldsSet) {
            let defaults: Vec<_> = fieldset
                .defaults
                .iter()
                .map(|default| quote! {#default}.to_string())
                .collect();
            assert_eq!(defaults, vec!["\"unknown\" . to_string ()", "4"]);

            let fields: HashSet<_> = fieldset.fields.iter().map(fmt_field).collect();
            let expected_fields: HashSet<_> = [
                "0, Required(0)",
                "1, Fixed(true, yes)",
                "1, Fixed(false, no)",
                "2, Fixed(10, a)",
                "2, Optional(1, None, b)",
                "3, Optional(2, Some(0), name)",
                "4, Default(None)",
                "5, Default(Some(1))",
            ]
            .iter()
            .copied()
            .map(str::to_string)
            .collect();
            assert_eq!(fields, expected_fields);
        }

        use std::{collections::HashSet, fmt::Write};

        fn fmt_field(field: &Field) -> String {
            let mut string = String::new();
            write!(&mut string, "{}, ", field.field_index).unwrap();
            match &field.value {
                FieldValue::Required { parser } => {
                    write!(&mut string, "Required({})", parser.0).unwrap();
                }
                FieldValue::Optional {
                    parser,
                    default,
                    name,
                } => write!(
                    &mut string,
                    "Optional({}, {:?}, {})",
                    parser.0, default, name
                )
                .unwrap(),
                FieldValue::Fixed { value, name } => write!(
                    &mut string,
                    "Fixed({}, {})",
                    quote! {#value}.to_string(),
                    name
                )
                .unwrap(),
                FieldValue::Default(default) => {
                    write!(&mut string, "Default({:?})", default).unwrap();
                }
            }
            string
        }
    }
}