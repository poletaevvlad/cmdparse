use crate::attributes::{BuildableAttributes, FieldAttributes};
use crate::context::{CodegenContext, Parser, ParserIndex};
use std::collections::HashMap;

pub(crate) enum FieldView<'a> {
    Required {
        field_index: usize,
        parser: ParserIndex,
        position: usize,
    },
    Optional {
        field_index: usize,
        parser: ParserIndex,
        default: Option<&'a syn::Expr>,
        name: &'a str,
    },
    Fixed {
        field_index: usize,
        value: &'a syn::Expr,
        name: &'a str,
        default: Option<&'a syn::Expr>,
    },
    Default {
        field_index: usize,
        default: Option<&'a syn::Expr>,
    },
}

impl<'a> FieldView<'a> {
    pub(crate) fn field_index(&self) -> usize {
        match self {
            FieldView::Required { field_index, .. } => *field_index,
            FieldView::Optional { field_index, .. } => *field_index,
            FieldView::Fixed { field_index, .. } => *field_index,
            FieldView::Default { field_index, .. } => *field_index,
        }
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
        default: Option<usize>,
    },
    Default(Option<usize>),
}

pub(crate) struct Field {
    value: FieldValue,
    field_index: usize,
    alias_values: HashMap<String, syn::Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum StructType {
    Unit,
    Named,
    Unnamed,
}

#[derive(Default)]
pub(crate) struct FieldsSet<'a> {
    idents: Vec<&'a syn::Ident>,
    defaults: Vec<syn::Expr>,
    fields: Vec<Field>,
}

impl<'a> FieldsSet<'a> {
    pub(crate) fn from_fields(
        context: &mut CodegenContext<'a>,
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
                Some(parser) => Parser::Explicit(Box::new(parser)),
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
                result.fields.push(Field {
                    value,
                    field_index,
                    alias_values: attributes.alias_values,
                });
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
                            default: default.flatten(),
                        },
                        None => FieldValue::Optional {
                            name,
                            parser: parser_id.unwrap(),
                            default: default.flatten(),
                        },
                    };
                    result.fields.push(Field {
                        value,
                        field_index,
                        alias_values: attributes.alias_values.clone(),
                    });
                }
            }
        }

        Ok(result)
    }

    pub(crate) fn struct_type(&self) -> StructType {
        if self.fields.is_empty() {
            StructType::Unit
        } else if self.idents.is_empty() {
            StructType::Unnamed
        } else {
            StructType::Named
        }
    }

    pub(crate) fn get_ident(&self, index: usize) -> Option<&syn::Ident> {
        self.idents.get(index).copied()
    }

    pub(crate) fn fields_views<'b: 'a>(
        &'b self,
        variant: Option<&'b str>,
    ) -> impl Iterator<Item = FieldView<'b>> + 'b {
        let mut required_position = 0;
        self.fields.iter().map(move |field| {
            let field_index = field.field_index;
            if let Some(variant) = variant {
                if let Some(value) = field.alias_values.get(variant) {
                    return FieldView::Default {
                        field_index,
                        default: Some(value),
                    };
                }
            }
            match &field.value {
                FieldValue::Required { parser } => {
                    required_position += 1;
                    FieldView::Required {
                        field_index,
                        parser: *parser,
                        position: required_position - 1,
                    }
                }
                FieldValue::Optional {
                    parser,
                    default,
                    name,
                } => FieldView::Optional {
                    field_index,
                    parser: *parser,
                    default: default.map(|index| &self.defaults[index]),
                    name,
                },
                FieldValue::Fixed {
                    value,
                    name,
                    default,
                } => FieldView::Fixed {
                    field_index,
                    value: value.as_ref(),
                    name,
                    default: default.map(|index| &self.defaults[index]),
                },
                FieldValue::Default(default) => FieldView::Default {
                    field_index,
                    default: default.map(|index| &self.defaults[index]),
                },
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{Field, FieldValue, FieldsSet, StructType};
    use crate::context::MockCodegenContext;
    use quote::quote;

    mod parsable_struct {
        use super::*;

        #[test]
        fn unit_struct() {
            let struct_ = quote! { struct Mock; };
            let fields = syn::parse2::<syn::ItemStruct>(struct_).unwrap().fields;
            let mock_context = MockCodegenContext::default();
            let mut context = mock_context.context();

            let fieldset = FieldsSet::from_fields(&mut context, &fields).unwrap();
            assert!(context.parsers.is_empty());
            assert!(fieldset.fields.is_empty());
            assert_eq!(fieldset.struct_type(), StructType::Unit);
        }

        #[test]
        fn tuple_struct() {
            let struct_ = quote! { struct Mock(
                u8,
                #[cmd(parser = "CustomParser", attr(yes="true", no="false"))] bool,
                #[cmd(attr(a="10", b), default="6")] u16,
                #[cmd(attr(name), default="\"unknown\".to_string()")] String,
                #[cmd(default)] u32,
                #[cmd(default = "4")] u64,
            ); };
            let fields = syn::parse2::<syn::ItemStruct>(struct_).unwrap().fields;
            let mock_context = MockCodegenContext::default();
            let mut context = mock_context.context();

            let fieldset = FieldsSet::from_fields(&mut context, &fields).unwrap();
            assert!(fieldset.idents.is_empty());
            assert_eq!(fieldset.fields.len(), 8);
            assert_eq!(fieldset.struct_type(), StructType::Unnamed);
            assert_fieldset(fieldset);
        }

        #[test]
        fn tuple_named() {
            let struct_ = quote! { struct Mock{
                required: u8,
                #[cmd(parser = "CustomParser", attr(yes="true", no="false"))] opt_no_entry: bool,
                #[cmd(attr(a="10", b), default="6")] opt_maybe_entry: u16,
                #[cmd(attr(name), default="\"unknown\".to_string()")] opt_only_entry: String,
                #[cmd(default)] default_only: u32,
                #[cmd(default = "4")] custom_default_only: u64,
            } };
            let fields = syn::parse2::<syn::ItemStruct>(struct_).unwrap().fields;
            let mock_context = MockCodegenContext::default();
            let mut context = mock_context.context();

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

            assert_eq!(fieldset.struct_type(), StructType::Named);
            assert_fieldset(fieldset);
        }

        fn assert_fieldset(fieldset: FieldsSet) {
            let defaults: Vec<_> = fieldset
                .defaults
                .iter()
                .map(|default| quote! {#default}.to_string())
                .collect();
            assert_eq!(defaults, vec!["6", "\"unknown\" . to_string ()", "4"]);

            let fields: HashSet<_> = fieldset.fields.iter().map(fmt_field).collect();
            let expected_fields: HashSet<_> = [
                "0, Required(ParserIndex(0))",
                "1, Fixed(true, None, yes)",
                "1, Fixed(false, None, no)",
                "2, Fixed(10, Some(0), a)",
                "2, Optional(ParserIndex(1), Some(0), b)",
                "3, Optional(ParserIndex(2), Some(1), name)",
                "4, Default(None)",
                "5, Default(Some(2))",
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
                    write!(&mut string, "Required({:?})", parser).unwrap();
                }
                FieldValue::Optional {
                    parser,
                    default,
                    name,
                } => write!(
                    &mut string,
                    "Optional({:?}, {:?}, {})",
                    parser, default, name
                )
                .unwrap(),
                FieldValue::Fixed {
                    value,
                    name,
                    default,
                } => write!(
                    &mut string,
                    "Fixed({}, {:?}, {})",
                    quote! {#value},
                    default,
                    name,
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
