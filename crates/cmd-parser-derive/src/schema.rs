use crate::attributes::{BuildableAttributes, FieldAttributes};
use linked_hash_map::LinkedHashMap;
use quote::format_ident;
use std::collections::HashMap;

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

#[derive(Default)]
pub(crate) struct ParsableStruct<'a> {
    fields: Vec<ParserIndex>,
    names: Option<Vec<(&'a syn::Ident, usize)>>,
    required: Vec<usize>,
    optional: Vec<(String, usize, Option<syn::Expr>)>,
    defaults: HashMap<usize, syn::Expr>,
}

impl<'a> ParsableStruct<'a> {
    pub(crate) fn from_fields(
        context: &mut ParsableContext<'a>,
        fields: &'a syn::Fields,
    ) -> Result<Self, syn::Error> {
        let mut result = ParsableStruct::default();

        let fields = match fields {
            syn::Fields::Named(fields) => {
                result.names = Some(Vec::new());
                &fields.named
            }
            syn::Fields::Unnamed(fields) => &fields.unnamed,
            syn::Fields::Unit => return Ok(result),
        };

        for (field_index, field) in fields.iter().enumerate() {
            let attributes = FieldAttributes::from_attributes(field.attrs.iter())?;

            let parser = match attributes.parser {
                None => Parser::FromParsable(&field.ty),
                Some(parser) => Parser::Explicit(parser),
            };
            let parser_index = context.push_parser(parser);
            result.fields.push(parser_index);

            if attributes.names.is_empty() {
                result.required.push(field_index);
            } else {
                for (name, value) in attributes.names {
                    result.optional.push((name, field_index, value));
                }
            }
            if let Some(default) = attributes.default {
                result.defaults.insert(field_index, default);
            }

            if let Some(ref mut names) = result.names {
                // unwrap: `names` is Some if the fields are named
                names.push((field.ident.as_ref().unwrap(), result.fields.len() - 1));
            }
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::{Parser, ParserIndex};
    use quote::quote;

    mod parsable_struct {
        use quote::format_ident;

        use super::*;
        use crate::schema::{ParsableContext, ParsableStruct};

        #[test]
        fn unit_struct() {
            let struct_ = quote! { struct Mock; };
            let fields = syn::parse2::<syn::ItemStruct>(struct_).unwrap().fields;
            let mut context = ParsableContext::default();

            let parsable = ParsableStruct::from_fields(&mut context, &fields).unwrap();
            assert!(context.parsers.is_empty());
            assert!(parsable.fields.is_empty());
        }

        fn assert_parsable_struct(mut parsable: ParsableStruct<'_>, context: &ParsableContext) {
            let parsers: Vec<_> = context.parsers.iter().map(|(parser, _)| parser).collect();
            assert_eq!(parsers.len(), 3);
            assert!(
                matches!(parsers[0], Parser::FromParsable(ty) if &quote!{#ty}.to_string() == "u8")
            );
            assert!(
                matches!(parsers[1], Parser::Explicit(ty) if &quote!{#ty}.to_string() == "CustomParser")
            );
            assert!(
                matches!(parsers[2], Parser::FromParsable(ty) if &quote!{#ty}.to_string() == "String")
            );

            assert_eq!(
                parsable.fields,
                vec![ParserIndex(0), ParserIndex(1), ParserIndex(2)]
            );

            assert_eq!(parsable.required, vec![0]);

            parsable.optional.sort_by(|a, b| a.0.cmp(&b.0));
            assert_eq!(parsable.optional.len(), 3);
            assert!(matches!(&parsable.optional[0], (name, 2, None) if name.as_str() == "name"));
            assert!(
                matches!(&parsable.optional[1], (name, 1, Some(val)) if &quote!{#val}.to_string() == "false" && name.as_str() == "no")
            );
            assert!(
                matches!(&parsable.optional[2], (name, 1, Some(val)) if &quote!{#val}.to_string() == "true" && name.as_str() == "yes")
            );

            assert_eq!(parsable.defaults.len(), 1);
            assert!(
                matches!(&parsable.defaults.get(&2), Some(expr) if &quote!{#expr}.to_string() == "\"unknown\" . to_string ()")
            );
        }

        #[test]
        fn tuple_struct() {
            let struct_ = quote! { struct Mock(
                u8,
                #[cmd(parser = "CustomParser", attr(yes="true", no="false"))] bool,
                #[cmd(attr(name), default="\"unknown\".to_string()")] String
            ); };
            let fields = syn::parse2::<syn::ItemStruct>(struct_).unwrap().fields;
            let mut context = ParsableContext::default();

            let parsable = ParsableStruct::from_fields(&mut context, &fields).unwrap();
            assert!(parsable.names.is_none());
            assert_parsable_struct(parsable, &context);
        }

        #[test]
        fn tuple_named() {
            let struct_ = quote! { struct Mock{
                number: u8,
                #[cmd(parser = "CustomParser", attr(yes="true", no="false"))] boolean: bool,
                #[cmd(attr(name), default="\"unknown\".to_string()")] name: String
            } };
            let fields = syn::parse2::<syn::ItemStruct>(struct_).unwrap().fields;
            let mut context = ParsableContext::default();

            let parsable = ParsableStruct::from_fields(&mut context, &fields).unwrap();

            let ident_number = format_ident!("number");
            let ident_boolean = format_ident!("boolean");
            let ident_name = format_ident!("name");
            assert_eq!(
                parsable.names,
                Some(vec![
                    (&ident_number, 0),
                    (&ident_boolean, 1),
                    (&ident_name, 2)
                ])
            );
            assert_parsable_struct(parsable, &context);
        }
    }
}
