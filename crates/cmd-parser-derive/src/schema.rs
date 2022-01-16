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

#[derive(Default)]
pub(crate) struct ParsableContext<'a> {
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

pub(crate) struct ParsableStruct<'a> {
    fields: Vec<ParserIndex>,
    names: Option<Vec<(&'a syn::Ident, usize)>>,
}

impl<'a> ParsableStruct<'a> {
    pub(crate) fn from_fields(
        context: &mut ParsableContext<'a>,
        fields: &'a syn::Fields,
    ) -> Result<Self, syn::Error> {
        let mut result = ParsableStruct {
            fields: Vec::new(),
            names: None,
        };
        let fields = match fields {
            syn::Fields::Named(fields) => {
                result.names = Some(Vec::new());
                &fields.named
            }
            syn::Fields::Unnamed(fields) => &fields.unnamed,
            syn::Fields::Unit => return Ok(result),
        };

        for field in fields.iter() {
            let parser = Parser::FromParsable(&field.ty);
            let parser_index = context.push_parser(parser);
            result.fields.push(parser_index);

            if let Some(ref mut names) = result.names {
                // unwrap: names is Some if the fields are names
                names.push((field.ident.as_ref().unwrap(), result.fields.len() - 1));
            }
        }

        Ok(result)
    }
}
