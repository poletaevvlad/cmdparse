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

pub(crate) struct CodegenContext<'a> {
    pub(crate) type_name: &'a syn::Ident,
    pub(crate) context_type: Option<ContextType>,
    pub(crate) generics: &'a syn::Generics,
    pub(crate) parsers: LinkedHashMap<Parser<'a>, ParserIndex>,
}

impl<'a> CodegenContext<'a> {
    pub(crate) fn from_derive_input(derive_input: &'a syn::DeriveInput) -> Self {
        CodegenContext {
            type_name: &derive_input.ident,
            context_type: None,
            generics: &derive_input.generics,
            parsers: LinkedHashMap::new(),
        }
    }

    pub(crate) fn push_parser(&mut self, parser: Parser<'a>) -> ParserIndex {
        let items_count = self.parsers.len();
        *self
            .parsers
            .entry(parser)
            .or_insert_with(|| ParserIndex(items_count))
    }

    pub(crate) fn ctx_requires_clone(&self) -> bool {
        self.parsers.len() > 1
    }
}

#[cfg(test)]
pub(crate) struct MockCodegenContext {
    type_name: syn::Ident,
    generics: syn::Generics,
}

#[cfg(test)]
impl Default for MockCodegenContext {
    fn default() -> Self {
        Self {
            type_name: format_ident!("MockType"),
            generics: syn::Generics::default(),
        }
    }
}

#[cfg(test)]
impl MockCodegenContext {
    pub(crate) fn context(&self) -> CodegenContext<'_> {
        CodegenContext {
            type_name: &self.type_name,
            context_type: None,
            generics: &self.generics,
            parsers: LinkedHashMap::new(),
        }
    }
}
