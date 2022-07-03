use linked_hash_map::LinkedHashMap;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub(crate) enum Parser<'a> {
    Explicit(Box<syn::Type>),
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
    pub(crate) visibility_mod: &'a syn::Visibility,
    pub(crate) type_name: &'a syn::Ident,
    pub(crate) context_type: Option<ContextType>,
    pub(crate) generics: &'a syn::Generics,
    pub(crate) parsers: LinkedHashMap<Parser<'a>, ParserIndex>,
}

impl<'a> CodegenContext<'a> {
    pub(crate) fn from_derive_input(derive_input: &'a syn::DeriveInput) -> Self {
        CodegenContext {
            visibility_mod: &derive_input.vis,
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

    pub(crate) fn parse_context_ident(&self) -> TokenStream {
        match &self.context_type {
            Some(ContextType::Concrete(ty)) => quote! {#ty},
            _ => quote! {CmdParserCtx},
        }
    }
}

#[cfg(test)]
pub(crate) struct MockCodegenContext {
    pub(crate) type_name: syn::Ident,
    pub(crate) generics: syn::Generics,
    pub(crate) vis_modifier: syn::Visibility,
}

#[cfg(test)]
impl Default for MockCodegenContext {
    fn default() -> Self {
        Self {
            vis_modifier: syn::Visibility::Inherited,
            type_name: format_ident!("MockType"),
            generics: syn::Generics::default(),
        }
    }
}

#[cfg(test)]
impl MockCodegenContext {
    pub(crate) fn context(&self) -> CodegenContext<'_> {
        CodegenContext {
            visibility_mod: &self.vis_modifier,
            type_name: &self.type_name,
            context_type: None,
            generics: &self.generics,
            parsers: LinkedHashMap::new(),
        }
    }
}
