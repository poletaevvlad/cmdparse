use crate::context::ContextType;
use proc_macro2::Span;
use std::collections::HashMap;
use syn::spanned::Spanned;
use syn::Error;

fn unknown_attr_error(path: &syn::Path) -> Error {
    match path.get_ident() {
        Some(ident) => Error::new(path.span(), format!("Unknown attribute: {}", ident)),
        None => Error::new(path.span(), "Unknown attribute"),
    }
}

pub(crate) trait BuildableAttributes {
    fn visit_name_value(&mut self, name_value: &syn::MetaNameValue) -> Result<(), Error> {
        Err(unknown_attr_error(&name_value.path))
    }

    fn visit_path(&mut self, path: &syn::Path) -> Result<(), Error> {
        Err(unknown_attr_error(path))
    }

    fn visit_list(&mut self, list: &syn::MetaList) -> Result<(), Error> {
        Err(unknown_attr_error(&list.path))
    }

    fn from_attributes<'a>(attrs: impl Iterator<Item = &'a syn::Attribute>) -> Result<Self, Error>
    where
        Self: Default,
    {
        let mut attributes = Self::default();

        for attr in attrs {
            let meta = attr.parse_meta()?;
            let inner = match meta {
                syn::Meta::Path(path) if compare_path(&path, "cmd") => {
                    return Err(Error::new(path.span(), "Missing argument parameters"));
                }
                syn::Meta::NameValue(name_value) if compare_path(&name_value.path, "cmd") => {
                    return Err(Error::new(
                        name_value.span(),
                        "Key-value argument style is not allowed",
                    ));
                }
                syn::Meta::List(list) if compare_path(&list.path, "cmd") => list,
                _ => continue,
            };

            for nested in inner.nested.iter() {
                attributes.visit_nested_meta(nested)?;
            }
        }

        Ok(attributes)
    }

    fn visit_nested_meta(&mut self, nested: &syn::NestedMeta) -> Result<(), Error> {
        let meta = match nested {
            syn::NestedMeta::Meta(meta) => meta,
            syn::NestedMeta::Lit(lit) => return Err(Error::new(lit.span(), "Unexpected literal")),
        };
        match meta {
            syn::Meta::Path(path) => self.visit_path(path),
            syn::Meta::NameValue(name_value) => self.visit_name_value(name_value),
            syn::Meta::List(list) => self.visit_list(list),
        }
    }
}

fn compare_path(path: &syn::Path, name: &str) -> bool {
    matches!(path.get_ident(), Some(ident) if ident == name)
}

fn get_name_value_string(name_value: &syn::MetaNameValue) -> Result<String, Error> {
    if let syn::Lit::Str(string) = &name_value.lit {
        Ok(string.value())
    } else {
        Err(Error::new(name_value.lit.span(), "Expected a string"))
    }
}

#[derive(Debug, Default)]
pub(crate) struct TypeAttributes {
    pub(crate) context_type: Option<ContextType>,
}

impl TypeAttributes {
    fn set_context_type(
        &mut self,
        context_type: ContextType,
        span: proc_macro2::Span,
    ) -> Result<(), Error> {
        let error_message = match self.context_type {
            Some(ContextType::Generic(_)) if matches!(context_type, ContextType::Generic(_)) => {
                "ctx_bounds attribute cannot be used more than once"
            }
            Some(ContextType::Concrete(_)) if matches!(context_type, ContextType::Concrete(_)) => {
                "ctx attribute cannot be used more than once"
            }
            Some(_) => "ctx and ctx_bounds cannot be used at the same time",
            None => {
                self.context_type = Some(context_type);
                return Ok(());
            }
        };
        Err(Error::new(span, error_message))
    }
}

impl BuildableAttributes for TypeAttributes {
    fn visit_name_value(&mut self, name_value: &syn::MetaNameValue) -> Result<(), Error> {
        if compare_path(&name_value.path, "ctx") {
            let string = get_name_value_string(name_value)?;
            let type_ = syn::parse_str::<syn::Type>(&string)
                .map_err(|error| Error::new(name_value.lit.span(), error))?;
            self.set_context_type(ContextType::Concrete(Box::new(type_)), name_value.span())?;
            Ok(())
        } else if compare_path(&name_value.path, "ctx_bounds") {
            let string = get_name_value_string(name_value)?;
            let type_param = syn::parse_str::<syn::TypeParam>(&format!("T: {}", string))
                .map_err(|error| Error::new(name_value.lit.span(), error))?;
            self.set_context_type(
                ContextType::Generic(Box::new(type_param.bounds)),
                name_value.span(),
            )?;
            Ok(())
        } else {
            Err(unknown_attr_error(&name_value.path))
        }
    }
}

#[derive(Debug, Default)]
pub(crate) struct FieldAttributes {
    pub(crate) parser: Option<syn::Type>,
    pub(crate) default: Option<Option<syn::Expr>>,
    pub(crate) names: HashMap<String, Option<syn::Expr>>,
    pub(crate) alias_values: HashMap<String, syn::Expr>,
}

impl BuildableAttributes for FieldAttributes {
    fn visit_name_value(&mut self, name_value: &syn::MetaNameValue) -> Result<(), Error> {
        if compare_path(&name_value.path, "parser") {
            let string = get_name_value_string(name_value)?;
            let parser_type = syn::parse_str(&string)
                .map_err(|error| Error::new(name_value.lit.span(), error))?;
            if self.parser.is_some() {
                return Err(Error::new(
                    name_value.span(),
                    "multiple parsers aren't allowed",
                ));
            }
            self.parser = Some(parser_type);
            Ok(())
        } else if compare_path(&name_value.path, "default") {
            let string = get_name_value_string(name_value)?;
            let default = syn::parse_str(&string)
                .map_err(|error| Error::new(name_value.lit.span(), error))?;
            if self.default.is_some() {
                return Err(Error::new(
                    name_value.span(),
                    "multiple default values aren't allowed",
                ));
            }
            self.default = Some(Some(default));
            Ok(())
        } else {
            Err(unknown_attr_error(&name_value.path))
        }
    }

    fn visit_path(&mut self, path: &syn::Path) -> Result<(), Error> {
        if compare_path(path, "default") {
            if self.default.is_some() {
                return Err(Error::new(
                    path.span(),
                    "multiple default values aren't allowed",
                ));
            }
            self.default = Some(None);
            Ok(())
        } else {
            Err(unknown_attr_error(path))
        }
    }

    fn visit_list(&mut self, list: &syn::MetaList) -> Result<(), Error> {
        if compare_path(&list.path, "attr") {
            let mut names_attributes = FieldNamesAttributes(&mut self.names);
            for nested in list.nested.iter() {
                names_attributes.visit_nested_meta(nested)?;
            }
            Ok(())
        } else if compare_path(&list.path, "alias_value") {
            let mut inner_attributes = AliasValueOverrideAttributes::default();
            for nested in list.nested.iter() {
                inner_attributes.visit_nested_meta(nested)?;
            }

            match (inner_attributes.alias, inner_attributes.value) {
                (None, _) => Err(Error::new(list.span(), "alias attribute is required")),
                (_, None) => Err(Error::new(list.span(), "value attribute is required")),
                (Some(alias), Some(value)) => {
                    if self.alias_values.contains_key(&alias) {
                        return Err(Error::new(
                            list.span(),
                            "multiple values for the same attribute",
                        ));
                    }
                    self.alias_values.insert(alias, value);
                    Ok(())
                }
            }
        } else {
            Err(unknown_attr_error(&list.path))
        }
    }

    fn from_attributes<'a>(attrs: impl Iterator<Item = &'a syn::Attribute>) -> Result<Self, Error>
    where
        Self: Default,
    {
        let mut attributes = Self::default();

        for attr in attrs {
            let meta = attr.parse_meta()?;
            let inner = match meta {
                syn::Meta::Path(path) if compare_path(&path, "cmd") => {
                    return Err(Error::new(path.span(), "Missing argument parameters"));
                }
                syn::Meta::NameValue(name_value) if compare_path(&name_value.path, "cmd") => {
                    return Err(Error::new(
                        name_value.span(),
                        "Key-value argument style is not allowed",
                    ));
                }
                syn::Meta::List(list) if compare_path(&list.path, "cmd") => list,
                _ => continue,
            };

            for nested in inner.nested.iter() {
                attributes.visit_nested_meta(nested)?;
            }
        }

        Ok(attributes)
    }

    fn visit_nested_meta(&mut self, nested: &syn::NestedMeta) -> Result<(), Error> {
        let meta = match nested {
            syn::NestedMeta::Meta(meta) => meta,
            syn::NestedMeta::Lit(lit) => return Err(Error::new(lit.span(), "Unexpected literal")),
        };
        match meta {
            syn::Meta::Path(path) => self.visit_path(path),
            syn::Meta::NameValue(name_value) => self.visit_name_value(name_value),
            syn::Meta::List(list) => self.visit_list(list),
        }
    }
}

struct FieldNamesAttributes<'a>(&'a mut HashMap<String, Option<syn::Expr>>);

impl<'a> FieldNamesAttributes<'a> {
    #[allow(clippy::map_entry)]
    fn push(
        &mut self,
        path_span: Span,
        name: String,
        value: Option<syn::Expr>,
    ) -> Result<(), Error> {
        if self.0.contains_key(&name) {
            Err(Error::new(
                path_span,
                format!("attribute \"{}\" declared more than once", name),
            ))
        } else {
            self.0.insert(name, value);
            Ok(())
        }
    }
}

impl<'a> BuildableAttributes for FieldNamesAttributes<'a> {
    fn visit_name_value(&mut self, name_value: &syn::MetaNameValue) -> Result<(), Error> {
        let name = name_to_kebab_case(&get_path_string(&name_value.path)?);
        let value = syn::parse_str::<syn::Expr>(&get_name_value_string(name_value)?)
            .map_err(|error| Error::new(name_value.lit.span(), error))?;

        self.push(name_value.path.span(), name, Some(value))
    }

    fn visit_path(&mut self, path: &syn::Path) -> Result<(), Error> {
        let name = name_to_kebab_case(&get_path_string(path)?);
        self.push(path.span(), name, None)
    }
}

#[derive(Default)]
struct AliasValueOverrideAttributes {
    alias: Option<String>,
    value: Option<syn::Expr>,
}

impl BuildableAttributes for AliasValueOverrideAttributes {
    fn visit_name_value(&mut self, name_value: &syn::MetaNameValue) -> Result<(), Error> {
        if compare_path(&name_value.path, "alias") {
            if self.alias.is_some() {
                return Err(Error::new(
                    name_value.span(),
                    "multiple aliases are not allowed",
                ));
            }
            let alias = get_name_value_string(name_value)?;
            self.alias = Some(alias);
            Ok(())
        } else if compare_path(&name_value.path, "value") {
            if self.value.is_some() {
                return Err(Error::new(
                    name_value.span(),
                    "multiple values are not allowed",
                ));
            }
            let value_str = get_name_value_string(name_value)?;
            let value = syn::parse_str::<syn::Expr>(&value_str)
                .map_err(|error| Error::new(name_value.lit.span(), error))?;
            self.value = Some(value);
            Ok(())
        } else {
            Err(unknown_attr_error(&name_value.path))
        }
    }
}

fn get_path_string(path: &syn::Path) -> Result<String, Error> {
    path.get_ident()
        .map(|ident| ident.to_string())
        .ok_or_else(|| Error::new(path.span(), "multi-segment paths aren't allowed"))
}

#[derive(Debug, Default)]
pub(crate) struct VariantAttributes {
    pub(crate) aliases: Vec<String>,
    pub(crate) renamed: Option<String>,
    pub(crate) ignored: bool,
    pub(crate) transparent_no_error: Option<bool>,
}

impl BuildableAttributes for VariantAttributes {
    fn visit_path(&mut self, path: &syn::Path) -> Result<(), Error> {
        if compare_path(path, "ignore") {
            if self.renamed.is_some() {
                return Err(Error::new(
                    path.span(),
                    "ignore and rename have no effect together",
                ));
            }
            self.ignored = true;
            Ok(())
        } else if compare_path(path, "transparent") {
            self.transparent_no_error = Some(false);
            Ok(())
        } else if compare_path(path, "transparent_no_error") {
            self.transparent_no_error = Some(true);
            Ok(())
        } else {
            Err(unknown_attr_error(path))
        }
    }

    fn visit_name_value(&mut self, name_value: &syn::MetaNameValue) -> Result<(), Error> {
        if compare_path(&name_value.path, "rename") {
            if self.ignored {
                return Err(Error::new(
                    name_value.path.span(),
                    "ignore and rename have no effect together",
                ));
            }
            if self.renamed.is_some() {
                return Err(Error::new(
                    name_value.span(),
                    "variant cannot be renamed more than once",
                ));
            }
            let value = get_name_value_string(name_value)?;
            self.renamed = Some(value);
            Ok(())
        } else if compare_path(&name_value.path, "alias") {
            let value = get_name_value_string(name_value)?;
            if self.aliases.contains(&value) {
                return Err(Error::new(
                    name_value.span(),
                    "duplicate aliases are not allowed",
                ));
            }
            self.aliases.push(value);
            Ok(())
        } else {
            Err(unknown_attr_error(&name_value.path))
        }
    }
}

fn name_to_kebab_case(name: &str) -> String {
    name.replace('_', "-")
}

#[cfg(test)]
mod tests {
    use super::BuildableAttributes;
    use crate::context::ContextType;
    use proc_macro2::{Span, TokenStream};
    use quote::quote;

    fn make_attribute(path_str: &str, tokens: TokenStream) -> syn::Attribute {
        syn::Attribute {
            pound_token: syn::token::Pound {
                spans: [Span::call_site()],
            },
            style: syn::AttrStyle::Outer,
            bracket_token: syn::token::Bracket {
                span: Span::call_site(),
            },
            path: syn::parse_str(path_str).unwrap(),
            tokens,
        }
    }

    mod type_atttributes {
        use super::super::TypeAttributes;
        use super::*;

        #[test]
        fn parse_emty() {
            let attributes = TypeAttributes::from_attributes(std::iter::empty()).unwrap();
            assert!(attributes.context_type.is_none());
        }

        #[test]
        fn parse_generic() {
            let attrs = [
                make_attribute("unknown", quote! {= "ignored"}),
                make_attribute("cmd", quote! {(ctx_bounds = "Sync + Clone")}),
            ];
            let attributes = TypeAttributes::from_attributes(attrs.iter()).unwrap();
            assert!(
                matches!(attributes.context_type, Some(ContextType::Generic(bounds)) if quote!{#bounds}.to_string() == "Sync + Clone")
            );
        }

        #[test]
        fn parse_concrete() {
            let attrs = [make_attribute("cmd", quote! {(ctx = "u16")})];
            let attributes = TypeAttributes::from_attributes(attrs.iter()).unwrap();
            assert!(
                matches!(attributes.context_type, Some(ContextType::Concrete(ty)) if quote!{#ty}.to_string() == "u16")
            );
        }

        #[test]
        fn error_duplicate_cmd() {
            let attrs = [make_attribute("cmd", quote! {(ctx = "u16", ctx="u32")})];
            let error = TypeAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(
                &error.to_string(),
                "ctx attribute cannot be used more than once"
            );
        }

        #[test]
        fn error_duplicate_cmd_bounds() {
            let attrs = [make_attribute(
                "cmd",
                quote! {(ctx_bounds = "Clone", ctx_bounds="Send")},
            )];
            let error = TypeAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(
                &error.to_string(),
                "ctx_bounds attribute cannot be used more than once"
            );
        }

        #[test]
        fn error_both_cmd_and_bounds() {
            let attrs = [make_attribute(
                "cmd",
                quote! {(ctx = "u16", ctx_bounds="Clone")},
            )];
            let error = TypeAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(
                &error.to_string(),
                "ctx and ctx_bounds cannot be used at the same time"
            );
        }
    }

    mod field_attributes {
        use super::*;
        use crate::attributes::FieldAttributes;

        #[test]
        fn with_parser() {
            let attrs = [make_attribute("cmd", quote! {(parser = "crate::Parser")})];
            let attributes = FieldAttributes::from_attributes(attrs.iter()).unwrap();
            assert!(
                matches!(attributes.parser, Some(ty) if quote!{#ty}.to_string() == "crate :: Parser")
            );
        }

        #[test]
        fn error_duplicate_parsers() {
            let attrs = [make_attribute(
                "cmd",
                quote! {(parser = "crate::Parser", parser="crate::Parser2")},
            )];
            let error = FieldAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(&error.to_string(), "multiple parsers aren't allowed");
        }

        #[test]
        fn with_default() {
            let attrs = [make_attribute("cmd", quote! {(default = "42")})];
            let attributes = FieldAttributes::from_attributes(attrs.iter()).unwrap();
            assert!(
                matches!(attributes.default, Some(Some(expr)) if quote!{#expr}.to_string() == "42")
            );
        }

        #[test]
        fn with_default_optional() {
            let attrs = [make_attribute("cmd", quote! {(default)})];
            let attributes = FieldAttributes::from_attributes(attrs.iter()).unwrap();
            assert!(matches!(attributes.default, Some(None)));
        }

        #[test]
        fn error_duplicate_default() {
            let attrs = [make_attribute(
                "cmd",
                quote! {(default = "true", default="false")},
            )];
            let error = FieldAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(&error.to_string(), "multiple default values aren't allowed");
        }

        #[test]
        fn with_attributes() {
            let attrs = [
                make_attribute("cmd", quote! {(attr(one = "1", two = "2"))}),
                make_attribute("cmd", quote! {(attr(three = "3"))}),
            ];
            let attributes = FieldAttributes::from_attributes(attrs.iter()).unwrap();
            assert_eq!(attributes.names.len(), 3);
            assert!(
                matches!(attributes.names.get("one"), Some(expr) if quote!{#expr}.to_string() == "1")
            );
            assert!(
                matches!(attributes.names.get("two"), Some(expr) if quote!{#expr}.to_string() == "2")
            );
            assert!(
                matches!(attributes.names.get("three"), Some(expr) if quote!{#expr}.to_string() == "3")
            );
        }

        #[test]
        fn error_invalid_attribute_path() {
            let attrs = [make_attribute("cmd", quote! {(attr(long::path = "1"))})];
            let error = FieldAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(&error.to_string(), "multi-segment paths aren't allowed");
        }

        #[test]
        fn error_duplicate_attribures() {
            let attrs = [make_attribute("cmd", quote! {(attr(one = "1", one = "2"))})];
            let error = FieldAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(
                &error.to_string(),
                "attribute \"one\" declared more than once"
            );
        }

        mod alias_value_override {
            use super::*;

            #[test]
            fn parsing() {
                let attrs = [make_attribute(
                    "cmd",
                    quote!((
                        alias_value(alias = "al1", value = "5"),
                        alias_value(alias = "al2", value = "10")
                    )),
                )];
                let attributes = FieldAttributes::from_attributes(attrs.iter()).unwrap();
                assert_eq!(attributes.alias_values.len(), 2);
                let al1 = attributes.alias_values.get("al1").unwrap();
                assert_eq!(quote!(#al1).to_string(), "5");
                let al2 = attributes.alias_values.get("al2").unwrap();
                assert_eq!(quote!(#al2).to_string(), "10");
            }

            #[test]
            fn missing_alias() {
                let attrs = [make_attribute("cmd", quote! {(alias_value(value = "1"))})];
                let error = FieldAttributes::from_attributes(attrs.iter()).unwrap_err();
                assert_eq!(&error.to_string(), "alias attribute is required");
            }

            #[test]
            fn duplicate_alias() {
                let attrs = [make_attribute(
                    "cmd",
                    quote! {(alias_value(alias = "a", alias = "b", value = "1"))},
                )];
                let error = FieldAttributes::from_attributes(attrs.iter()).unwrap_err();
                assert_eq!(&error.to_string(), "multiple aliases are not allowed");
            }

            #[test]
            fn missing_value() {
                let attrs = [make_attribute("cmd", quote! {(alias_value(alias = "al"))})];
                let error = FieldAttributes::from_attributes(attrs.iter()).unwrap_err();
                assert_eq!(&error.to_string(), "value attribute is required");
            }

            #[test]
            fn duplicate_values() {
                let attrs = [make_attribute(
                    "cmd",
                    quote! {(alias_value(alias = "a", value = "1", value = "2"))},
                )];
                let error = FieldAttributes::from_attributes(attrs.iter()).unwrap_err();
                assert_eq!(&error.to_string(), "multiple values are not allowed");
            }

            #[test]
            fn conflicting_value() {
                let attrs = [make_attribute(
                    "cmd",
                    quote! {(alias_value(alias = "a", value = "1"), alias_value(alias = "a", value = "2"))},
                )];
                let error = FieldAttributes::from_attributes(attrs.iter()).unwrap_err();
                assert_eq!(&error.to_string(), "multiple values for the same attribute");
            }
        }
    }

    mod variant_attributes {
        use super::super::VariantAttributes;
        use super::*;

        #[test]
        fn with_aliases() {
            let attrs = [make_attribute(
                "cmd",
                quote! {(ignore, alias="first", alias = "second")},
            )];
            let attributes = VariantAttributes::from_attributes(attrs.iter()).unwrap();
            assert!(attributes.ignored);
            assert!(attributes.renamed.is_none());
            assert_eq!(
                attributes.aliases,
                vec!["first".to_string(), "second".to_string()]
            );
        }

        #[test]
        fn with_rename() {
            let attrs = [make_attribute("cmd", quote! {(rename = "renamed")})];
            let attributes = VariantAttributes::from_attributes(attrs.iter()).unwrap();
            assert!(!attributes.ignored);
            assert_eq!(attributes.renamed, Some("renamed".to_string()));
            assert!(attributes.aliases.is_empty());
        }

        #[test]
        fn error_duplicate_rename() {
            let attrs = [make_attribute("cmd", quote! {(rename = "a", rename = "b")})];
            let error = VariantAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(
                &error.to_string(),
                "variant cannot be renamed more than once"
            );
        }

        #[test]
        fn error_duplicate_aliases() {
            let attrs = [make_attribute("cmd", quote! {(alias = "a", alias = "a")})];
            let error = VariantAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(&error.to_string(), "duplicate aliases are not allowed");
        }

        #[test]
        fn error_ignore_and_rename() {
            let attrs = [make_attribute("cmd", quote! {(rename = "a", ignore)})];
            let error = VariantAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(
                &error.to_string(),
                "ignore and rename have no effect together"
            );
        }

        #[test]
        fn error_ignore_and_rename_flipped() {
            let attrs = [make_attribute("cmd", quote! {(ignore, rename = "a")})];
            let error = VariantAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(
                &error.to_string(),
                "ignore and rename have no effect together"
            );
        }
    }
}
