use crate::schema::ContextType;
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
                let meta = match nested {
                    syn::NestedMeta::Meta(meta) => meta,
                    syn::NestedMeta::Lit(lit) => {
                        return Err(Error::new(lit.span(), "Unexpected literal"))
                    }
                };
                match meta {
                    syn::Meta::Path(path) => attributes.visit_path(path)?,
                    syn::Meta::NameValue(name_value) => attributes.visit_name_value(name_value)?,
                    syn::Meta::List(list) => attributes.visit_list(list)?,
                }
            }
        }

        Ok(attributes)
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
    context_type: Option<ContextType>,
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
            let type_ = syn::parse_str::<syn::Type>(&string)?;
            self.set_context_type(ContextType::Concrete(type_), name_value.span())?;
            Ok(())
        } else if compare_path(&name_value.path, "ctx_bounds") {
            let string = get_name_value_string(name_value)?;
            let type_param = syn::parse_str::<syn::TypeParam>(&format!("T: {}", string))?;
            self.set_context_type(ContextType::Generic(type_param.bounds), name_value.span())?;
            Ok(())
        } else {
            Err(unknown_attr_error(&name_value.path))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{BuildableAttributes, TypeAttributes};
    use proc_macro2::TokenStream;
    use quote::quote;

    mod type_atttributes {
        use crate::schema::ContextType;

        use super::*;
        use proc_macro2::Span;

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
            let attributes = TypeAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(
                &attributes.to_string(),
                "ctx attribute cannot be used more than once"
            );
        }

        #[test]
        fn error_duplicate_cmd_bounds() {
            let attrs = [make_attribute(
                "cmd",
                quote! {(ctx_bounds = "Clone", ctx_bounds="Send")},
            )];
            let attributes = TypeAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(
                &attributes.to_string(),
                "ctx_bounds attribute cannot be used more than once"
            );
        }

        #[test]
        fn error_both_cmd_and_bounds() {
            let attrs = [make_attribute(
                "cmd",
                quote! {(ctx = "u16", ctx_bounds="Clone")},
            )];
            let attributes = TypeAttributes::from_attributes(attrs.iter()).unwrap_err();
            assert_eq!(
                &attributes.to_string(),
                "ctx and ctx_bounds cannot be used at the same time"
            );
        }
    }
}
