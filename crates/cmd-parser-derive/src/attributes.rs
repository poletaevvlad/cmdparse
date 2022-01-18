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
