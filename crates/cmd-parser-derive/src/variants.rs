use crate::attributes::{BuildableAttributes, VariantAttributes};
use crate::fields::{FieldsSet, ParsableContext};

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Variant {
    label: String,
    fieldset_index: usize,
}

pub(crate) struct VariantFieldsSet<'a> {
    ident: &'a syn::Ident,
    fieldset: FieldsSet<'a>,
}

pub(crate) struct VariantsSet<'a> {
    variants: Vec<Variant>,
    fieldsets: Vec<VariantFieldsSet<'a>>,
}

impl<'a> VariantsSet<'a> {
    pub(crate) fn from_variants(
        context: &mut ParsableContext<'a>,
        variants: impl Iterator<Item = &'a syn::Variant>,
    ) -> Result<Self, syn::Error> {
        let mut result = VariantsSet {
            variants: Vec::new(),
            fieldsets: Vec::new(),
        };

        for variant in variants {
            let attributes = VariantAttributes::from_attributes(variant.attrs.iter())?;
            if attributes.ignored && attributes.aliases.is_empty() {
                continue;
            }

            let fieldset = FieldsSet::from_fields(context, &variant.fields)?;
            let fieldset_index = result.fieldsets.len();
            result.fieldsets.push(VariantFieldsSet {
                ident: &variant.ident,
                fieldset,
            });

            if !attributes.ignored {
                let label = attributes
                    .renamed
                    .unwrap_or_else(|| variant_to_kebab_case(&variant.ident.to_string()));
                result.variants.push(Variant {
                    label,
                    fieldset_index,
                });
            }
            for alias in attributes.aliases {
                result.variants.push(Variant {
                    label: alias,
                    fieldset_index,
                });
            }
        }

        Ok(result)
    }
}

fn variant_to_kebab_case(ident: &str) -> String {
    let mut result = String::new();
    for (i, ch) in ident.chars().enumerate() {
        let lowercase = ch.to_ascii_lowercase();
        if i > 0 && ch != lowercase {
            result.push('-');
        }
        result.push(lowercase);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::{variant_to_kebab_case, Variant, VariantsSet};
    use crate::fields::ParsableContext;
    use quote::quote;

    #[test]
    fn rename_variant() {
        assert_eq!(&variant_to_kebab_case("Word"), "word");
        assert_eq!(&variant_to_kebab_case("TwoWords"), "two-words");
    }

    #[test]
    fn simple_variants() {
        let enum_ = quote! { enum Mock {
            First,
            Second(u8),
            MultipleWords,
            #[cmd(rename = "new-name")] Renamed,
            #[cmd(alias = "alias-1", alias = "alias-2")] WithAliases,
            #[cmd(ignore, alias = "ignored-1", alias = "ignored-2")] AliasesOnly,
            #[cmd(ignore)] Ignored,
        }};
        let variants = syn::parse2::<syn::ItemEnum>(enum_).unwrap().variants;
        let mut context = ParsableContext::default();

        let variantsset = VariantsSet::from_variants(&mut context, variants.iter()).unwrap();

        let expected_variants = [
            ("first", 0),
            ("second", 1),
            ("multiple-words", 2),
            ("new-name", 3),
            ("with-aliases", 4),
            ("alias-1", 4),
            ("alias-2", 4),
            ("ignored-1", 5),
            ("ignored-2", 5),
        ];
        assert_eq!(
            variantsset.variants,
            expected_variants
                .iter()
                .map(|(label, index)| Variant {
                    label: label.to_string(),
                    fieldset_index: *index
                })
                .collect::<Vec<_>>(),
        );

        let variant_fields: Vec<_> = variantsset
            .fieldsets
            .iter()
            .map(|fs| fs.ident.to_string())
            .collect();
        assert_eq!(
            variant_fields,
            vec![
                "First".to_string(),
                "Second".to_string(),
                "MultipleWords".to_string(),
                "Renamed".to_string(),
                "WithAliases".to_string(),
                "AliasesOnly".to_string(),
            ]
        );
    }
}
