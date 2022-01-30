pub fn complete_variants<'a, 'b>(
    token: &'b str,
    variants: &'b [&'a str],
) -> impl Iterator<Item = &'a str> + 'b {
    let index = variants.binary_search(&token).unwrap_or_else(|idx| idx);
    variants[index..]
        .iter()
        .map(move |variant| variant.strip_prefix(token))
        .take_while(Option::is_some)
        .map(|suggestion| suggestion.unwrap()) // Iterator::take_while is unstable, unwrap is safe, Nones are filtered out
        .filter(|suggestion| !suggestion.is_empty())
}
