use crate::error::{ParseError, UnrecognizedToken};
use crate::tokens::Token;
use crate::{tokens::TokenStream, Parser};
use crate::{CompletionResult, Parsable, ParseResult};
use std::borrow::Cow;
use std::path::{Path, PathBuf};

fn reduce_dir_contents<T>(path: &Path, initial: T, reducer: impl Fn(T, &str) -> T) -> T {
    let dir_contents = match path.read_dir() {
        Ok(dir_contents) => dir_contents,
        Err(_) => return initial,
    };

    let mut value = initial;
    for item in dir_contents {
        let file_name = match item.map(|item| item.file_name()) {
            Ok(item) => item,
            Err(_) => continue,
        };
        if let Some(file_name) = file_name.to_str() {
            value = reducer(value, file_name);
        }
    }
    value
}

/// Parser implementation for [`PathBuf`]s with support for directory item completion
///
/// This parser consumes exactly one token and doesn't recognize any attributes. The contents of
/// the token are transformed into a [`PathBuf`]. When performing completion this parser enumerates
/// the directory at the given path and suggests completions that would point to existing directory
/// items.
#[derive(Default)]
pub struct PathParser;

impl<Ctx> Parser<Ctx> for PathParser {
    type Value = PathBuf;

    fn parse<'a>(&self, input: TokenStream<'a>, _ctx: Ctx) -> ParseResult<'a, Self::Value> {
        match input.take() {
            Some(Ok((token, remaining))) => match token {
                Token::Text(text) => Ok((text.parse_string().into_owned().into(), remaining)),
                Token::Attribute(_) => Err(UnrecognizedToken::new(token, remaining).into()),
            },
            Some(Err(err)) => Err(err.into()),
            None => Err(ParseError::token_required().expected("path").into()),
        }
    }

    fn complete<'a>(&self, input: TokenStream<'a>, _ctx: Ctx) -> CompletionResult<'a> {
        match input.take() {
            Some(Ok((Token::Text(_), remaining))) if !remaining.is_all_consumed() => {
                CompletionResult::new(remaining, true)
            }
            Some(Ok((Token::Text(text), _))) => {
                let text = text.parse_string();
                let path = Path::new::<str>(&text);
                if text.ends_with(&[std::path::MAIN_SEPARATOR, '/'] as &[char]) {
                    reduce_dir_contents(path, CompletionResult::new_final(true), |result, item| {
                        let suggestion = Cow::Owned(item.to_string());
                        result.add_suggestions(std::iter::once(suggestion))
                    })
                } else {
                    let filename = path.file_name().and_then(|os_str| os_str.to_str());
                    let parent = path.parent();
                    if let (Some(file_name), Some(parent)) = (filename, parent) {
                        reduce_dir_contents(
                            parent,
                            CompletionResult::new_final(true),
                            |mut result, item| {
                                if let Some(suffix) = item.strip_prefix(file_name) {
                                    if !suffix.is_empty() {
                                        let suggestion = Cow::Owned(suffix.to_string());
                                        result =
                                            result.add_suggestions(std::iter::once(suggestion));
                                    }
                                }
                                result
                            },
                        )
                    } else {
                        CompletionResult::new_final(true)
                    }
                }
            }
            Some(Ok((Token::Attribute(_), _))) => CompletionResult::new(input, false),
            None | Some(Err(_)) => CompletionResult::new_final(false),
        }
    }
}

impl<Ctx> Parsable<Ctx> for PathBuf {
    type Parser = PathParser;
}

#[cfg(test)]
mod tests {
    use crate::error::ParseError;
    use crate::testing::{test_complete, test_parse, token};
    use crate::tokens::TokenStream;
    use crate::{Parsable, Parser};
    use std::collections::BTreeSet;
    use std::fs::{create_dir, File};
    use std::path::{Path, PathBuf};

    test_parse!(
        parse_empty, PathBuf,
        "" => Error(ParseError::token_required().expected("path"))
    );
    test_parse!(
        parse_attribute, PathBuf,
        "--attr remaining" => Unrecognized(token!(--"attr"), Some(token!("remaining")))
    );
    test_parse!(
        parse_path, PathBuf,
        "dir/file.txt remaining" => Ok(PathBuf::from("dir/file.txt".to_string()), Some(token!("remaining")))
    );

    test_complete!(complete_attr, PathBuf, "--attr" => {
        consumed: false,
        remaining: Some(Some(token!(--"attr"))),
        suggestions: [],
    });
    test_complete!(complete_consumed, PathBuf, "path remaining" => {
        consumed: true,
        remaining: Some(Some(token!("remaining"))),
        suggestions: [],
    });

    fn create_file(parent: impl AsRef<Path>, name: &str) -> std::io::Result<()> {
        let mut path = parent.as_ref().to_path_buf();
        path.push(name);
        File::create(path)?;
        Ok(())
    }

    #[test]
    fn complete_suggestions() {
        let dir = tempdir::TempDir::new("cmd_parser_tests").unwrap();
        let mut path = dir.path().to_path_buf();
        path.push("inner");
        create_dir(&path).unwrap();
        create_file(&path, "first").unwrap();
        create_file(&path, "second").unwrap();
        create_file(&path, "third").unwrap();
        create_file(&path, "fourth").unwrap();
        create_file(&path, "fifth").unwrap();
        path.pop();

        macro_rules! assert_completion {
            ($name:literal, [$($expected:literal),*]) => {
                let parser = <PathBuf as Parsable<()>>::Parser::default();
                let input = format!("{}/{}", path.as_os_str().to_str().unwrap(), $name);
                let tokens = TokenStream::new(&input);
                let results = Parser::<()>::complete(&parser, tokens, ());
                assert!(results.value_consumed, "not consumed for input {}", $name);
                assert!(results.remaining.is_none(), "not final for input {}", $name);
                assert_eq!(results.suggestions, BTreeSet::from([$($expected.into()),*]), "suggestions incorrect for input {}", $name);
            };
        }

        assert_completion!("unknown/", []);
        assert_completion!("unknown", []);
        assert_completion!("in", ["ner"]);
        assert_completion!("inner", []);
        assert_completion!("inner/", ["first", "second", "third", "fourth", "fifth"]);
        assert_completion!("inner/fi", ["rst", "fth"]);
        assert_completion!("inner/first", []);
    }
}
