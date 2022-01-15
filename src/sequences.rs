use crate::utils::{complete_inner, has_tokens, parse_inner};
use crate::{CompletionResult, Parsable, ParseResult, Parser};

pub struct VecParser<Ctx, T: Parsable<Ctx>> {
    inner_parser: T::Parser,
}

impl<Ctx, T: Parsable<Ctx>> Parser<Ctx> for VecParser<Ctx, T> {
    type Value = Vec<<T::Parser as Parser<Ctx>>::Value>;

    fn create(ctx: Ctx) -> Self {
        VecParser {
            inner_parser: T::new_parser(ctx),
        }
    }

    fn parse<'a>(&self, mut input: &'a str) -> ParseResult<'a, Self::Value> {
        let mut result = Vec::new();
        while has_tokens(input) {
            match parse_inner(input, &self.inner_parser) {
                ParseResult::Parsed(value, remaining) => {
                    input = remaining;
                    result.push(value);
                }
                ParseResult::Unrecognized(err) | ParseResult::Failed(err) => {
                    return ParseResult::Failed(err)
                }
            }
        }
        ParseResult::Parsed(result, input)
    }

    fn complete<'a>(&self, mut input: &'a str) -> CompletionResult<'a> {
        while has_tokens(input) {
            match complete_inner(input, &self.inner_parser) {
                CompletionResult::Consumed(remaining) => input = remaining,
                result => return result,
            }
        }
        CompletionResult::Consumed(input)
    }
}

impl<Ctx, T: Parsable<Ctx>> Parsable<Ctx> for Vec<T> {
    type Parser = VecParser<Ctx, T>;
}

#[cfg(test)]
mod tests {
    use super::VecParser;
    use crate::{ParseResult, Parser};

    mod vec_parser {
        use crate::ParseError;

        use super::*;

        #[test]
        fn parse_empty() {
            let parser: VecParser<(), i32> = VecParser::create(());
            assert_eq!(parser.parse(""), ParseResult::Parsed(vec![], ""));
        }

        #[test]
        fn parse_flat() {
            let parser: VecParser<(), i32> = VecParser::create(());
            assert_eq!(
                parser.parse("1 2 3 4 5"),
                ParseResult::Parsed(vec![1, 2, 3, 4, 5], "")
            );
        }

        #[test]
        fn parse_error() {
            let parser: VecParser<(), i32> = VecParser::create(());
            assert_eq!(
                parser.parse("1 2 nan 3 4"),
                ParseResult::Failed(ParseError::token_parse("nan".into(), None, "integer"))
            );
        }

        #[test]
        fn parse_nested() {
            let parser: VecParser<(), Vec<i32>> = VecParser::create(());
            assert_eq!(
                parser.parse("() (1 2 3) 4 5 6 7"),
                ParseResult::Parsed(vec![vec![], vec![1, 2, 3], vec![4, 5, 6, 7]], "")
            );
        }
    }
}
