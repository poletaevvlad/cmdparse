use crate::utils::{complete_inner, has_tokens, parse_inner};
use crate::{CompletionResult, Parsable, ParseResult, Parser};

pub struct VecParser<Ctx, T: Parsable<Ctx>> {
    inner_parser: T::Parser,
}

impl<Ctx, T: Parsable<Ctx>> Parser<Ctx> for VecParser<Ctx, T> {
    type Value = Vec<T>;

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

macro_rules! gen_parsable_tuple {
    ($parser_name:ident, $($param:ident)*) => {
        #[allow(non_snake_case)]
        pub struct $parser_name<Ctx, $($param: Parsable<Ctx>),*> {
            $(
                $param: $param::Parser,
            )*
        }

        impl<Ctx: Clone, $($param: Parsable<Ctx>),*> Parser<Ctx> for $parser_name<Ctx, $($param),*> {
            type Value = ($($param,)*);

            fn create(ctx: Ctx) -> Self {
                $parser_name {
                    $($param: $param::new_parser(ctx.clone())),*
                }
            }

            #[allow(non_snake_case)]
            fn parse<'a>(&self, mut input: &'a str) -> ParseResult<'a, Self::Value> {
                $(
                    let $param = match parse_inner(input, &self.$param) {
                        ParseResult::Parsed(value, remaining) => {
                            input = remaining;
                            value
                        }
                        ParseResult::Unrecognized(err) | ParseResult::Failed(err) => {
                            return ParseResult::Failed(err)
                        }
                    };
                )*
                ParseResult::Parsed(($($param,)*), input)
            }

            fn complete<'a>(&self, mut input: &'a str) -> CompletionResult<'a> {
                $(
                    match complete_inner(input, &self.$param) {
                        CompletionResult::Consumed(remaining) => input = remaining,
                        result => return result,
                    }
                )*
                CompletionResult::Consumed(input)
            }
        }

        impl<Ctx: Clone, $($param: Parsable<Ctx>),*> Parsable<Ctx> for ($($param,)*) {
            type Parser = $parser_name<Ctx, $($param),*>;
        }
    }
}

gen_parsable_tuple!(TupleParser1, T1);
gen_parsable_tuple!(TupleParser2, T1 T2);
gen_parsable_tuple!(TupleParser3, T1 T2 T3);
gen_parsable_tuple!(TupleParser4, T1 T2 T3 T4);
gen_parsable_tuple!(TupleParser5, T1 T2 T3 T4 T5);
gen_parsable_tuple!(TupleParser6, T1 T2 T3 T4 T5 T6);
gen_parsable_tuple!(TupleParser7, T1 T2 T3 T4 T5 T6 T7);
gen_parsable_tuple!(TupleParser8, T1 T2 T3 T4 T5 T6 T7 T8);
gen_parsable_tuple!(TupleParser9, T1 T2 T3 T4 T5 T6 T7 T8 T9);
gen_parsable_tuple!(TupleParser10, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10);
gen_parsable_tuple!(TupleParser11, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11);
gen_parsable_tuple!(TupleParser12, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12);
gen_parsable_tuple!(TupleParser13, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13);
gen_parsable_tuple!(TupleParser14, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14);
gen_parsable_tuple!(TupleParser15, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15);
gen_parsable_tuple!(TupleParser16, T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16);

pub struct BoxParser<Ctx, T: Parsable<Ctx>> {
    inner_parser: T::Parser,
}

impl<Ctx, T: Parsable<Ctx>> Parser<Ctx> for BoxParser<Ctx, T> {
    type Value = Box<T>;

    fn create(ctx: Ctx) -> Self {
        BoxParser {
            inner_parser: T::new_parser(ctx),
        }
    }

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
        self.inner_parser.parse(input).map(Box::new)
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        self.inner_parser.complete(input)
    }
}

impl<Ctx, T: Parsable<Ctx>> Parsable<Ctx> for Box<T> {
    type Parser = BoxParser<Ctx, T>;
}

pub struct OptionParser<Ctx, T: Parsable<Ctx>> {
    inner_parser: T::Parser,
}

impl<Ctx, T: Parsable<Ctx>> Parser<Ctx> for OptionParser<Ctx, T> {
    type Value = Option<T>;

    fn create(ctx: Ctx) -> Self {
        OptionParser {
            inner_parser: T::new_parser(ctx),
        }
    }

    fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
        if has_tokens(input) {
            self.inner_parser.parse(input).map(Some)
        } else {
            ParseResult::Parsed(None, input)
        }
    }

    fn complete<'a>(&self, input: &'a str) -> CompletionResult<'a> {
        self.inner_parser.complete(input)
    }
}

impl<Ctx, T: Parsable<Ctx>> Parsable<Ctx> for Option<T> {
    type Parser = OptionParser<Ctx, T>;
}

#[cfg(test)]
mod tests {
    use super::VecParser;
    use crate::{CompletionResult, Parsable, ParseError, ParseResult, Parser};

    mod vec_parser {
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

        #[test]
        fn suggest_first() {
            let parser: VecParser<(), bool> = VecParser::create(());
            assert_eq!(
                parser.complete("tr"),
                CompletionResult::Suggestions(vec!["ue".into()])
            );
        }

        #[test]
        fn suggest_not_first() {
            let parser: VecParser<(), bool> = VecParser::create(());
            assert_eq!(
                parser.complete("true fa"),
                CompletionResult::Suggestions(vec!["lse".into()])
            );
        }

        #[test]
        fn suggestion_consumed() {
            let parser: VecParser<(), bool> = VecParser::create(());
            assert_eq!(
                parser.complete("true false "),
                CompletionResult::Consumed("")
            );
        }

        #[test]
        fn suggestion_nested() {
            let parser: VecParser<(), Vec<bool>> = VecParser::create(());
            assert_eq!(
                parser.complete("(true false) (false) (fal"),
                CompletionResult::Suggestions(vec!["se".into()]),
            );
        }

        #[test]
        fn suggestion_nested_closed() {
            let parser: VecParser<(), Vec<bool>> = VecParser::create(());
            assert_eq!(
                parser.complete("(true false) (false) (fal)"),
                CompletionResult::Consumed(""),
            );
        }
    }

    mod tuple_parser {
        use super::*;

        #[test]
        fn parse_tuple() {
            let parser =
                <(u8, (u16, bool), (i32, i32, i32), (bool,)) as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2 true 4 5 6 false remaining"),
                ParseResult::Parsed((1, (2, true), (4, 5, 6), (false,)), "remaining"),
            );
            assert_eq!(
                parser.parse("1 (2 true) (4 5 6) (false) remaining"),
                ParseResult::Parsed((1, (2, true), (4, 5, 6), (false,)), "remaining"),
            );
        }

        #[test]
        fn parse_wrong_tokens_number() {
            let parser = <(u8, (u8, u8)) as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("1 2"),
                ParseResult::Failed(ParseError::token_required("integer"))
            );
            assert_eq!(
                parser.parse("1 (3 4 5)"),
                ParseResult::Failed(ParseError::unexpected_token("5".into()))
            );
        }

        #[test]
        fn parse_ivalid_token() {
            let parser = <(u8, u8) as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("5 abc"),
                ParseResult::Failed(ParseError::token_parse("abc".into(), None, "integer"))
            );
        }

        #[test]
        fn completion() {
            let parser = <(u8, (bool, u8)) as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("5 fa"),
                CompletionResult::Suggestions(vec!["lse".into()])
            );
            assert_eq!(parser.complete("5 false 4"), CompletionResult::empty());
            assert_eq!(
                parser.complete("5 false 4 "),
                CompletionResult::Consumed("")
            );
            assert_eq!(
                parser.complete("5 false 4 6"),
                CompletionResult::Consumed("6")
            );
        }
    }

    mod box_parser {
        use super::*;

        #[test]
        fn parse() {
            let parser = <Box<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("true 10"),
                ParseResult::Parsed(Box::new(true), "10")
            );
        }

        #[test]
        fn completion() {
            let parser = <Box<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("tr"),
                CompletionResult::Suggestions(vec!["ue".into()])
            );
        }
    }

    mod option_parser {
        use super::*;

        #[test]
        fn parse_some() {
            let parser = <Option<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.parse("true remaining"),
                ParseResult::Parsed(Some(true), "remaining")
            );
        }

        #[test]
        fn parse_none() {
            let parser = <Option<bool> as Parsable<()>>::new_parser(());
            assert_eq!(parser.parse(""), ParseResult::Parsed(None, ""));
            assert_eq!(parser.parse(")"), ParseResult::Parsed(None, ")"));
        }

        #[test]
        fn completion() {
            let parser = <Option<bool> as Parsable<()>>::new_parser(());
            assert_eq!(
                parser.complete("tr"),
                CompletionResult::Suggestions(vec!["ue".into()])
            );
        }
    }
}
