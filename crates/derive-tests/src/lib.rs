#![cfg(test)]

use cmd_parser::error::ParseError;
use cmd_parser::testing::{test_parse, token};
use cmd_parser::Parsable;

mod unit_struct {
    use super::*;

    #[derive(Parsable, Debug, PartialEq, Eq)]
    struct Unit;

    test_parse!(empty, Unit, "" => Ok(Unit, None));
    test_parse!(
        followed_by_token, Unit,
        "remaining" => Ok(Unit, Some(token!("remaining", last)))
    );
    test_parse!(
        followed_by_attr, Unit,
        "--remaining" => Ok(Unit, Some(token!(--"remaining", last)))
    );
    // test_complete!(complete_empty, "" => Consumed(""));
    // test_complete!(complete_followed_by_token, "remaining" => Consumed("remaining"));
    // test_complete!(complete_followed_by_attr, "--remaining" => Consumed("--remaining"));
}

mod newtype_struct {

    use super::*;

    #[derive(Parsable, Debug, PartialEq, Eq)]
    struct Newtype(u16);

    test_parse!(
        require_token, Newtype,
        "" => Error(ParseError::token_required().expected("integer"))
    );
    test_parse!(
        parse_error, Newtype,
        "abc" => Error(ParseError::invalid(token!("abc", last), None).expected("integer"))
    );
    test_parse!(
        success, Newtype,
        "15 abc" => Ok(Newtype(15), Some(token!("abc", last)))
    );
    test_parse!(
        unrecognized_attr, Newtype,
        "--attr 15 abc" => Unrecognized(token!(--"attr"), Some(token!("15")))
    );

    // test_complete!(complere_empty_if_not_terminated, "abc" => []);
    // test_complete!(complete_consumes_if_followed_by_ws, "15 abc" => Consumed("abc"));
}

mod multiple_args {
    use super::*;

    #[derive(Debug, Parsable, PartialEq)]
    struct Multiple {
        a: i32,
        b: f64,
    }

    test_parse!(
        success, Multiple,
        "1 2 3" => Ok(Multiple { a: 1, b: 2.0 }, Some(token!("3", last)))
    );
    test_parse!(
        unrecognized_attr, Multiple,
        "--attr 1 2 3" => Unrecognized(token!(--"attr"), Some(token!("1")))
    );
    test_parse!(
        attr_between_fields, Multiple,
        "1 --attr 2 3" => Error(ParseError::unknown(token!(--"attr")))
    );
    test_parse!(
        attr_after_required, Multiple,
        "1 2 --attr 3" => Ok(Multiple { a: 1, b: 2.0 }, Some(token!(--"attr")))
    );
    // test_complete!(complete_attr_between_fields, "1 --attr 2 3" => []);
    // test_success!(attr_after_fields, "1 2 --attr 3" => (TestParsable{ a: 1, b: 2.0 }, "--attr 3"));
    // test_complete!(complete_attr_after_fields, "1 2 --attr 3" => Consumed("--attr 3"));
}

mod custom_ctx {
    use super::*;
    use cmd_parser::{tokens::TokenStream, ParseResult, Parser};

    struct MyParser {
        multiplier: u8,
        u8_parser: <u8 as Parsable<u8>>::Parser,
    }

    impl Parser<u8> for MyParser {
        type Value = u8;

        fn create(ctx: u8) -> Self {
            MyParser {
                multiplier: ctx,
                u8_parser: <u8 as Parsable<u8>>::new_parser(ctx),
            }
        }

        fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
            let (value, remaining) = Parser::<u8>::parse(&self.u8_parser, input)?;
            Ok((value * self.multiplier, remaining))
        }

        fn complete<'a>(&self, input: TokenStream<'a>) -> cmd_parser::CompletionResult<'a> {
            Parser::<u8>::complete(&self.u8_parser, input)
        }
    }

    #[derive(Debug, PartialEq, Eq, Parsable)]
    #[cmd(ctx = "u8")]
    struct NewtypeCustomCtx(#[cmd(parser = "MyParser")] u8);

    #[test]
    fn parse() {
        let stream = TokenStream::new("5 2");
        let parser = NewtypeCustomCtx::new_parser(4);
        let (value, remaining) = Parser::<u8>::parse(&parser, stream).unwrap();
        assert_eq!(value, NewtypeCustomCtx(20));
        assert_eq!(remaining.peek().unwrap().unwrap(), token!("2", last));
    }
}

mod custom_ctx_bounds {
    use super::*;
    use cmd_parser::{tokens::TokenStream, ParseResult, Parser};

    trait CustomCtx {
        fn get_multiplier(&self) -> u8;
    }

    struct MyParser<Ctx: CustomCtx> {
        multiplier: u8,
        u8_parser: <u8 as Parsable<Ctx>>::Parser,
    }

    impl<Ctx: CustomCtx> Parser<Ctx> for MyParser<Ctx> {
        type Value = u8;

        fn create(ctx: Ctx) -> Self {
            MyParser {
                multiplier: ctx.get_multiplier(),
                u8_parser: <u8 as Parsable<Ctx>>::new_parser(ctx),
            }
        }

        fn parse<'a>(&self, input: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
            let (value, remaining) = Parser::<u8>::parse(&self.u8_parser, input)?;
            Ok((value * self.multiplier, remaining))
        }

        fn complete<'a>(&self, input: TokenStream<'a>) -> cmd_parser::CompletionResult<'a> {
            Parser::<u8>::complete(&self.u8_parser, input)
        }
    }

    #[derive(Debug, PartialEq, Eq, Parsable)]
    #[cmd(ctx_bounds = "CustomCtx")]
    struct NewtypeCustomCtx(#[cmd(parser = "MyParser<CmdParserCtx>")] u8);

    #[derive(Clone)]
    struct MockCtx;

    impl CustomCtx for MockCtx {
        fn get_multiplier(&self) -> u8 {
            4
        }
    }

    #[test]
    fn parse() {
        let stream = TokenStream::new("5 2");
        let parser = NewtypeCustomCtx::new_parser(MockCtx);
        let (value, remaining) = Parser::<MockCtx>::parse(&parser, stream).unwrap();
        assert_eq!(value, NewtypeCustomCtx(20));
        assert_eq!(remaining.peek().unwrap().unwrap(), token!("2", last));
    }
}

mod some_optional {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Parsable)]
    struct WithOptional(
        u16,
        #[cmd(attr(opt), default = "5")] usize,
        #[cmd(attr(yes = "true", false = "false"))] bool,
    );

    test_parse!(
        not_enough_tokens, WithOptional,
        "" => Error(ParseError::token_required().expected("integer"))
    );
    test_parse!(
        all_optional_missing, WithOptional,
        "10" => Ok(WithOptional(10, 5, false), None)
    );
    test_parse!(
        has_usize, WithOptional,
        "10 --opt 2 rest" => Ok(WithOptional(10, 2, false), Some(token!("rest", last)))
    );
    test_parse!(
        has_attr_with_value, WithOptional,
        "10 --yes rest" => Ok(WithOptional(10, 5, true), Some(token!("rest", last)))
    );
    test_parse!(
        has_all_fields, WithOptional,
        "10 --yes --opt 7 rest" => Ok(WithOptional(10, 7, true), Some(token!("rest", last)))
    );
    test_parse!(
        stops_after_required_finished, WithOptional,
        "10 --unknown --yes rest" => Ok(WithOptional(10, 5, false), Some(token!(--"unknown")))
    );
    test_parse!(
        keeps_taking_tokens, WithOptional,
        "10 --yes --unknown rest" => Ok(WithOptional(10, 5, true), Some(token!(--"unknown")))
    );
    test_parse!(
        unrecognized_attr, WithOptional,
        "--unknown 10 rest" => Unrecognized(token!(--"unknown"), Some(token!("10")))
    );

    // test_complete!(complete_attr_dashes_only, "--" => ["false", "opt", "yes"]);
    // test_complete!(complete_attr_partial, "--y" => ["es"]);
    // test_complete!(complete_attr_unknown, "--l" => Unrecognized);
}

mod all_optional {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Parsable)]
    struct AllOptional {
        #[cmd(attr(a))]
        a: u16,
        #[cmd(attr(b))]
        b: u16,
        #[cmd(attr(c))]
        c: u16,
    }

    test_parse!(
        none_specified, AllOptional,
        "abc" => Ok(AllOptional { a: 0, b: 0, c: 0 }, Some(token!("abc", last)))
    );
    test_parse!(
        none_specified_attr, AllOptional,
        "--unknown abc" => Ok(AllOptional { a: 0, b: 0, c: 0 }, Some(token!(--"unknown")))
    );
    test_parse!(
        only_one, AllOptional,
        "--a 10 abc" => Ok(AllOptional { a: 10, b: 0, c: 0 }, Some(token!("abc", last)))
    );
    test_parse!(
        two_specified, AllOptional,
        "--a 10 --b 20 abc" => Ok(AllOptional { a: 10, b: 20, c: 0 }, Some(token!("abc", last)))
    );
    test_parse!(
        all_specified, AllOptional,
        "--a 10 --b 20 --c 30 abc" => Ok(AllOptional { a: 10, b: 20, c: 30 }, Some(token!("abc", last)))
    );
    test_parse!(
        stop_on_unknown_attr, AllOptional,
        "--a 10 --unknown --b 20" => Ok(AllOptional { a: 10, b: 0, c: 0 }, Some(token!(--"unknown")))
    );

    // test_complete!(complete_stop_on_unknown_attr, "--a 10 --unknown --a 10" => Consumed("--unknown --a 10"));
}

mod some_default {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Parsable)]
    struct SomeDefault(#[cmd(default)] u8, #[cmd(default = "5")] u8, u8);

    test_parse!(
        single_specified, SomeDefault,
        "10 abc" => Ok(SomeDefault(0, 5, 10), Some(token!("abc", last)))
    );
    test_parse!(
        token_required, SomeDefault,
        "" => Error(ParseError::token_required().expected("integer"))
    );
    test_parse!(
        unrecognized_attr, SomeDefault,
        "--unknown abc" => Unrecognized(token!(--"unknown"), Some(token!("abc", last)))
    );
}

mod all_default {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Parsable)]
    struct AllDefault(#[cmd(default)] u8, #[cmd(default = "5")] u8);

    test_parse!(empty, AllDefault, "" => Ok(AllDefault(0, 5), None));
    test_parse!(
        followed_by_token, AllDefault,
        "remaining" => Ok(AllDefault(0, 5), Some(token!("remaining", last)))
    );
    test_parse!(
        followed_by_attr, AllDefault,
        "--remaining" => Ok(AllDefault(0, 5), Some(token!(--"remaining", last)))
    );
}

// mod enum_simple {
//     use super::*;

//     #[derive(Debug, PartialEq, Eq, Parsable)]
//     enum TestParsable {
//         First,
//         Second,
//     }

//     test_success!(parse_first, "first remaining" => (TestParsable::First, "remaining"));
//     test_success!(parse_second, "second remaining" => (TestParsable::Second, "remaining"));
//     test_unrecognized_variant!(unknown_variant, "third remaining" => "third");
//     test_failed!(variant_required, "" => ParseError::token_required("variant"));
//     test_unrecognized_attr!(unrecognized_attr, "--unknown first" => ("unknown", "first"));
// }

// mod enum_ignore_aliases {
//     use super::*;

//     #[derive(Debug, PartialEq, Eq, Parsable)]
//     enum TestParsable {
//         #[cmd(alias = "alias")]
//         Real(u8),
//         #[cmd(ignore)]
//         #[allow(dead_code)]
//         Ignored,
//         #[cmd(rename = "new-name")]
//         Renamed,
//     }

//     test_success!(parse_normal, "real 10 remaining" => (TestParsable::Real(10), "remaining"));
//     test_failed!(returns_inner_error, "real nan" => ParseError::token_parse("nan".into(), None, "integer"));
//     test_success!(parse_alias, "alias 10 remaining" => (TestParsable::Real(10), "remaining"));
//     test_success!(parse_renamed, "new-name remaining" => (TestParsable::Renamed, "remaining"));
//     test_unrecognized_variant!(cannot_parse_ignored, "ignored remaining" => "ignored");
//     test_unrecognized_variant!(cannot_parse_renamed, "renamed remaining" => "renamed");
// }

// mod enum_transparent {
//     use super::*;

//     #[derive(Debug, PartialEq, Eq, Parsable)]
//     enum Letter {
//         A,
//         B,
//         C,
//     }

//     #[derive(Debug, PartialEq, Eq, Parsable)]
//     enum Number {
//         One,
//         Two,
//         Three,
//     }

//     #[derive(Debug, PartialEq, Eq, Parsable)]
//     enum TestParsable {
//         #[cmd(transparent)]
//         Letter(Letter),
//         #[cmd(transparent)]
//         Number(Number),
//     }

//     test_success!(parse_letter_1, "a abc" => (TestParsable::Letter(Letter::A), "abc"));
//     test_success!(parse_letter_2, "b abc" => (TestParsable::Letter(Letter::B), "abc"));
//     test_success!(parse_number, "two abc" => (TestParsable::Number(Number::Two), "abc"));
//     test_unrecognized_variant!(unknown_variant, "five remaining" => "five");
// }
