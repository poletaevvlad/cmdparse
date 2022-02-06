use cmd_parser::error::ParseError;
use cmd_parser::testing::{test_complete, test_parse, token};
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

    test_complete!(complete_empty, Unit, "" => {
        consumed: true,
        remaining: Some(None),
        suggestions: [],
    });
    test_complete!(complete_token, Unit, "remaining" => {
        consumed: true,
        remaining: Some(Some(token!("remaining", last))),
        suggestions: [],
    });
    test_complete!(complete_attribute, Unit, "--remaining" => {
        consumed: true,
        remaining: Some(Some(token!(--"remaining", last))),
        suggestions: [],
    });
}

mod newtype_struct {
    use super::*;

    #[derive(Parsable, Debug, PartialEq, Eq)]
    struct Newtype(bool);

    test_parse!(
        require_token, Newtype,
        "" => Error(ParseError::token_required().expected("boolean"))
    );
    test_parse!(
        parse_error, Newtype,
        "abc" => Error(ParseError::invalid(token!("abc", last), None).expected("boolean"))
    );
    test_parse!(
        success, Newtype,
        "true abc" => Ok(Newtype(true), Some(token!("abc", last)))
    );
    test_parse!(
        unrecognized_attr, Newtype,
        "--attr false abc" => Unrecognized(token!(--"attr"), Some(token!("false")))
    );

    test_complete!(complete_inner_not_terminated, Newtype, "fal" => {
        consumed: true,
        remaining: None,
        suggestions: ["se"],
    });
    test_complete!(complete_consumed_followed_by_ws, Newtype, "false " => {
        consumed: true,
        remaining: Some(None),
        suggestions: [],
    });
    test_complete!(complete_unrecognized_attr, Newtype, "--unknown false" => {
        consumed: false,
        remaining: Some(Some(token!(--"unknown"))),
        suggestions: [],
    });
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

    test_complete!(complete_attr_between_fields, Multiple, "1 --attr" => {
        consumed: true,
        remaining: None,
        suggestions: [],
    });
    test_complete!(complete_attr_after_fields, Multiple, "1 2 --attr" => {
        consumed: true,
        remaining: Some(Some(token!(--"attr", last))),
        suggestions: [],
    });
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

    test_complete!(complete_attr_dashes_only, WithOptional, "--" => {
        consumed: false,
        remaining: Some(Some(token!(--"", last))),
        suggestions: ["false", "opt", "yes"],
    });
    test_complete!(complete_attr_partial, WithOptional, "--y" => {
        consumed: false,
        remaining: Some(Some(token!(--"y", last))),
        suggestions: ["es"],
    });
    test_complete!(complete_attr_partial_consumed, WithOptional, " 5 --y" => {
        consumed: true,
        remaining: Some(Some(token!(--"y", last))),
        suggestions: ["es"],
    });
    test_complete!(complete_attr_last, WithOptional, "5 --opt 3 --fa" => {
        consumed: true,
        remaining: Some(Some(token!(--"fa", last))),
        suggestions: ["lse"],
    });
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

    test_complete!(complete_attr_dashes_only, AllOptional, "--" => {
        consumed: true,
        remaining: Some(Some(token!(--"", last))),
        suggestions: ["a", "b", "c"],
    });
    test_complete!(complete_attr_partial, AllOptional, "--a 5 --b 10 --c" => {
        consumed: true,
        remaining: Some(Some(token!(--"c", last))),
        suggestions: [],
    });
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

    test_complete!(complete_empty, AllDefault, "" => {
        consumed: true,
        remaining: Some(None),
        suggestions: [],
    });
    test_complete!(complete_token, AllDefault, "remaining" => {
        consumed: true,
        remaining: Some(Some(token!("remaining", last))),
        suggestions: [],
    });
    test_complete!(complete_attribute, AllDefault, "--remaining" => {
        consumed: true,
        remaining: Some(Some(token!(--"remaining", last))),
        suggestions: [],
    });
}

mod enum_simple {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Parsable)]
    enum Enum {
        First,
        Second,
    }

    test_parse!(
        parse_first, Enum,
        "first remaining" => Ok(Enum::First, Some(token!("remaining", last)))
    );
    test_parse!(
        parse_second, Enum,
        "second remaining" => Ok(Enum::Second, Some(token!("remaining", last)))
    );
    test_parse!(
        unknown_variant, Enum,
        "third remaining" => Unrecognized(token!("third"), Some(token!("remaining", last)))
    );
    test_parse!(
        variant_required, Enum,
        "" => Error(ParseError::token_required().expected("variant"))
    );
    test_parse!(
        unrecognized_attr, Enum,
        "--unknown remaining" => Unrecognized(token!(--"unknown"), Some(token!("remaining", last)))
    );

    test_complete!(complete_empty, Enum, "" => {
        consumed: false,
        remaining: None,
        suggestions: [],
    });
    test_complete!(complete_partial_variant, Enum, "fir" => {
        consumed: false,
        remaining: Some(Some(token!("fir", last))),
        suggestions: ["st"],
    });
    test_complete!(complete_complete_variant, Enum, "second abc" => {
        consumed: true,
        remaining: Some(Some(token!("abc", last))),
        suggestions: [],
    });
}

mod enum_ignore_aliases {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Parsable)]
    enum WithAliases {
        #[cmd(alias = "alias")]
        Real(u8),
        #[cmd(ignore)]
        #[allow(dead_code)]
        Ignored,
        #[cmd(rename = "new-name")]
        Renamed,
    }

    test_parse!(
        parse_normal, WithAliases,
        "real 10 abc" => Ok(WithAliases::Real(10), Some(token!("abc", last)))
    );
    test_parse!(
        returns_inner_error, WithAliases,
        "real nan" => Error(ParseError::invalid(token!("nan", last), None).expected("integer"))
    );
    test_parse!(
        parse_alias, WithAliases,
        "alias 10 abc" => Ok(WithAliases::Real(10), Some(token!("abc", last)))
    );
    test_parse!(
        parse_renamed, WithAliases,
        "new-name abc" => Ok(WithAliases::Renamed, Some(token!("abc", last)))
    );
    test_parse!(
        cannot_parse_ignored, WithAliases,
        "ignored abc" => Unrecognized(token!("ignored"), Some(token!("abc", last)))
    );
    test_parse!(
        cannot_parse_renamed, WithAliases,
        "renamed abc" => Unrecognized(token!("renamed"), Some(token!("abc", last)))
    );

    test_complete!(complete_suggestions_normal, WithAliases, "r" => {
        consumed: false,
        remaining: Some(Some(token!("r", last))),
        suggestions: ["eal"]
    });
    test_complete!(complete_suggestions_alias, WithAliases, "a" => {
        consumed: false,
        remaining: Some(Some(token!("a", last))),
        suggestions: ["lias"]
    });
    test_complete!(complete_suggestions_renamed, WithAliases, "n" => {
        consumed: false,
        remaining: Some(Some(token!("n", last))),
        suggestions: ["ew-name"]
    });
    test_complete!(complete_recognized, WithAliases, "new-name " => {
        consumed: true,
        remaining: Some(None),
        suggestions: []
    });
    test_complete!(complete_unrecognized, WithAliases, "unknown " => {
        consumed: false,
        remaining: Some(Some(token!("unknown"))),
        suggestions: []
    });
}

mod enum_transparent {
    use super::*;

    #[derive(Debug, PartialEq, Eq, Parsable)]
    enum Letter {
        A,
        B,
        C,
    }

    #[derive(Debug, PartialEq, Eq, Parsable)]
    enum Number {
        One,
        Two,
        Three,
    }

    #[derive(Debug, PartialEq, Eq, Parsable)]
    enum WithTransparent {
        #[cmd(transparent)]
        Letter(Letter),
        #[cmd(transparent)]
        Number(Number),
    }

    test_parse!(
        parse_letter_1, WithTransparent,
        "a abc" => Ok(WithTransparent::Letter(Letter::A), Some(token!("abc", last)))
    );
    test_parse!(
        parse_letter_2, WithTransparent,
        "b abc" => Ok(WithTransparent::Letter(Letter::B), Some(token!("abc", last)))
    );
    test_parse!(
        parse_number, WithTransparent,
        "two abc" => Ok(WithTransparent::Number(Number::Two), Some(token!("abc", last)))
    );
    test_parse!(
        parse_variant, WithTransparent,
        "five abc" => Unrecognized(token!("five"), Some(token!("abc", last)))
    );

    test_complete!(complete_number_variant, WithTransparent, "t" => {
        consumed: false,
        remaining: Some(Some(token!("t", last))),
        suggestions: ["wo", "hree"],
    });
    test_complete!(complete_recognized, WithTransparent, "a " => {
        consumed: true,
        remaining: Some(None),
        suggestions: []
    });
    test_complete!(complete_unrecognized, WithTransparent, "unknown " => {
        consumed: false,
        remaining: Some(Some(token!("unknown"))),
        suggestions: []
    });
}