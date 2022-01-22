use cmd_parser::{Parsable, ParseError, ParseResult, Parser};

macro_rules! test_success {
    ($name:ident, $input:expr => ($value:expr, $remaining:expr)) => {
        #[test]
        fn $name() {
            let parser = TestParsable::new_parser(());
            assert_eq!(
                Parser::<()>::parse(&parser, $input),
                ParseResult::Parsed($value, $remaining)
            );
        }
    };
}

macro_rules! test_unrecognized_attr {
    ($name:ident, $input:expr => ($token:expr, $remaining:expr)) => {
        #[test]
        fn $name() {
            let parser = TestParsable::new_parser(());
            assert_eq!(
                Parser::<()>::parse(&parser, $input),
                ParseResult::UnrecognizedAttribute($token.into(), $remaining)
            );
        }
    };
}

macro_rules! test_failed {
    ($name:ident, $input:expr => $err:expr) => {
        #[test]
        fn $name() {
            let parser = TestParsable::new_parser(());
            assert_eq!(
                Parser::<()>::parse(&parser, $input),
                ParseResult::Failed($err)
            );
        }
    };
}

mod unit_struct {
    use super::*;

    #[derive(Parsable, Debug, PartialEq, Eq)]
    struct TestParsable;

    test_success!(empty, "remaining" => (TestParsable, "remaining"));
    test_unrecognized_attr!(unrecognized_attr, "--unknown abc" => ("--unknown", "abc"));
}

mod newtype_struct {
    use super::*;

    #[derive(Parsable, Debug, PartialEq, Eq)]
    struct TestParsable(u16);

    test_failed!(requre_token, "" => ParseError::token_required("integer"));
    test_failed!(parse_error, "abc" => ParseError::token_parse("abc".into(), None, "integer"));
    test_success!(success, "15 abc" => (TestParsable(15), "abc"));
    test_unrecognized_attr!(unrecognized_attr, "--attr 15 abc" => ("attr", "15 abc"));
}

mod multiple_args {
    use super::*;

    #[derive(Debug, Parsable, PartialEq)]
    struct TestParsable {
        a: i32,
        b: f64,
    }

    test_success!(success, "1 2 3" => (TestParsable{ a: 1, b: 2.0 }, "3"));
    test_unrecognized_attr!(unrecognized_attr, "--attr 1 2 3" => ("attr", "1 2 3"));
    test_failed!(attr_between_fields, " 1 --attr 2 3" => ParseError::unknown_attribute("attr"));
    test_success!(attr_after_fields, " 1 2 --attr 3" => (TestParsable{ a: 1, b: 2.0 }, "--attr 3"));
}

mod custom_ctx {
    use super::*;

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

        fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
            Parser::<u8>::parse(&self.u8_parser, input).map(|val| val * self.multiplier)
        }

        fn complete<'a>(&self, input: &'a str) -> cmd_parser::CompletionResult<'a> {
            Parser::<u8>::complete(&self.u8_parser, input)
        }
    }

    #[derive(Debug, PartialEq, Eq, Parsable)]
    #[cmd(ctx = "u8")]
    struct NewtypeCustomCtx(#[cmd(parser = "MyParser")] u8);

    #[test]
    fn parse() {
        let parser = NewtypeCustomCtx::new_parser(4);
        assert_eq!(
            Parser::<u8>::parse(&parser, "5 2"),
            ParseResult::Parsed(NewtypeCustomCtx(20), "2")
        );
    }
}

mod custom_ctx_bounds {
    use super::*;

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

        fn parse<'a>(&self, input: &'a str) -> ParseResult<'a, Self::Value> {
            Parser::<u8>::parse(&self.u8_parser, input).map(|val| val * self.multiplier)
        }

        fn complete<'a>(&self, input: &'a str) -> cmd_parser::CompletionResult<'a> {
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
        let parser = NewtypeCustomCtx::new_parser(MockCtx);
        assert_eq!(
            Parser::<MockCtx>::parse(&parser, "5 2"),
            ParseResult::Parsed(NewtypeCustomCtx(20), "2")
        );
    }
}
