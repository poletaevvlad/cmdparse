// mod x;

use cmd_parser::{Parsable, ParseError, ParseResult, Parser};

mod unit_struct {
    use super::*;

    #[derive(Parsable, Debug, PartialEq, Eq)]
    struct UnitStruct;

    #[test]
    fn derserializes_empty() {
        let parser = UnitStruct::new_parser(());
        assert_eq!(
            Parser::<()>::parse(&parser, "remaining"),
            ParseResult::Parsed(UnitStruct, "remaining")
        );
    }

    #[test]
    fn unrecognized_attr() {
        let parser = UnitStruct::new_parser(());
        assert_eq!(
            Parser::<()>::parse(&parser, "--unknown abc"),
            ParseResult::UnrecognizedAttribute("unknown".into(), "abc")
        );
    }
}

mod newtype_struct {
    use super::*;

    #[derive(Parsable, Debug, PartialEq, Eq)]
    struct NewtypeStruct(u16);

    #[test]
    fn require_token() {
        let parser = NewtypeStruct::new_parser(());
        assert_eq!(
            Parser::<()>::parse(&parser, ""),
            ParseResult::Failed(ParseError::token_required("integer")),
        );
    }

    #[test]
    fn parse_error() {
        let parser = NewtypeStruct::new_parser(());
        assert_eq!(
            Parser::<()>::parse(&parser, "abc"),
            ParseResult::Failed(ParseError::token_parse("abc".into(), None, "integer")),
        );
    }

    #[test]
    fn parse_success() {
        let parser = NewtypeStruct::new_parser(());
        assert_eq!(
            Parser::<()>::parse(&parser, "15 abc"),
            ParseResult::Parsed(NewtypeStruct(15), "abc"),
        );
    }

    #[test]
    fn parse_unknown_attribute() {
        let parser = NewtypeStruct::new_parser(());
        assert_eq!(
            Parser::<()>::parse(&parser, "--attr 15 abc"),
            ParseResult::UnrecognizedAttribute("attr".into(), "15 abc"),
        );
    }
}

mod multiple_args {
    use super::*;

    #[derive(Debug, Parsable, PartialEq)]
    struct TwoFields {
        a: i32,
        b: f64,
    }

    #[test]
    fn parse_success() {
        let parser = TwoFields::new_parser(());
        assert_eq!(
            Parser::<()>::parse(&parser, "1 2 3"),
            ParseResult::Parsed(TwoFields { a: 1, b: 2.0 }, "3")
        );
    }

    #[test]
    fn parse_unrecognized_attr() {
        let parser = TwoFields::new_parser(());
        assert_eq!(
            Parser::<()>::parse(&parser, "--attr 1 2 3"),
            ParseResult::UnrecognizedAttribute("attr".into(), "1 2 3"),
        );
    }

    #[test]
    fn parse_unrecognized_attr_between_fields() {
        let parser = TwoFields::new_parser(());
        assert_eq!(
            Parser::<()>::parse(&parser, "1 --attr 2 3"),
            ParseResult::Failed(ParseError::unknown_attribute("attr")),
        );
    }

    #[test]
    fn parse_unrecognized_attr_after_fields() {
        let parser = TwoFields::new_parser(());
        assert_eq!(
            Parser::<()>::parse(&parser, "1 2 --attr 3"),
            ParseResult::Parsed(TwoFields { a: 1, b: 2.0 }, "--attr 3")
        );
    }
}
