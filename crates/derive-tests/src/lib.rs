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
            Parser::<()>::parse(&parser, "--unknown"),
            ParseResult::Unrecognized(ParseError::unknown_attribute("unknown"))
        );
    }
}
