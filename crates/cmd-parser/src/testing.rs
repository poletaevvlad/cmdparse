#[macro_export]
macro_rules! token {
    (--$text:literal $(, $mod:ident)?) => {
        $crate::tokens::Token::from_parts (
            $crate::tokens::TokenValue::Attribute($crate::tokens::RawLexeme::from_str($text)),
            token!(@internal is_last $($mod)?),
        )
    };
    ($text:literal $(, $mod:ident)?) => {
        $crate::tokens::Token::from_parts (
            $crate::tokens::TokenValue::Text($crate::tokens::RawLexeme::from_str($text)),
            token!(@internal is_last $($mod)?),
        )
    };

    (@internal is_last last) => {true};
    (@internal is_last ) => {false};
}
pub use token;

#[macro_export]
macro_rules! test_parse {
    ($name:ident, $type:ty, $input:literal => Ok($value:expr, $next_token:expr)) => {
        #[test]
        fn $name() {
            let parser = <$type as $crate::Parsable<()>>::new_parser(());
            let stream = $crate::tokens::TokenStream::new($input);
            let (result, remaining) = $crate::Parser::<()>::parse(&parser, stream).unwrap();
            assert_eq!(result, $value);
            assert_eq!(remaining.peek().transpose().unwrap(), $next_token);
        }
    };
    ($name:ident, $type:ty, $input:literal => Error($error:expr)) => {
        #[test]
        fn $name() {
            let parser = <$type as $crate::Parsable<()>>::new_parser(());
            let stream = $crate::tokens::TokenStream::new($input);
            let error = $crate::Parser::<()>::parse(&parser, stream).unwrap_err();
            match error {
                $crate::error::ParseFailure::Error(error) => assert_eq!(error, $error),
                $crate::error::ParseFailure::Unrecognized(unrecognized) => {
                    panic!("expected Error, but found {:?}", unrecognized)
                }
            }
        }
    };
    ($name:ident, $type:ty, $input:literal => Unrecognized($token:expr, $next_token:expr)) => {
        #[test]
        fn $name() {
            let parser = <$type as $crate::Parsable<()>>::new_parser(());
            let stream = $crate::tokens::TokenStream::new($input);
            let error = $crate::Parser::<()>::parse(&parser, stream).unwrap_err();
            match error {
                $crate::error::ParseFailure::Error(error) => {
                    panic!("expected Unrecognized, but found {:?}", error)
                }
                $crate::error::ParseFailure::Unrecognized(unrecognized) => {
                    assert_eq!(unrecognized.token(), $token);
                    assert_eq!(
                        unrecognized.remaining().peek().transpose().unwrap(),
                        $next_token
                    );
                }
            }
        }
    };
}
pub use test_parse;

#[macro_export]
macro_rules! test_complete {
    ($name:ident, $type:ty, $input:literal => { consumed: $consumed:expr, remaining: $remaining:expr, suggestions: [$($suggestion:expr),*] $(,)?}) => {
        #[test]
        #[allow(clippy::bool_assert_comparison)]
        fn $name() {
            let parser = <$type as Parsable<()>>::new_parser(());
            let stream = TokenStream::new($input);
            let result = $crate::Parser::<()>::complete(&parser, stream);
            assert_eq!(result.suggestions, HashSet::from([$($suggestion.into()),*]));
            assert_eq!(result.value_consumed, $consumed);
            assert_eq!(result.remaining.map(|input| input.peek().transpose().unwrap()), $remaining);
        }
    };
}
pub use test_complete;
