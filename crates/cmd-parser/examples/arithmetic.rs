use std::io::Write;

use cmd_parser::error::ParseError;
use cmd_parser::parsers::{ParsableTransformation, TransformParser};
use cmd_parser::{parse_parser, Parsable};

#[derive(Debug, Parsable)]
enum Expression {
    #[cmd(transparent)]
    Value(f64),
    #[cmd(rename = "+")]
    Add(
        #[cmd(parser = "EvaluationParser")] f64,
        #[cmd(parser = "EvaluationParser")] f64,
    ),
    #[cmd(rename = "-")]
    Subtract(
        #[cmd(parser = "EvaluationParser")] f64,
        #[cmd(parser = "EvaluationParser")] f64,
    ),
    #[cmd(rename = "*")]
    Multiply(
        #[cmd(parser = "EvaluationParser")] f64,
        #[cmd(parser = "EvaluationParser")] f64,
    ),
    #[cmd(rename = "/")]
    Divide(
        #[cmd(parser = "EvaluationParser")] f64,
        #[cmd(parser = "EvaluationParser")] f64,
    ),
}

impl ParsableTransformation<f64> for Expression {
    type Input = Expression;

    fn transform(input: Self::Input) -> Result<f64, ParseError<'static>> {
        match input {
            Expression::Value(value) => Ok(value),
            Expression::Add(a, b) => Ok(a + b),
            Expression::Subtract(a, b) => Ok(a - b),
            Expression::Multiply(a, b) => Ok(a * b),
            Expression::Divide(_, b) if b == 0.0 => {
                Err(ParseError::custom("cannot divide by zero"))
            }
            Expression::Divide(a, b) => Ok(a / b),
        }
    }
}

type EvaluationParser = TransformParser<ExpressionParser, Expression, f64>;

fn main() -> std::io::Result<()> {
    let stdin = std::io::stdin();
    let mut buffer = String::new();
    loop {
        print!("> ");
        std::io::stdout().flush()?;
        stdin.read_line(&mut buffer)?;
        match parse_parser::<_, EvaluationParser>(&buffer, ()) {
            Ok(value) => println!("< {:?}", value),
            Err(err) => println!("Error: {}", err),
        }
        buffer.clear();
    }
}
