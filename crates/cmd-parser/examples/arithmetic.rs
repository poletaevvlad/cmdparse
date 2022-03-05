use cmd_parser::error::ParseError;
use cmd_parser::parsers::{ParsableTransformation, TransformParser};
use cmd_parser::{parse_parser, Parsable};
use rustyline::error::ReadlineError;
use rustyline::Editor;

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

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => match parse_parser::<_, EvaluationParser>(&line, ()) {
                Ok(value) => println!("<< {}", value),
                Err(err) => println!("Error: {}", err),
            },
            Err(ReadlineError::Interrupted | ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {}", err);
                break;
            }
        }
    }
}
