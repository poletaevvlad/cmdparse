use cmdparse::error::ParseError;
use cmdparse::parsers::{
    CollectionParser, ParsableCollection, ParsableTransformation, TransformParser,
};
use cmdparse::{parse_parser, Parsable};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use smallvec::SmallVec;

#[derive(Default)]
struct ParsableSmallVec<A: smallvec::Array>(SmallVec<A>);

impl<A: smallvec::Array> ParsableCollection for ParsableSmallVec<A> {
    type Item = A::Item;

    fn append(&mut self, item: Self::Item) {
        self.0.push(item)
    }
}

impl<A: smallvec::Array> ParsableTransformation<SmallVec<A>> for ParsableSmallVec<A> {
    type Input = Self;

    fn transform(input: Self::Input) -> Result<SmallVec<A>, ParseError<'static>> {
        Ok(input.0)
    }
}

type SmallVecParser<Ctx, A> = TransformParser<
    CollectionParser<ParsableSmallVec<A>, <<A as smallvec::Array>::Item as Parsable<Ctx>>::Parser>,
    ParsableSmallVec<A>,
    SmallVec<A>,
>;

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => match parse_parser::<(), SmallVecParser<(), [i32; 5]>>(&line, ()) {
                Ok(vector) => println!("<< {:?}", vector),
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
