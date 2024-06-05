use lex::parse_value;
use logos::Logos;

mod lex;

fn main() -> anyhow::Result<()> {
    let mut l = lex::Token::lexer("{ \"asfd\": 123 }");
    let v = parse_value(&mut l)?;
    println!("{:?}", v);
    Ok(())
}
