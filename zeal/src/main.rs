use zeal::ast::Ast;

fn main() -> anyhow::Result<()> {
    let s = "let x = 5 + 5 + (5 * 3)\n\n";

    // let x = TokBuffer::read_string(s)?;
    let x = Ast::from_str(s)?;
    println!("{x}");
    // for t in x.slice() {
    // println!("{}", t);
    // }

    Ok(())
}
