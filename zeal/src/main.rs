use zeal_ast::Ast;

const PATH: &str = "../test_scripts/loops.zl";

fn main() -> anyhow::Result<()> {
    let ast = Ast::from_file(PATH)?;
    println!("{ast}");
    Ok(())
}
