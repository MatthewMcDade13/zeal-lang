use zeal_ast::ast_mod::AstModule;

const PATH: &str = "../test_scripts/loops.zl";

fn main() -> anyhow::Result<()> {
    let ast = AstModule::from_file(PATH)?;
    println!("{ast}");
    Ok(())
}
