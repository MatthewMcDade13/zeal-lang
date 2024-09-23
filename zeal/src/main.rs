use log::info;
use zeal::{
    ast::{lex::TokBuffer, Ast},
    compiler::{opcode::Op, Archon},
    vm::VM,
};

const PATH: &'static str = "./src/scripts/funcs.zl";
fn main() -> anyhow::Result<()> {
    const SOURCE: &'static str = "4 * 3 + 10 / 5 - 6\n\n"; //\nlet x = 5 + 5 + (5 * 3)\n\n";
                                                           //
    env_logger::init();

    // let ast = TokBuffer::read_file(PATH)?;
    // let ast = Ast::from_file(PATH)?;
    // println!("{ast}");

    try_compile()?;
    // let mut vm = VM::new();
    // let v = vm.exec_file(PATH)?;
    // println!("EXEC RESULT => {v}");
    // let x = TokBuffer::read_string(s)?;

    // let x = Ast::from_str(SOURCE)?;
    // println!("{x}");
    // let code = Archon::compile(&x)?;
    // println!("{a:?}");
    // println!("{}", c.debug_dissassembly());
    // for t in x.slice() {
    // println!("{}", t);
    // }

    Ok(())
}

fn try_run() -> anyhow::Result<()> {
    let mut vm = VM::new();
    vm.exec_file(PATH)?;
    Ok(())
}

fn try_compile() -> anyhow::Result<()> {
    let ast = Ast::from_file(PATH)?;
    log::info!("\n{ast}");
    let c = Archon::compile(&ast)?;
    log::info!("\n{}", c.chunk().to_string());
    Ok(())
}

fn try_parse_ast() -> anyhow::Result<()> {
    let ast = Ast::from_file(PATH)?;
    println!("{ast}");
    Ok(())
}
