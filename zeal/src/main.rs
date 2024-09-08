use zeal::{
    ast::{lex::TokBuffer, Ast},
    compiler::{opcode::Op, Archon},
    vm::VM,
};

const PATH: &'static str = "./src/scripts/loops.zl";
fn main() -> anyhow::Result<()> {
    const SOURCE: &'static str = "4 * 3 + 10 / 5 - 6\n\n"; //\nlet x = 5 + 5 + (5 * 3)\n\n";
                                                           //

    // let ast = TokBuffer::read_file(PATH)?;
    // let ast = Ast::from_file(PATH)?;
    // println!("{ast}");

    let mut vm = VM::new();
    let v = vm.exec_file(PATH)?;
    println!("EXEC RESULT => {v}");
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
