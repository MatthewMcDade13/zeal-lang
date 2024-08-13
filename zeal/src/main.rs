use zeal::{
    ast::Ast,
    compiler::{opcode::Op, Archon},
    vm::VM,
};

fn main() -> anyhow::Result<()> {
    const SOURCE: &'static str = "4 * 3 + 10 / 5 - 6\n\n"; //\nlet x = 5 + 5 + (5 * 3)\n\n";

    let mut vm = VM::new();
    let v = vm.exec_source(SOURCE)?;
    println!("{v}");
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
