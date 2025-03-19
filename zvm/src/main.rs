use zvm::vm::VM;

const PATH: &str = "./test_scripts/loops.zl";

fn main() -> anyhow::Result<()> {
    //

    // let ast = TokBuffer::read_file(PATH)?;
    // let ast = Ast::from_file(PATH)?;
    // println!("{ast}");

    // let mut vm = VM::new();
    // vm.exec_file(PATH)?;
    let mut vm = VM::load_entrypoint(PATH)?;

    // println!("{}", vm.dump_stack());
    println!("dump:\n {}", vm.dump_bytecode());
    vm.exec()?;

    // println!("{vm}");
    // println!("EXEC RESULT => {v}");
    // let x = TokBuffer::read_string(s)?;

    // let x = Ast::from_str(PATH)?;
    // println!("{x}");
    // let code = Archon::compile(&x)?;
    // println!("{a:?}");
    // println!("{}", c.debug_dissassembly());
    // for t in x.slice() {
    // println!("{}", t);
    // }

    Ok(())
}
