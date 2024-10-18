// NOTE: Everything in this zvm project is half-broken. Im going to probably
// redo all of this and revisit it in the future. I still want to be able to
// compile to my own 'ZVM' bytecode for research and learning...
// the code is still half-usable so i dont have to rewrite EVERYTHING from scratch, though i might
// not mind that in the future. TO LLVM!!!!!!

use std::str::FromStr;

use zeal_ast::Ast;
use zvm::vm::VM;

const PATH: &str = "./test_scripts/loops.zl";

fn main() -> anyhow::Result<()> {
    //

    // let ast = TokBuffer::read_file(PATH)?;
    // let ast = Ast::from_file(PATH)?;
    // println!("{ast}");

    let mut vm = VM::new();
    vm.exec_file(PATH)?;
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
