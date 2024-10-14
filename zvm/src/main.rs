// NOTE: Everything in this zvm project is half-broken. Im going to probably
// redo all of this and revisit it in the future. I still want to be able to
// compile to my own 'ZVM' bytecode for research and learning...
// the code is still half-usable so i dont have to rewrite EVERYTHING from scratch, though i might
// not mind that in the future. TO LLVM!!!!!!

use zvm::vm::VM;

const PATH: &str = "./zeal/src/scripts/loops.zl";

fn main() -> anyhow::Result<()> {
    const SOURCE: &str = "4 * 3 + 10 / 5 - 6\n\n"; //\nlet x = 5 + 5 + (5 * 3)\n\n";
                                                   //

    // let ast = TokBuffer::read_file(PATH)?;
    // let ast = Ast::from_file(PATH)?;
    // println!("{ast}");

    let mut vm = VM::new();
    let v = vm.exec_file(PATH)?;
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
