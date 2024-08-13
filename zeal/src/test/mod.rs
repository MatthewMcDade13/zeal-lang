#[cfg(test)]
mod tests {
    use crate::{
        ast::Ast,
        compiler::{opcode::Op, Archon},
        vm::VM,
    };

    use super::*;

    const SOURCE: &'static str = "4 * 3 + 10 / 5 - 6\n";
    const SRC_LET: &'static str = "let x = 5 + 5 + (5 * 3)\n\n";
    // const SOURCE_OPS:

    #[test]
    fn compile_test() -> anyhow::Result<()> {
        let ast = Ast::from_str(SOURCE)?;
        let code = Archon::compile(&ast)?;

        // const EXPECTED = []

        // assert_eq!(code.as_slice()),
        Ok(())
        // let bytes = Archon::compile(&ast)?;
    }
}
