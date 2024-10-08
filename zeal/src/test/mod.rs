#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{
        ast::Ast,
        compiler::{opcode::Op, Archon},
        sys::{
            self,
            mem::{KB, MB1},
        },
        vm::VM,
    };

    use super::*;

    const SOURCE: &'static str = "4 * 3 + 10 / 5 - 6\n";
    const SRC_LET: &'static str = "let x = 5 + 5 + (5 * 3)\n\n";
    // const SOURCE_OPS:

    #[repr(C)]
    #[derive(Debug, Clone, Copy, bytemuck::AnyBitPattern, bytemuck::NoUninit)]
    struct Point {
        x: i32,
        y: i32,
    }

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn slab_stack() -> anyhow::Result<()> {
        const SIZE: usize = KB * 512;
        let mut stack = sys::mem::SlabStack::<SIZE>::zeroed();
        let num_handle = stack.push(46);
        let point_handle = stack.push(Point { x: 1, y: 2 });

        let num = stack.lookup(num_handle);
        let point = stack.lookup(point_handle);

        assert_eq!(*num, 46);
        assert_eq!(point.x, 1);
        assert_eq!(point.y, 2);

        Ok(())
    }

    #[test]
    fn compile_test() -> anyhow::Result<()> {
        let ast = Ast::from_str(SOURCE)?;
        let code = Archon::compile(&ast)?;

        // const EXPECTED = []

        // assert_eq!(code.as_slice()),
        Ok(())
        // let bytes = Archon::compile(&ast)?;
    }

    #[test]
    fn calc_vars_test() -> anyhow::Result<()> {
        const PATH: &str = "src/test/scripts/calc_vars.zeal";

        let ast = Ast::from_file(PATH)?;
        let code = Archon::compile(&ast)?;
        Ok(())
        // let ast = Ast::from_file("src/test/scripts/calc_vars.zeal")?;
    }

    #[test]
    fn parse_ast_funcs() -> anyhow::Result<()> {
        const PATH: &str = "src/scripts/funcs.zl";
        let ast = Ast::from_file(PATH)?;
        Ok(())
    }
}
