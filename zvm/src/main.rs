// use zeal::{
//     ast::{lex::TokBuffer, Ast},
//     compiler::{opcode::Op, Archon},
//     vm::VM,
// };
//
// const PATH: &'static str = "./zeal/src/scripts/loops.zl";
// fn main() -> anyhow::Result<()> {
//     const SOURCE: &'static str = "4 * 3 + 10 / 5 - 6\n\n"; //\nlet x = 5 + 5 + (5 * 3)\n\n";
//                                                            //
//
//     // let ast = TokBuffer::read_file(PATH)?;
//     // let ast = Ast::from_file(PATH)?;
//     // println!("{ast}");
//
//     let mut vm = VM::new();
//     let v = vm.exec_file(PATH)?;
//     // println!("EXEC RESULT => {v}");
//     // let x = TokBuffer::read_string(s)?;
//
//     // let x = Ast::from_str(SOURCE)?;
//     // println!("{x}");
//     // let code = Archon::compile(&x)?;
//     // println!("{a:?}");
//     // println!("{}", c.debug_dissassembly());
//     // for t in x.slice() {
//     // println!("{}", t);
//     // }
//
//     Ok(())
// }
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::OptimizationLevel;

use std::error::Error;

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn jit_compile_sum(&self) -> Option<JitFunction<SumFunc>> {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);

        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0)?.into_int_value();
        let y = function.get_nth_param(1)?.into_int_value();
        let z = function.get_nth_param(2)?.into_int_value();

        let sum = self.builder.build_int_add(x, y, "sum").unwrap();
        let sum = self.builder.build_int_add(sum, z, "sum").unwrap();

        self.builder.build_return(Some(&sum)).unwrap();

        unsafe { self.execution_engine.get_function("sum").ok() }
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
    };

    let sum = codegen
        .jit_compile_sum()
        .ok_or("Unable to JIT compile `sum`")?;

    let x = 1u64;
    let y = 2u64;
    let z = 3u64;

    unsafe {
        println!("{} + {} + {} = {}", x, y, z, sum.call(x, y, z));
        assert_eq!(sum.call(x, y, z), x + y + z);
    }

    Ok(())
}
