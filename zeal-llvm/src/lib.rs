// This is the native Rust function we want to call.
#[no_mangle] // Prevents name mangling
pub extern "C" fn my_native_function(x: i32) -> i32 {
    x * 2
}

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, IntValue};

#[cfg(test)]
mod tests {
    use inkwell::OptimizationLevel;

    use super::*;

    #[test]
    fn llvm_call_native() -> anyhow::Result<()> {
        // Create a new LLVM context
        let context = Context::create();
        let module = context.create_module("my_module");
        let builder = context.create_builder();

        // Define the function signature for `my_native_function`
        let fn_type = context
            .i32_type()
            .fn_type(&[context.i32_type().into()], false);
        let native_fn = module.add_function("my_native_function", fn_type, None);

        // Create a new function in the module
        let function = module.add_function("call_native_function", fn_type, None);
        let entry = context.append_basic_block(function, "entry");
        builder.position_at_end(entry);

        // Create an argument for the native function
        let arg = function.get_nth_param(0).unwrap().into_int_value();

        // Call the native function
        let call = builder.build_call(native_fn, &[arg.into()], "call_result")?;

        let result = call.try_as_basic_value().left().unwrap();

        // Return the result
        builder.build_return(Some(&result))?;

        // Create the execution engine
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .expect("Failed to create execution engine");

        let func_ptr = unsafe {
            execution_engine
                .get_function::<unsafe extern "C" fn(i32) -> i32>("call_native_function")
                .expect("Failed to get function pointer")
        };
        // Get the function pointer for the function we want to call

        // Call the function through the execution engine
        unsafe {
            let result = func_ptr.call(10); // Call with an argument, e.g., 10
            println!("Result of calling native function: {}", result);
        }

        // Print the generated LLVM IR
        module.print_to_stderr();
        Ok(())
    }
}
