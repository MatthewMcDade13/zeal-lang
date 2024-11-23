// This is the native Rust function we want to call.
#[no_mangle] // Prevents name mangling
pub extern "C" fn my_native_function(x: i32) -> i32 {
    x * 2
}

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{FunctionValue, IntValue};
