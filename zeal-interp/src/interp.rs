use std::{collections::HashMap, rc::Rc};

use zeal_ast::AstModule;
use zvm::{
    native::{zvm_print, zvm_println},
    val::{NativeFunc, Val},
};

use crate::core::Env;

#[derive(Debug, Clone)]
pub struct Interp {
    env: Env,
    ast: AstModule,
}

impl Interp {
    pub fn new(ast: AstModule) -> Self {
        let env = Env::default();
        Self { env, ast }
    }
}
