use std::{borrow::Borrow, cell::Cell, collections::HashMap, rc::Rc, sync::Arc};

use inkwell::{builder::Builder, context::Context, module::Module, values::FunctionValue};
use zeal_ast::{
    expr::{Expr, ExprStmt},
    AstWalker,
};

pub const MAIN_MODULE_NAME: &str = "<<MAIN>>";
pub const ENTRYPOINT_NAME: &str = "__ENTRY__";

#[derive(Debug)]
pub struct Compiler<'ctx> {
    ctx: &'ctx Context,
    modules: HashMap<String, Module<'ctx>>,
    scope_stack: Vec<FunctionValue<'ctx>>,
    builder: Builder<'ctx>,
}

impl<'a> Compiler<'a> {
    pub fn new(ctx: &'a Context) -> Self {
        let mut modules = HashMap::new();
        modules.insert(MAIN_MODULE_NAME.into(), ctx.create_module(MAIN_MODULE_NAME));

        let builder = ctx.create_builder();
        let mut scope_stack = Vec::with_capacity(128);

        // creates entrypoint function and preps builder position to
        // build instructions
        let entrypoint_fn = {
            let fn_type = ctx.void_type().fn_type(&[], false);
            let mm = &modules[MAIN_MODULE_NAME];
            let entrypoint = mm.add_function(ENTRYPOINT_NAME, fn_type, None);
            let entry = ctx.append_basic_block(entrypoint, "entry");
            builder.position_at_end(entry);
            entrypoint
        };

        scope_stack.push(entrypoint_fn);

        Self {
            ctx,
            modules,
            scope_stack,
            builder,
        }
    }

    /// Gets current scope of compiler.     ///
    pub fn this_scope(&self) -> &FunctionValue<'a> {
        // SEFETY ::  Scope Stack is never empty, (scope_stack[0] == ENTRYPOINT)
        // so this is always safe.
        unsafe { self.scope_stack.last().unwrap_unchecked() }
    }

    // pub const fn new_func(&self, name: &str, param_types: &[CompType], ret_type: CompType) -> FunctionValue
}

// impl AstWalker<ExprStmt> for Compiler<'_> {
//     fn visit(&mut self, node: &ExprStmt) -> anyhow::Result<()> {
//         match node {
//             ExprStmt::Block(ast_list) => {
//                 let scope = self.this_scope();
//                 let entry = self.ctx.append_basic_block(*scope, "entry");
//                 self.builder.position_at_end(entry);
//             }
//             ExprStmt::Loop(ast_list) => {
//
//             }
//             ExprStmt::While { cond, body } => todo!(),
//             ExprStmt::When(ast_list) => todo!(),
//             ExprStmt::DefFunc(func_decl) => todo!(),
//             ExprStmt::Binding(bind_stmt) => todo!(),
//             ExprStmt::Escape(escape_expr) => todo!(),
//             ExprStmt::Atom(expr) => todo!(),
//         }
//
//         Ok(())
//     }
// }

impl AstWalker<Expr> for Compiler<'_> {
    fn visit(&mut self, node: &Expr) -> anyhow::Result<()> {
        match node {
            Expr::Rune(rc) => todo!(),
            Expr::Bool(b) => todo!(),
            Expr::Byte(_) => todo!(),
            Expr::SByte(_) => todo!(),
            Expr::Int(_) => todo!(),
            Expr::Uint(_) => todo!(),
            Expr::Float(_) => todo!(),
            Expr::String(rc) => todo!(),
            Expr::List(ast_list) => todo!(),
            Expr::Operator { ty, args } => todo!(),
            Expr::Pair(rc) => todo!(),
            Expr::Triple(rc) => todo!(),
            Expr::Assign { lhs, rhs } => todo!(),
            Expr::Call { head, args } => todo!(),
            Expr::Unit => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompType {
    Unit,
    /// AKA UInt8
    Byte,
    Int8,
    UInt16,
    Int16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    UWord,
    Word,
    F32,
    F64,
    String,
    Ptr,
    Map,
    Vec,
    /// AKA Vec<Byte>,
    Buffer,
}
