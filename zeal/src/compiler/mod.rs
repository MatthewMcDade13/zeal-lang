use std::rc::Rc;

use chunk::Chunk;
use opcode::{Op, Opcode};

use crate::{
    ast::{reduce::AstReducer, Ast, Expr},
    core_types::{
        num::{ZBool, ZFloat64},
        val::ZValue,
    },
};

pub mod chunk;
pub mod opcode;

/// Bytecode compiler for ZealVM bytecode
#[derive(Debug, Clone)]
pub struct Archon;

impl Archon {
    pub fn compile(ast: &Ast) -> anyhow::Result<Chunk> {
        let mut ch = Chunk::default();
        Self::compile_with(ast, &mut ch);
        Ok(ch)
        // let mut chunks = Vec::with_capacity(ast.slice().len());
        // for expr in ast.slice() {
        //     let mut ch = Chunk::default();
        //     Self::compile_expr(&mut ch, expr)?;
        //     chunks.push(ch);
        // }
        // Ok(chunks)
    }

    pub fn compile_with(ast: &Ast, ch: &mut Chunk) -> anyhow::Result<()> {
        for expr in ast.slice() {
            Self::compile_expr(ch, expr)?;
        }
        Ok(())
    }

    fn compile_expr(ch: &mut Chunk, expr: &Expr) -> anyhow::Result<()> {
        match expr {
            Expr::Call(cl) => {
                let fn_name = cl
                    .first()
                    .expect("Cannot call expression call list with no parameters!");
                if let Some(op) = fn_name.sys_op() {
                    // NOTE: For now mathematic operators have only 2 operands. In the future
                    // we can easily allow for a lisp style ops
                    let left = &cl[1];
                    let right = &cl[2];
                    Self::compile_expr(ch, left)?;
                    Self::compile_expr(ch, right)?;
                    ch.push_opcode(Opcode::new(op));
                } else {
                    todo!()
                }
            }
            Expr::Block(bl) => todo!(),
            Expr::List(l) => Self::compile_expr(ch, l.as_ref())?,
            Expr::Atom(a) => match a {
                ZValue::Nil => ch.push_opcode(Opcode::new(Op::Nil)),
                ZValue::Bool(ZBool(true)) => ch.push_opcode(Opcode::new(Op::True)),
                ZValue::Bool(ZBool(false)) => ch.push_opcode(Opcode::new(Op::False)),
                ZValue::Number(n) => ch.push_number(n.unwrap()),

                ZValue::Byte(_) => todo!(),
                ZValue::Buffer(_) => todo!(),
                ZValue::Str(_) => todo!(),
                ZValue::Vec(_) => todo!(),
                ZValue::Obj(_) => todo!(),
                ZValue::MutRef(_) => todo!(),
                ZValue::Rune(_) => todo!(),
                ZValue::Sym(_) => todo!(),
                ZValue::Unit => todo!(),
            },
        }
        Ok(())
    }
}
