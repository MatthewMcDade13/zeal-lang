use anyhow::bail;
use chunk::Chunk;
use opcode::{Op, Opcode, VarOp};

use crate::{
    ast::{Ast, Expr},
    core_types::{idents, num::ZBool, val::ZValue},
};

pub mod chunk;
pub mod opcode;
pub mod state;

/// Bytecode compiler for ZealVM bytecode
pub struct Archon;

impl Archon {
    pub fn compile(ast: &Ast) -> anyhow::Result<Chunk> {
        // let ast = todo!(); //ast.pipe();
        let mut ch = Chunk::default();
        Self::compile_with(ast, &mut ch)?;
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
        for expr in ast.tree.iter() {
            Self::compile_expr(ast, ch, &expr)?;
        }
        Ok(())
    }

    fn compile_expr(ast: &Ast, ch: &mut Chunk, expr: &Expr) -> anyhow::Result<()> {
        match expr {
            Expr::Call(cl) => {
                let head = cl
                    .first()
                    .expect("Cannot call expression call list with no parameters!");

                if let Some(op) = head.binary_operator() {
                    // NOTE: For now mathematic operators have only 2 operands. In the future
                    // we can easily allow for a lisp style ops
                    let left = &cl[1];
                    let right = &cl[2];
                    Self::compile_expr(ast, ch, left)?;
                    Self::compile_expr(ast, ch, right)?;
                    ch.push_opcode(Opcode::new(op));
                } else if let Some(op) = head.unary_operator() {
                    let operand = &cl[1];
                    Self::compile_expr(ast, ch, operand)?;
                    ch.push_opcode(Opcode::new(op));
                } else {
                    if let Expr::Atom(ZValue::Ident(ident)) = head {
                        match ident.name() {
                            idents::PRINT => {
                                let right = &cl[1];
                                Self::compile_expr(ast, ch, right)?;
                                ch.push_opcode(Opcode::new(Op::Print))
                            }
                            idents::PRINTLN => {
                                let right = &cl[1];
                                Self::compile_expr(ast, ch, right)?;
                                ch.push_opcode(Opcode::new(Op::Println))
                            }
                            _ => bail!("cant compile: {head}"),
                        }
                    }
                }
            }
            Expr::Effect(ef) => {
                let head = ef.first().expect("Cannot call empty Expr::Effect");
                if let Some(ident) = head.as_ident_varop() {
                    assert!(ef.len() >= 3);

                    let name = &ef[1];
                    let val = &ef[2];

                    // only compile value,
                    Self::compile_expr(ast, ch, val)?;
                    if let Some(n) = name.inner_ident() {
                        match ident.name() {
                            idents::LET | idents::VAR => {
                                ch.push_variable(n.clone(), VarOp::Define);
                            }
                            idents::ASSIGN => {
                                let _ = ch.push_variable(n.clone(), VarOp::Set);
                            }
                            _ => unreachable!(),
                        }
                    } else {
                        bail!(
                        "!!!NEED ERROR TYPE!!! => Cannot declare binding with non-identifier name."
                    );
                    }
                } else {
                    // TODO: Check if this a keyword, otherwise call list as a normal function...

                    if let Expr::Atom(ZValue::Ident(ident)) = head {
                        match ident.name() {
                            idents::PRINT => {
                                let right = &ef[1];
                                Self::compile_expr(ast, ch, right)?;
                                ch.push_opcode(Opcode::new(Op::Print))
                            }
                            idents::PRINTLN => {
                                let right = &ef[1];
                                Self::compile_expr(ast, ch, right)?;
                                ch.push_opcode(Opcode::new(Op::Println))
                            }
                            _ => bail!("cant compile: {head}"),
                        }
                    }
                }
                // .expect(
                // "Uknown effect keyword. Use 'let', 'var' or '=' for variable & assignment.",
                // );
            }
            Expr::Block(bl) => {
                ch.scope.start_scope();
                for e in bl.iter() {
                    Self::compile_expr(ast, ch, e)?;
                }
                ch.scope.end_scope();
            }
            // Expr::List(l) => Self::compile_expr(ch, l.as_ref())?,
            Expr::Atom(a) => match a {
                ZValue::Nil => ch.push_opcode(Opcode::new(Op::Nil)),
                ZValue::Bool(ZBool(true)) => ch.push_opcode(Opcode::new(Op::True)),
                ZValue::Bool(ZBool(false)) => ch.push_opcode(Opcode::new(Op::False)),
                ZValue::Number(n) => {
                    let _ = ch.push_number(n.unwrap());
                }

                ZValue::Byte(_) => todo!(),
                ZValue::Buffer(_) => todo!(),
                ZValue::Str(s) => {
                    let _ = ch.push_string(s.as_str());
                }
                ZValue::Vec(_) => todo!(),
                ZValue::Obj(_) => todo!(),
                ZValue::MutRef(_) => todo!(),
                ZValue::Rune(_) => todo!(),
                ZValue::Ident(ident) => {
                    let _ = ch.push_variable(ident.clone(), VarOp::Get);
                }
                ZValue::Unit => todo!(),
            },
        }
        Ok(())
    }
}
