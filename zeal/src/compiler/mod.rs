use anyhow::bail;
use chunk::Chunk;
use opcode::{Op, Opcode, VarOp};

use crate::{
    ast::{Ast, Expr, ExprList, VarType},
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
        println!("{}", ch.debug_dissassembly());

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
        Self::compile_expr(ast, ch, &ast.tree)?;
        // for expr in ast.tree.iter() {
        // Self::compile_expr_list(ast, ch, &expr)?;
        // }
        Ok(())
    }

    fn compile_expr_list(ast: &Ast, ch: &mut Chunk, el: &ExprList) -> anyhow::Result<()> {
        match el {
            ExprList::Block(bl) => {
                ch.scope.start_scope();
                for e in bl.iter() {
                    Self::compile_expr(ast, ch, e)?;
                }
                let pops = ch.scope.end_scope();
                ch.push_popn(pops as u8);
            }
            ExprList::Tuple(tup) => {
                for ex in tup.iter() {
                    Self::compile_expr(ast, ch, ex)?;
                }
            }
            // ExprList::Unit(ex) => Self::compile_expr(ast, ch, ex.as_ref())?,
            ExprList::Nil => Self::compile_expr(ast, ch, &Expr::Nil)?,
        };
        Ok(())
    }

    fn compile_expr(ast: &Ast, ch: &mut Chunk, expr: &Expr) -> anyhow::Result<()> {
        match expr {
            Expr::Call(cl) => {
                let head = cl.head.expect_ident();

                if let Some(op) = head.binary_operator() {
                    // NOTE: For now mathematic operators have only 2 operands. In the future
                    // we can easily allow for a lisp style ops
                    let left = &cl.params[0];
                    let right = &cl.params[1];
                    Self::compile_expr(ast, ch, left)?;
                    Self::compile_expr(ast, ch, right)?;
                    ch.push_opcode(Opcode::new(op));
                } else if let Some(op) = head.unary_operator() {
                    let operand = &cl.params[0];
                    Self::compile_expr(ast, ch, operand)?;
                    ch.push_opcode(Opcode::new(op));
                } else {
                    match head.name() {
                        idents::ASSIGN => {
                            let name = &cl.params[0];
                            let ident = name
                                .inner_ident()
                                .expect("first param of Assignment call must be an Identifier!!!");
                            let ty = ast.symbols.var_type(&ident);
                            if let Some(VarType::Var) = ty {
                                let val = &cl.params[1];
                                Self::compile_expr(ast, ch, val)?;
                                let _ = ch.push_binding(ident.clone(), VarOp::Set)?;
                            } else {
                                bail!("Cannot assign to a binding that was not declared as 'var'. ident: {}, var_type: {ty:?}", ident.name());
                            }
                        }
                        idents::PRINT => {
                            let right = &cl.params[0];
                            Self::compile_expr(ast, ch, right)?;
                            ch.push_opcode(Opcode::new(Op::Print));
                        }
                        idents::PRINTLN => {
                            let right = &cl.params[0];
                            Self::compile_expr(ast, ch, right)?;
                            ch.push_opcode(Opcode::new(Op::Println));
                        }
                        _ => bail!("cant compile: {head}"),
                    };
                }
            }
            Expr::Binding { ty, name, init } => {
                // let init = if let Some(i) = init {
                //     i
                // } else {
                //     &ExprList::Nil
                // };
                Self::compile_expr(ast, ch, init)?;
                let name = name.expect_ident();
                ch.declare_binding(name.clone())?;

                // if let Some(ident) = head.as_ident_varop() {
                //     assert!(ef.len() >= 3);
                //
                //     let name = &ef[1];
                //     let val = &ef[2];
                //
                //     // only compile value,
                //     Self::compile_expr(ast, ch, val)?;
                //     if let Some(n) = name.inner_ident() {
                //         match ident.name() {
                //             idents::LET | idents::VAR => {
                //                 ch.push_variable(n.clone(), VarOp::Define);
                //             }
                //             idents::ASSIGN => {
                //                 let _ = ch.push_variable(n.clone(), VarOp::Set);
                //             }
                //             _ => unreachable!(),
                //         }
                //     } else {
                //         bail!(
                //         "!!!NEED ERROR TYPE!!! => Cannot declare binding with non-identifier name."
                //     );
                //     }
                // } else {
                //     // TODO: Check if this a keyword, otherwise call list as a normal function...
                //
                //     if let Expr::Atom(ZValue::Ident(ident)) = head {
                //         match ident.name() {
                //             idents::PRINT => {
                //                 let right = &ef[1];
                //                 Self::compile_expr(ast, ch, right)?;
                //                 ch.push_opcode(Opcode::new(Op::Print))
                //             }
                //             idents::PRINTLN => {
                //                 let right = &ef[1];
                //                 Self::compile_expr(ast, ch, right)?;
                //                 ch.push_opcode(Opcode::new(Op::Println))
                //             }
                //             _ => bail!("cant compile: {head}"),
                //         }
                //     }
                // }
                // .expect(
                // "Uknown effect keyword. Use 'let', 'var' or '=' for variable & assignment.",
                // );
            }
            // Expr::List(l) => Self::compile_expr(ch, l.as_ref())?,
            Expr::List(li) => Self::compile_expr_list(ast, ch, li)?,
            Expr::Atom(a) => match a {
                ZValue::Nil => ch.push_opcode(Opcode::new(Op::Nil)),
                ZValue::Bool(ZBool(true)) => ch.push_opcode(Opcode::new(Op::True)),
                ZValue::Bool(ZBool(false)) => ch.push_opcode(Opcode::new(Op::False)),
                ZValue::Number(n) => {
                    let _ = ch.push_number(n.unwrap());
                }

                ZValue::Byte(_) => todo!(),
                // ZValue::Buffer(_) => todo!(),
                ZValue::Str(s) => {
                    let _ = ch.push_string(s.as_str());
                }
                ZValue::Vec(v) => {
                    let vec = v.as_ref();
                }
                // ZValue::Obj(_) => todo!(),
                // ZValue::MutRef(_) => todo!(),
                ZValue::Rune(_) => todo!(),
                ZValue::Ident(ident) => {
                    ch.push_binding(ident.clone(), VarOp::Get)?;
                }
                ZValue::Unit => todo!(),
            },

            Expr::Nil => todo!(),
        }
        Ok(())
    }
}
